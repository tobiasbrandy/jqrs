use logos::{
    source::{self, Source},
    Lexer, Logos, Skip,
};
use std::io::{self, Read};
use std::{cell::Ref, fs::File, ops::Deref, path::Path};
use std::{cell::RefCell, ops::Range};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LinePos {
    pub line: usize,
    pub line_start: usize,
}

impl Default for LinePos {
    fn default() -> Self {
        Self {
            line: 1,
            line_start: 0,
        }
    }
}

pub trait LexState: Default {
    fn line_pos(&self) -> LinePos;
    fn line_pos_mut(&mut self) -> &mut LinePos;
}

pub trait LexToken<'a>: Logos<'a> + Default
where
    Self::Extras: LexState,
{
}

impl<'a, T: Logos<'a> + Default> LexToken<'a> for T where T::Extras: LexState {}

pub fn register_newline<'a, Token>(lex: &mut Lexer<'a, Token>) -> Skip
where
    Token: LexToken<'a>,
    Token::Extras: LexState,
{
    let span = lex.span();
    let line_pos = lex.extras.line_pos_mut();
    line_pos.line += 1;
    line_pos.line_start = span.end;
    Skip
}

#[derive(Debug)]
pub struct LazyFileSourceRef<'a, T: ?Sized>(Ref<'a, T>);
impl<T: ?Sized> Deref for LazyFileSourceRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}
impl<T: ?Sized + PartialEq> PartialEq for LazyFileSourceRef<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&*other.0)
    }
}
impl<T: ?Sized + Eq> Eq for LazyFileSourceRef<'_, T> {}

struct LazyFileSourceState {
    file: File,
    buffer: Vec<u8>,
    start_offset: usize,
    consumed: usize,
    len: usize,
    eof: bool,
    error: bool,
}

pub struct LazyFileSource(RefCell<LazyFileSourceState>);

impl LazyFileSource {
    const DEFAULT_BUF_SIZE: usize = 8 * 1024;

    pub fn new(file: File) -> io::Result<Self> {
        Self::with_capacity(file, LazyFileSource::DEFAULT_BUF_SIZE)
    }

    pub fn from_path<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        Self::new(File::open(path)?)
    }

    pub fn from_path_with_capacity<P: AsRef<Path>>(path: P, capacity: usize) -> io::Result<Self> {
        Self::with_capacity(File::open(path)?, capacity)
    }

    pub fn with_capacity(file: File, capacity: usize) -> io::Result<Self> {
        assert!(capacity > 0);

        let len = file.metadata()?.len() as usize;

        Ok(LazyFileSource(RefCell::new(LazyFileSourceState {
            file,
            buffer: Vec::with_capacity(capacity),
            start_offset: 0,
            consumed: 0,
            len,
            eof: false,
            error: false,
        })))
    }

    fn ensure(state: &mut LazyFileSourceState, range: Range<usize>) {
        fn inner(
            state: &mut LazyFileSourceState,
            Range { start, end }: Range<usize>,
        ) -> io::Result<()> {
            assert!(start >= state.start_offset);
            assert!(end <= state.len);

            if state.eof || state.error {
                return Ok(());
            }

            let rel_end = end - state.start_offset;
            if rel_end <= state.buffer.len() {
                // We already have the data
                return Ok(());
            }

            state.buffer.drain(..state.consumed - state.start_offset);
            state.start_offset = state.consumed;

            let buf_len = state.buffer.len();
            let mut additional_capacity = state.buffer.capacity() - buf_len;
            if end - start > additional_capacity {
                additional_capacity = 2 * (end - start);
            }

            state.buffer.reserve(additional_capacity);
            additional_capacity = state.buffer.capacity() - buf_len;

            let n = state
                .file
                .by_ref()
                .take((additional_capacity) as u64)
                .read_to_end(&mut state.buffer)?;

            state.eof = n == 0;

            Ok(())
        }

        if let Err(err) = inner(state, range) {
            // Underlying file failed
            // We log the error and close the source
            eprintln!("LazyFileSource file error: {err}");
            state.buffer.clear();
            state.eof = true;
            state.error = true;
        }
    }

    pub fn slice_bytes(&self, range: Range<usize>) -> Option<LazyFileSourceRef<'_, [u8]>> {
        let real_range = {
            let state = &mut *self.0.borrow_mut();

            if state.error || range.end > state.len || (state.eof && range.end - state.start_offset > state.buffer.len()) {
                return None;
            }

            LazyFileSource::ensure(state, range.clone());

            let start_offset = state.start_offset;
            range.start - start_offset..range.end - start_offset
        };

        Some(LazyFileSourceRef(Ref::map(self.0.borrow(), |state| {
            &state.buffer[real_range]
        })))
    }
}

impl Source for LazyFileSource {
    type Slice<'a> = LazyFileSourceRef<'a, str>;

    fn len(&self) -> usize {
        self.0.borrow().len
    }

    fn read<'a, Chunk: source::Chunk<'a>>(&'a self, offset: usize) -> Option<Chunk> {
        let slice_ref = self.slice_bytes(offset..Chunk::SIZE + offset)?;

        // SAFETY: This operation is incredibly unsafe!!!
        //
        // We are using transmute to extend the lifetime of the slice to the lifetime of Self.
        // If we didn't mutate the buffer from this point on, this would be safe.
        // However, we are mutating the buffer, so we need to be careful.
        //
        // Logos wants the slice to live as long as the source, so the user can avoid copying
        // the slice when creating tokens during callbacks.
        // Using LazyFileSource forces us to avoid keeping any slice reference after the callback ends.
        // In consequence, this forces all token content to be owned.
        // This is the case for jqrs, so this shouldn't be a problem for us.
        let slice = unsafe { std::mem::transmute::<&[u8], &[u8]>(&*slice_ref) };

        Chunk::from_slice(slice)
    }

    fn read_byte(&self, offset: usize) -> u8 {
        self.slice_bytes(offset..offset + 1).unwrap()[0]
    }

    fn slice(&self, range: Range<usize>) -> Option<Self::Slice<'_>> {
        // Hypothesis: After a slice operation, data is never accessed again by Logos
        self.0.borrow_mut().consumed = range.start;

        Some(LazyFileSourceRef(Ref::map(self.slice_bytes(range)?.0, |slice| {
            std::str::from_utf8(slice).expect("lexer must match only valid utf-8 slices")
        })))
    }

    fn is_boundary(&self, index: usize) -> bool {
        index <= self.len()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LexSourceRef<'a> {
    File(LazyFileSourceRef<'a, str>),
    String(&'a str),
}
impl Deref for LexSourceRef<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            LexSourceRef::File(file) => file.deref(),
            LexSourceRef::String(string) => string,
        }
    }
}

pub enum LexSource {
    File(LazyFileSource),
    String(String),
}

impl Source for LexSource {
    type Slice<'a> = LexSourceRef<'a>;

    fn len(&self) -> usize {
        match self {
            LexSource::File(source) => source.len(),
            LexSource::String(source) => source.len(),
        }
    }

    fn read<'a, Chunk: source::Chunk<'a>>(&'a self, offset: usize) -> Option<Chunk> {
        match self {
            LexSource::File(source) => source.read(offset),
            LexSource::String(source) => source.read(offset),
        }
    }

    fn read_byte(&self, offset: usize) -> u8 {
        match self {
            LexSource::File(source) => source.read_byte(offset),
            LexSource::String(source) => source.read_byte(offset),
        }
    }

    fn slice(&self, range: Range<usize>) -> Option<Self::Slice<'_>> {
        Some(match self {
            LexSource::File(source) => LexSourceRef::File(source.slice(range)?),
            LexSource::String(source) => LexSourceRef::String(source.slice(range)?),
        })
    }

    fn is_boundary(&self, index: usize) -> bool {
        match self {
            LexSource::File(source) => source.is_boundary(index),
            LexSource::String(source) => source.is_boundary(index),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() -> io::Result<()> {
        #[derive(Logos, Debug, PartialEq)]
        #[logos(source = LazyFileSource)]
        enum Token {
            #[regex(r"[a-zA-Z]+")]
            Ident,
            #[regex(r"[0-9]+")]
            Number,
            #[token("(")]
            OpenParen,
            #[token(")")]
            CloseParen,
        }

        let source = LazyFileSource::new(File::open("input.txt")?)?;
        let mut lexer = Token::lexer(&source);

        // Process tokens.
        while let Some(token) = lexer.next() {
            println!("{:?} (span: {:?})", token, lexer.span());
        }

        Ok(())
    }
}
