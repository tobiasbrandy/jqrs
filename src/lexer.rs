use logos::{
    source::{self, Source},
    Lexer, Logos, Skip,
};
use std::io::{self, Read, Stdin};
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

pub trait LexState: Clone + Default {
    fn line_pos(&self) -> LinePos;
    fn line_pos_mut(&mut self) -> &mut LinePos;
}

pub trait LexToken<'a>: Logos<'a> + Clone + Default + PartialEq
where
    Self::Extras: LexState,
{
}

impl<'a, T: Logos<'a> + Clone + Default + PartialEq> LexToken<'a> for T where T::Extras: LexState {}

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

/// Possible references types to a source
#[derive(Debug, PartialEq, Eq)]
pub enum LexSourceRef<'a> {
    LazyReader(LazyReaderSourceRef<'a, str>),
    String(&'a str),
}
impl Deref for LexSourceRef<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            LexSourceRef::LazyReader(file) => file.deref(),
            LexSourceRef::String(string) => string,
        }
    }
}

// All sources types used in jqrs
pub enum LexSource {
    File(LazyReaderSource<File>),
    Stdin(LazyReaderSource<Stdin>),
    String(String),
}
impl LexSource {
    pub fn file(file: File) -> Self {
        LexSource::File(LazyReaderSource::file(file))
    }

    pub fn from_path<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        Ok(LexSource::File(LazyReaderSource::from_path(path)?))
    }

    pub fn stdin() -> Self {
        LexSource::Stdin(LazyReaderSource::stream(io::stdin()))
    }

    pub fn string(s: String) -> Self {
        LexSource::String(s)
    }
}
impl Source for LexSource {
    type Slice<'a> = LexSourceRef<'a>;

    fn len(&self) -> usize {
        match self {
            LexSource::File(source) => source.len(),
            LexSource::Stdin(source) => source.len(),
            LexSource::String(source) => source.len(),
        }
    }

    fn read<'a, Chunk: source::Chunk<'a>>(&'a self, offset: usize) -> Option<Chunk> {
        match self {
            LexSource::File(source) => source.read(offset),
            LexSource::Stdin(source) => source.read(offset),
            LexSource::String(source) => source.read(offset),
        }
    }

    fn read_byte(&self, offset: usize) -> u8 {
        match self {
            LexSource::File(source) => source.read_byte(offset),
            LexSource::Stdin(source) => source.read_byte(offset),
            LexSource::String(source) => source.read_byte(offset),
        }
    }

    fn slice(&self, range: Range<usize>) -> Option<Self::Slice<'_>> {
        Some(match self {
            LexSource::File(source) => LexSourceRef::LazyReader(source.slice(range)?),
            LexSource::Stdin(source) => LexSourceRef::LazyReader(source.slice(range)?),
            LexSource::String(source) => LexSourceRef::String(source.slice(range)?),
        })
    }

    fn is_boundary(&self, index: usize) -> bool {
        match self {
            LexSource::File(source) => source.is_boundary(index),
            LexSource::Stdin(source) => source.is_boundary(index),
            LexSource::String(source) => source.is_boundary(index),
        }
    }
}

// ---------------------- LazyReaderSource ---------------------- //

#[derive(Debug)]
pub struct LazyReaderSourceRef<'a, T: ?Sized>(Ref<'a, T>);
impl<T: ?Sized> Deref for LazyReaderSourceRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}
impl<T: ?Sized + PartialEq> PartialEq for LazyReaderSourceRef<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&*other.0)
    }
}
impl<T: ?Sized + Eq> Eq for LazyReaderSourceRef<'_, T> {}

struct LazyReaderSourceState<R: Read> {
    reader: R,
    buffer: Vec<u8>,
    start_offset: usize,
    consumed: usize,
    len: Option<usize>,
    eof: bool,
    error: bool,
}

pub struct LazyReaderSource<R: Read>(RefCell<LazyReaderSourceState<R>>);
impl LazyReaderSource<File> {
    pub fn from_path<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        Ok(Self::file(File::open(path)?))
    }

    pub fn from_path_with_capacity<P: AsRef<Path>>(path: P, capacity: usize) -> io::Result<Self> {
        Ok(Self::file_with_capacity(File::open(path)?, capacity))
    }

    pub fn file(file: File) -> Self {
        Self::file_with_capacity(file, LazyReaderSource::<File>::DEFAULT_BUF_SIZE)
    }

    pub fn file_with_capacity(file: File, capacity: usize) -> Self {
        let len = match file.metadata() {
            Ok(m) => Some(m.len() as usize),
            Err(err) => {
                eprintln!("LazyFileSource error: could not read file length: {err}");
                eprintln!("Falling back to unbounded source");
                None
            }
        };
        Self::new(file, len, capacity)
    }
}
impl<R: Read> LazyReaderSource<R> {
    const DEFAULT_BUF_SIZE: usize = 8 * 1024;

    /// Unbounded source
    pub fn stream(reader: R) -> Self {
        Self::stream_with_capacity(reader, LazyReaderSource::<R>::DEFAULT_BUF_SIZE)
    }

    pub fn stream_with_capacity(reader: R, capacity: usize) -> Self {
        Self::new(reader, None, capacity)
    }

    fn new(reader: R, len: Option<usize>, capacity: usize) -> Self {
        assert!(capacity > 0);
        LazyReaderSource(RefCell::new(LazyReaderSourceState {
            reader,
            buffer: Vec::with_capacity(capacity),
            start_offset: 0,
            consumed: 0,
            len,
            eof: false,
            error: false,
        }))
    }

    fn fixed_len(&self) -> Option<usize> {
        self.0.borrow().len
    }

    fn ensure(state: &mut LazyReaderSourceState<R>, range: Range<usize>) -> bool {
        fn inner<R: Read>(
            state: &mut LazyReaderSourceState<R>,
            Range { start, end }: Range<usize>,
        ) -> io::Result<bool> {
            // We can't backtrack
            assert!(start >= state.start_offset);

            let buf_len = state.buffer.len();

            if state.error || (state.eof && end - state.start_offset > buf_len) {
                return Ok(false);
            }

            let rel_end = end - state.start_offset;
            if rel_end <= buf_len {
                // We already have the data
                return Ok(true);
            }

            state.buffer.drain(..state.consumed - state.start_offset);
            let buf_len = state.buffer.len();
            state.start_offset = state.consumed;

            let missing = end - (state.start_offset + buf_len);
            let mut additional_capacity = state.buffer.capacity() - buf_len;
            if missing > additional_capacity {
                additional_capacity = 2 * state.buffer.capacity();

                if missing > additional_capacity {
                    additional_capacity = 2 * missing;
                }

                state.buffer.try_reserve(additional_capacity)?;
                additional_capacity = state.buffer.capacity() - buf_len;
            }

            let n = state
                .reader
                .by_ref()
                .take((additional_capacity) as u64)
                .read_to_end(&mut state.buffer)?;

            if n < additional_capacity {
                state.eof = true;
            }

            Ok(n >= missing)
        }

        match inner(state, range) {
            Ok(b) => b,
            Err(err) => {
                // Underlying reader failed
                // We log the error and close the source
                eprintln!("LazyFileSource error: {err}");

                state.buffer.clear();
                state.eof = true;
                state.error = true;

                false
            }
        }
    }

    pub fn slice_bytes(&self, range: Range<usize>) -> Option<LazyReaderSourceRef<'_, [u8]>> {
        let real_range = {
            let state = &mut *self.0.borrow_mut();

            if !LazyReaderSource::ensure(state, range.clone()) {
                return None;
            }

            let start_offset = state.start_offset;
            range.start - start_offset..range.end - start_offset
        };

        Some(LazyReaderSourceRef(Ref::map(self.0.borrow(), |state| {
            &state.buffer[real_range]
        })))
    }
}

impl<R: Read> Source for LazyReaderSource<R> {
    type Slice<'a>
        = LazyReaderSourceRef<'a, str>
    where
        R: 'a;

    fn len(&self) -> usize {
        let state = &*self.0.borrow();
        state
            .len
            .unwrap_or_else(|| state.start_offset + state.buffer.len())
    }

    fn is_boundary(&self, index: usize) -> bool {
        if let Some(len) = self.fixed_len() {
            return index <= len;
        }

        let state = &mut *self.0.borrow_mut();
        LazyReaderSource::ensure(state, index..index + 1)
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
        // Using LazyReaderSource forces us to avoid keeping any slice reference after the callback ends.
        // In consequence, this forces all token content to be owned.
        let slice = unsafe { std::mem::transmute::<&[u8], &[u8]>(&*slice_ref) };

        Chunk::from_slice(slice)
    }

    fn read_byte(&self, offset: usize) -> u8 {
        self.slice_bytes(offset..offset + 1).unwrap()[0]
    }

    fn slice(&self, range: Range<usize>) -> Option<Self::Slice<'_>> {
        // After a slice operation, data is never accessed again by Logos
        self.0.borrow_mut().consumed = range.start;

        Some(LazyReaderSourceRef(Ref::map(
            self.slice_bytes(range)?.0,
            |slice| std::str::from_utf8(slice).expect("lexer must match only valid utf-8 slices"),
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() -> io::Result<()> {
        #[derive(Logos, Debug, PartialEq)]
        #[logos(source = LexSource)]
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

        let source = LexSource::from_path("input.txt")?;
        let mut lexer = Token::lexer(&source);

        // Process tokens.
        while let Some(token) = lexer.next() {
            println!("{:?} (span: {:?})", token, lexer.span());
        }

        Ok(())
    }
}
