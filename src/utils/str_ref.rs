use std::sync::Arc;
use std::ops::Deref;
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StrRef {
    Literal(&'static str),
    Owned(String),
    Shared(Arc<str>),
}

impl StrRef {
    pub fn shareable(&mut self) -> &mut Self {
        if let StrRef::Owned(s) = self {
            *self = StrRef::Shared(Arc::from(std::mem::take(s)));
        }

        self
    }
}

impl From<&'static str> for StrRef {
    fn from(s: &'static str) -> Self {
        StrRef::Literal(s)
    }
}

impl From<String> for StrRef {
    fn from(s: String) -> Self {
        StrRef::Owned(s)
    }
}

impl From<Arc<str>> for StrRef {
    fn from(s: Arc<str>) -> Self {
        StrRef::Shared(s)
    }
}

impl Deref for StrRef {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        match self {
            StrRef::Literal(s) => s,
            StrRef::Owned(s) => s,
            StrRef::Shared(s) => s.as_ref(),
        }
    }
}

impl AsRef<str> for StrRef {
    fn as_ref(&self) -> &str {
        self.deref()
    }
}

impl std::fmt::Display for StrRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self, f)
    }
}
