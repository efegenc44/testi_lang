use crate::span::Span;

#[derive(Debug)]
pub struct Error {
    pub msg: String,
    pub span: Span,
    pub origin: Box<Option<Error>>
}

impl Error {
    pub fn new<T>(msg: T, span: Span, origin: Option<Error>) -> Self
    where
        T: Into<String>
    {
        Self { msg: msg.into(), span, origin: Box::new(origin) }
    }
}

#[macro_export]
macro_rules! handle {
    ($res:expr, $span:expr) => {
        $res.map_err(|err| Error::new(err, $span, None))
    };
}

pub type Res<T> = Result<T, Error>;

pub fn simple_error<T>(msg: impl Into<String>, span: Span) -> Res<T> {
    Err(Error::new(msg, span, None))
}