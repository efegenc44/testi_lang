#[derive(Clone, Debug)]
pub struct Span {
    pub source_name: String,
    pub first_line: usize, 
    pub last_line : usize,
    pub start     : usize, 
    pub end       : usize, 
}

impl Span {
    pub fn new(source_name: String, first_line: usize, last_line: usize, start: usize, end: usize) -> Self {
        Self { source_name, first_line, last_line, start, end }
    }

    pub fn extend(&self, other: &Span) -> Span {
        assert!(&self.source_name == &other.source_name);

        Span::new(
            self.source_name.clone(),
            self.first_line, 
            other.last_line, 
            self.start, 
            other.end
        )
    } 
}

impl std::fmt::Display for Span 
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{line_no}:{start},{end}", 
            line_no = self.first_line, 
            start = self.start, 
            end = self.end)    
    }
}

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span
}

impl<T> From<(T, Span)> for Spanned<T> {
    fn from(value: (T, Span)) -> Self {
        Self { data: value.0, span: value.1 }
    }
}

impl<T> std::fmt::Display for Spanned<T> 
where
    T: std::fmt::Display
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{data}", // ({span})", 
            data = self.data,)
            // span = self.span) 
    }
}

impl<T> Spanned<T> {
    pub fn new(data: T, span: Span) -> Self {
        Self { data, span }
    }
}
