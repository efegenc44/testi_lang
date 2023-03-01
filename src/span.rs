#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub first_line: usize, // If last_line is 0, this is effectively the last line. 
    pub last_line : usize, // 0 if span is in a single-line.
    pub start     : usize, 
    pub end       : usize, 
}

impl Span {
    pub fn new(first_line: usize, last_line: usize, start: usize, end: usize) -> Self {
        Self { first_line, last_line, start, end }
    }

    pub fn last(&self) -> usize {
        //   If span is multi-line, it's line is the last, 
        // otherwise the line that it's at 
        if self.last_line == 0 {
            self.first_line
        } else {
            self.last_line
        }
    }

    pub fn extend(&self, other: Span) -> Span {
        //   Span are on the same line, if and only if 
        // the most left element's first line is equal to
        // the most right element's last line
        let multiline = if self.first_line == other.last() {
            0
        } else {
            other.last()
        };

        Span::new(
            self.first_line, 
            multiline, 
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
