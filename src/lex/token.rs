use std::fmt::Display;

#[derive(Clone, PartialEq)]
pub enum Token {
    SYMBOL(String),
    STRING(String),
    NATURAL(usize),
    FLOAT(f32),
    PLUS,
    MINUS,
    STAR,
    SLASH,
    BACKSLASH,
    LPAREN,
    RPAREN,
    LSQUARE,
    RSQUARE,
    SEMICOLON,
    COLON,
    HASH,
    DOT,
    TWODOT,
    TWODOTEQUAL,
    COMMA,
    BANG,
    EQUAL,
    PLUSEQUAL,
    MINUSEQUAL,
    STAREQUAL,
    SLASHEQUAL,
    DEQUAL,
    LESS,
    LESSEQUAL,
    GREATER,
    GREATEREQUAL,
    BANGEQUAL,
    KMOD,
    KAND,
    KOR,
    KIS,
    KLET,
    KFUN,
    KDEF,
    KFOR,
    KIN,
    KWHILE,
    KIF,
    KEF,
    KTHEN,
    KELSE,
    KRETURN,
    KCONTINUE,
    KBREAK,
    KEND,
    KTRUE,
    KFALSE,
    KNOTHING,
    END,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::SYMBOL(sym)  => write!(f, "{sym}"),
            Token::STRING(s)    => write!(f, "\"{s}\""),
            Token::NATURAL(nat) => write!(f, "{nat}"),
            Token::FLOAT(float) => write!(f, "{float}"),
            Token::PLUS         => write!(f, "+"),
            Token::MINUS        => write!(f, "-"),
            Token::STAR         => write!(f, "*"),
            Token::SLASH        => write!(f, "/"),
            Token::BACKSLASH    => write!(f, "\\"),
            Token::LPAREN       => write!(f, "("),
            Token::RPAREN       => write!(f, ")"),
            Token::LSQUARE      => write!(f, "["),
            Token::RSQUARE      => write!(f, "]"),
            Token::SEMICOLON    => write!(f, ";"),
            Token::COLON        => write!(f, ":"),
            Token::HASH         => write!(f, "#"),
            Token::DOT          => write!(f, "."),
            Token::TWODOT       => write!(f, ".."),
            Token::TWODOTEQUAL  => write!(f, "..="),
            Token::COMMA        => write!(f, ","),
            Token::EQUAL        => write!(f, "="),
            Token::PLUSEQUAL    => write!(f, "+="),
            Token::MINUSEQUAL   => write!(f, "-="),
            Token::STAREQUAL    => write!(f, "*="),
            Token::SLASHEQUAL   => write!(f, "/="),
            Token::BANG         => write!(f, "!"),
            Token::DEQUAL       => write!(f, "=="),
            Token::LESS         => write!(f, "<"),
            Token::LESSEQUAL    => write!(f, "<="),
            Token::GREATER      => write!(f, ">"),
            Token::GREATEREQUAL => write!(f, ">="),
            Token::BANGEQUAL    => write!(f, "!="),
            Token::KMOD         => write!(f, "mod"),
            Token::KAND         => write!(f, "and"),
            Token::KOR          => write!(f, "or"),
            Token::KIS          => write!(f, "is"),
            Token::KLET         => write!(f, "let"),
            Token::KFUN         => write!(f, "fun"),
            Token::KDEF         => write!(f, "def"),
            Token::KFOR         => write!(f, "for"),
            Token::KIN          => write!(f, "in"),
            Token::KWHILE       => write!(f, "while"),
            Token::KIF          => write!(f, "if"),
            Token::KEF          => write!(f, "ef"),
            Token::KTHEN        => write!(f, "then"),
            Token::KELSE        => write!(f, "else"),
            Token::KRETURN      => write!(f, "return"),
            Token::KCONTINUE    => write!(f, "continue"),
            Token::KBREAK       => write!(f, "break"),
            Token::KEND         => write!(f, "end"),
            Token::KTRUE        => write!(f, "true"),
            Token::KFALSE       => write!(f, "false"),
            Token::KNOTHING     => write!(f, "nothing"),
            Token::END          => write!(f, "⟨END⟩"),
        }
    }
}