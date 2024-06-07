use std::{
    fmt,
    ops::{Index, Range},
};

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum TokenKind {
    // Literals
    Int,
    String,
    Char,
    Bool,
    Float,
    Identifier,
    Type,
    Comment,
    Semicolon,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    True,
    False,
    // Operators
    LAngle,
    RAngle,
    Arrow,
    Fatarrow,
    Eq,
    Assign,
    NotEq,
    GtEq,
    LtEq,
    Colon,
    Pipe,
    Or,
    Ampersand,
    And,
    Plus,
    Minus,
    Mul,
    Slash,
    ShiftRight,
    ShiftLeft,
    Decrement,
    Increment,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    ShlAssign,
    ShrAssign,
    Backslash,
    Caret,
    Percent,
    Bang,
    Dot,
    Comma,
    Underscore,
    // Keywords
    KeywordFn,
    KeywordLet,
    KeywordIf,
    KeywordElse,
    KeywordMatch,
    KeywordEnum,
    KeywordStruct,
    KeywordInterface,
    KeywordImpl,
    KeywordConst,
    KeywordReturn,
    KeywordDefer,
    KeywordUse,
    KeywordSpawn,
    KeywordMut,
    KeywordFor,
    KeywordIn,
    KeywordWhile,
    KeywordBreak,
    KeywordContinue,
    KeywordNil,
    KeywordPackage,
    Error,
    Whitespace,
    Eof,
}

#[macro_export]
macro_rules! T {
    [+] => {
        $crate::lexer::TokenKind::Plus
    };
    [-] => {
        $crate::lexer::TokenKind::Minus
    };
    [*] => {
        $crate::lexer::TokenKind::Mul
    };
    [/] => {
        $crate::lexer::TokenKind::Slash
    };
    [%] => {
        $crate::lexer::TokenKind::Percent
    };
    [^] => {
        $crate::lexer::TokenKind::Caret
    };
    [=] => {
        $crate::lexer::TokenKind::Assign
    };
    [.] => {
        $crate::lexer::TokenKind::Dot
    };
    [,] => {
        $crate::lexer::TokenKind::Comma
    };
    [_] => {
        $crate::lexer::TokenKind::Underscore
    };
    ['\\'] => {
        $crate::lexer::TokenKind::Backslash
    };
    [type] => {
        $crate::lexer::TokenKind::Type
    };
    [!] => {
        $crate::lexer::TokenKind::Bang
    };
    [&] => {
        $crate::lexer::TokenKind::Ampersand
    };
    [|] => {
        $crate::lexer::TokenKind::Pipe
    };
    [:] => {
        $crate::lexer::TokenKind::Colon
    };
    [;] => {
        $crate::lexer::TokenKind::Semicolon
    };
    [<] => {
        $crate::lexer::TokenKind::LAngle
    };
    [>] => {
        $crate::lexer::TokenKind::RAngle
    };
    [->] => {
        $crate::lexer::TokenKind::Arrow
    };
    [=>] => {
        $crate::lexer::TokenKind::Fatarrow
    };
    ['['] => {
        $crate::lexer::TokenKind::LBracket
    };
    [']'] => {
        $crate::lexer::TokenKind::RBracket
    };
    ['{'] => {
        $crate::lexer::TokenKind::LBrace
    };
    ['}'] => {
        $crate::lexer::TokenKind::RBrace
    };
    ['('] => {
        $crate::lexer::TokenKind::LParen
    };
    [')'] => {
        $crate::lexer::TokenKind::RParen
    };
    [true] => {
        $crate::lexer::TokenKind::True
    };
    [false] => {
        $crate::lexer::TokenKind::False
    };
    [string] => {
        $crate::lexer::TokenKind::String
    };
    [comment] => {
        $crate::lexer::TokenKind::Comment
    };
    [int] => {
        $crate::lexer::TokenKind::Int
    };
    [float] => {
        $crate::lexer::TokenKind::Float
    };
    [char] => {
        $crate::lexer::TokenKind::Char
    };
    [bool] => {
        $crate::lexer::TokenKind::Bool
    };
    [string] => {
        $crate::lexer::TokenKind::String
    };
    [ident] => {
        $crate::lexer::TokenKind::Identifier
    };
    [let] => {
        $crate::lexer::TokenKind::KeywordLet
    };
    [fn] => {
        $crate::lexer::TokenKind::KeywordFn
    };
    [struct] => {
        $crate::lexer::TokenKind::KeywordStruct
    };
    [if] => {
        $crate::lexer::TokenKind::KeywordIf
    };
    [else] => {
        $crate::lexer::TokenKind::KeywordElse
    };
    [use] => {
        $crate::lexer::TokenKind::KeywordUse
    };
    [package] => {
        $crate::lexer::TokenKind::KeywordPackage
    };
    [enum] => {
        $crate::lexer::TokenKind::KeywordEnum
    };
    [interface] => {
        $crate::lexer::TokenKind::KeywordInterface
    };
    [match] => {
        $crate::lexer::TokenKind::KeywordMatch
    };
    [impl] => {
        $crate::lexer::TokenKind::KeywordImpl
    };
    [const] => {
        $crate::lexer::TokenKind::KeywordConst
    };
    [return] => {
        $crate::lexer::TokenKind::KeywordReturn
    };
    [defer] => {
        $crate::lexer::TokenKind::KeywordDefer
    };
    [spawn] => {
        $crate::lexer::TokenKind::KeywordSpawn
    };
    [mut] => {
        $crate::lexer::TokenKind::KeywordMut
    };
    [for] => {
        $crate::lexer::TokenKind::KeywordFor
    };
    [in] => {
        $crate::lexer::TokenKind::KeywordIn
    };
    [while] => {
        $crate::lexer::TokenKind::KeywordWhile
    };
    [break] => {
        $crate::lexer::TokenKind::KeywordBreak
    };
    [continue] => {
        $crate::lexer::TokenKind::KeywordContinue
    };
    [nil] => {
        $crate::lexer::TokenKind::KeywordNil
    };
    [++] => {
        $crate::lexer::TokenKind::Increment
    };
    [--] => {
        $crate::lexer::TokenKind::Decrement
    };
    [&&] => {
        $crate::lexer::TokenKind::And
    };
    [||] => {
        $crate::lexer::TokenKind::Or
    };
    [==] => {
        $crate::lexer::TokenKind::Eq
    };
    [!=] => {
        $crate::lexer::TokenKind::NotEq
    };
    [>=] => {
        $crate::lexer::TokenKind::GtEq
    };
    [<=] => {
        $crate::lexer::TokenKind::LtEq
    };
    [+=] => {
        $crate::lexer::TokenKind::AddAssign
    };
    [-=] => {
        $crate::lexer::TokenKind::SubAssign
    };
    [*=] => {
        $crate::lexer::TokenKind::MulAssign
    };
    [/=] => {
        $crate::lexer::TokenKind::DivAssign
    };
    [&=] => {
        $crate::lexer::TokenKind::AndAssign
    };
    [|=] => {
        $crate::lexer::TokenKind::OrAssign
    };
    [^=] => {
        $crate::lexer::TokenKind::XorAssign
    };
    [%=] => {
        $crate::lexer::TokenKind::ModAssign
    };
    [<<] => {
        $crate::lexer::TokenKind::ShiftLeft
    };
    [>>] => {
        $crate::lexer::TokenKind::ShiftRight
    };
    [<<=] => {
        $crate::lexer::TokenKind::ShlAssign
    };
    [>>=] => {
        $crate::lexer::TokenKind::ShrAssign
    };
    [error] => {
        $crate::lexer::TokenKind::Error
    };
    [ws] => {
        $crate::lexer::TokenKind::Whitespace
    };
    [EOF] => {
        $crate::lexer::TokenKind::Eof
    };
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                // Single characters
                T![+] => "+",
                T![-] => "-",
                T![*] => "*",
                T![/] => "/",
                T![^] => "^",
                T![=] => "=",
                T![.] => ".",
                T![,] => ",",
                T![_] => "_",
                T![!] => "!",
                T![&] => "&",
                T![|] => "|",
                T![:] => ":",
                T![;] => ";",
                T![%] => "%",
                // Brackets
                T![<] => "<",
                T![>] => ">",
                T!['['] => "[",
                T![']'] => "]",
                T!['{'] => "{",
                T!['}'] => "}",
                T!['('] => "(",
                T![')'] => ")",
                T!['\\'] => "\\",
                // Multiple characters
                T![true] => "true",
                T![false] => "false",
                T![string] => "String",
                T![comment] => "// Comment",
                T![int] => "Int",
                T![float] => "Float",
                T![ident] => "Identifier",
                T![bool] => "Bool",
                T![char] => "Char",
                T![let] => "let",
                T![fn] => "fn",
                T![struct] => "struct",
                T![if] => "if",
                T![else] => "else",
                T![use] => "use",
                T![package] => "package",
                T![enum] => "enum",
                T![interface] => "interface",
                T![match] => "match",
                T![impl] => "impl",
                T![const] => "const",
                T![return] => "return",
                T![defer] => "defer",
                T![spawn] => "spawn",
                T![mut] => "mut",
                T![for] => "for",
                T![in] => "in",
                T![while] => "while",
                T![break] => "break",
                T![continue] => "continue",
                T![nil] => "nil",
                T![type] => "Type",
                T![=>] => "=>",
                T![->] => "->",
                // Operators
                T![++] => "++",
                T![--] => "--",
                T![&&] => "&&",
                T![||] => "||",
                T![==] => "==",
                T![!=] => "!=",
                T![>=] => ">=",
                T![<=] => "<=",
                T![+=] => "+=",
                T![-=] => "-=",
                T![*=] => "*=",
                T![/=] => "/=",
                T![%=] => "%=",
                T![&=] => "&=",
                T![|=] => "|=",
                T![^=] => "^=",
                T![<<] => "<<",
                T![>>] => ">>",
                T![<<=] => "<<=",
                T![>>=] => ">>=",
                // Misc
                T![error] => "<?>",
                T![ws] => "<WS>",
                T![EOF] => "<EOF>",
            }
        )
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Hash, Default, Debug)]
pub struct Span {
    /// inclusive
    pub start: u32,
    /// exclusive
    pub end: u32,
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start as usize..span.end as usize
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start as u32,
            end: range.end as u32,
        }
    }
}

impl Index<Span> for str {
    type Output = str;
    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn len(&self) -> usize {
        (self.span.end - self.span.start) as usize
    }

    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span]
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} - <{}, {}>", self.kind, self.span.start, self.span.end)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn token_kind_display() {
        assert_eq!(T![+].to_string(), "+");
        assert_eq!(T![<=].to_string(), "<=");
        assert_eq!(T![let].to_string(), "let");
        assert_eq!(T![error].to_string(), "<?>");
        assert_eq!(T![comment].to_string(), "// Comment");
    }
}
