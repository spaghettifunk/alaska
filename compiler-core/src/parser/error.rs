use super::*;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedToken {
        found: TokenKind,
        expected: Vec<TokenKind>,
        line: u32,
        column: u32,
    },
    InvalidExpressionStatement {
        line: u32,
        column: u32,
    },
    MissingPackageStatement,
    OnlyOnePackageStatement,
}

impl From<&ParseError> for String {
    fn from(error: &ParseError) -> Self {
        match error {
            ParseError::UnexpectedToken {
                found,
                expected,
                line,
                column,
            } => {
                format!(
                    "Unexpected token at line {} and column {}: found '{}', but expected {}",
                    line,
                    column,
                    found,
                    token_list_to_string(expected)
                )
            }
            ParseError::InvalidExpressionStatement { line, column } => format!(
                "Expression is invalid as statement at line {} and column {}",
                line, column
            ),
            ParseError::MissingPackageStatement => "Missing package statement at top of the file".to_string(),
            ParseError::OnlyOnePackageStatement => "Only one package statement is allowed per file".to_string(),
        }
    }
}

impl From<ParseError> for String {
    fn from(error: ParseError) -> Self {
        String::from(&error)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", String::from(self))
    }
}

impl std::error::Error for ParseError {}

fn token_list_to_string(tokens: &[TokenKind]) -> String {
    let res: Vec<String> = tokens.iter().map(|token| format!("'{}'", token)).collect();
    let mut res = res.join(", ");
    if let Some(pos) = res.rfind(", ") {
        res.replace_range(pos..=pos + 1, " or ");
        res.replace_range(0..0, "one of ");
    }
    res
}
