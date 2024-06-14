use lazy_static::lazy_static;
use regex::Regex;

use crate::T;

use super::TokenKind;

/// If the given character is a character that _only_
/// represents a token of length 1,
/// this method returns the corresponding `TokenKind`.
/// Note that this method will return `None` for characters
/// like `=` that may also occur at the first position
/// of longer tokens (here `==`).
pub(crate) const fn unambiguous_single_char(c: char) -> Option<TokenKind> {
    Some(match c {
        '.' => T![.],
        ',' => T![,],
        '[' => T!['['],
        ']' => T![']'],
        '{' => T!['{'],
        '}' => T!['}'],
        '(' => T!['('],
        ')' => T![')'],
        ':' => T![:],
        ';' => T![;],
        '_' => T![_],
        _ => return None,
    })
}

pub(crate) struct Rule {
    pub kind: TokenKind,
    pub matches: fn(&str) -> Option<u32>,
}

fn match_single_char(input: &str, c: char) -> Option<u32> {
    input.chars().next().and_then(|ch| if ch == c { Some(1) } else { None })
}

fn match_two_chars(input: &str, first: char, second: char) -> Option<u32> {
    if input.len() >= 2 {
        match_single_char(input, first).and_then(|_| match_single_char(&input[1..], second).map(|_| 2))
    } else {
        None
    }
}

// This function is used to match three characters to handle the special case of <<= and >>=
fn match_three_chars(input: &str, first: char, second: char, third: char) -> Option<u32> {
    if input.len() >= 3 {
        match_single_char(input, first)
            .and_then(|_| match_single_char(&input[1..], second))
            .and_then(|_| match_single_char(&input[2..], third).map(|_| 3))
    } else {
        None
    }
}

fn match_regex(input: &str, r: &Regex) -> Option<u32> {
    r.find(input).map(|regex_match| regex_match.end() as u32)
}

fn match_keyword(input: &str, keyword: &str) -> Option<u32> {
    input.starts_with(keyword).then(|| keyword.len() as u32)
}

lazy_static! {
    static ref STRING_REGEX: Regex = Regex::new(r#"^"((\\"|\\\\)|[^\\"])*""#).unwrap();
    static ref COMMENT_REGEX: Regex = Regex::new(r#"^//[^\n]*\n"#).unwrap();
    static ref MULTILINE_COMMENT_REGEX: Regex = Regex::new(r#"^/\*([^*]|\*[^/])*\*/"#).unwrap();
    static ref FLOAT_REGEX: Regex = Regex::new(r#"^((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?"#).unwrap();
    static ref IDENTIFIER_REGEX: Regex = Regex::new(r##"^([A-Za-z]|_)([A-Za-z]|_|\d)*"##).unwrap();
    static ref CHAR_REGEX: Regex = Regex::new(r#"^'[a-zA-Z0-9!-\\/:-@[-`{-~]]'"#).unwrap(); // single char using '' and ASCII range
}

pub(crate) fn get_rules() -> Vec<Rule> {
    vec![
        Rule {
            kind: T![+],
            matches: |input| match_single_char(input, '+'),
        },
        Rule {
            kind: T![-],
            matches: |input| match_single_char(input, '-'),
        },
        Rule {
            kind: T![*],
            matches: |input| match_single_char(input, '*'),
        },
        Rule {
            kind: T![^],
            matches: |input| match_single_char(input, '^'),
        },
        Rule {
            kind: T![!],
            matches: |input| match_single_char(input, '!'),
        },
        Rule {
            kind: T![=],
            matches: |input| match_single_char(input, '='),
        },
        Rule {
            kind: T![/],
            matches: |input| match_single_char(input, '/'),
        },
        Rule {
            kind: T![_],
            matches: |input| match_single_char(input, '_'),
        },
        Rule {
            kind: T![<],
            matches: |input| match_single_char(input, '<'),
        },
        Rule {
            kind: T![>],
            matches: |input| match_single_char(input, '>'),
        },
        Rule {
            kind: T![%],
            matches: |input| match_single_char(input, '%'),
        },
        Rule {
            kind: T![|],
            matches: |input| match_single_char(input, '|'),
        },
        Rule {
            kind: T![&],
            matches: |input| match_single_char(input, '&'),
        },
        Rule {
            kind: T![->],
            matches: |input| match_two_chars(input, '-', '>'),
        },
        Rule {
            kind: T![==],
            matches: |input| match_two_chars(input, '=', '='),
        },
        Rule {
            kind: T![!=],
            matches: |input| match_two_chars(input, '!', '='),
        },
        Rule {
            kind: T![&&],
            matches: |input| match_two_chars(input, '&', '&'),
        },
        Rule {
            kind: T![||],
            matches: |input| match_two_chars(input, '|', '|'),
        },
        Rule {
            kind: T![<=],
            matches: |input| match_two_chars(input, '<', '='),
        },
        Rule {
            kind: T![>=],
            matches: |input| match_two_chars(input, '>', '='),
        },
        Rule {
            kind: T![+=],
            matches: |input| match_two_chars(input, '+', '='),
        },
        Rule {
            kind: T![-=],
            matches: |input| match_two_chars(input, '-', '='),
        },
        Rule {
            kind: T![*=],
            matches: |input| match_two_chars(input, '*', '='),
        },
        Rule {
            kind: T![/=],
            matches: |input| match_two_chars(input, '/', '='),
        },
        Rule {
            kind: T![%=],
            matches: |input| match_two_chars(input, '%', '='),
        },
        Rule {
            kind: T![^=],
            matches: |input| match_two_chars(input, '^', '='),
        },
        Rule {
            kind: T![|=],
            matches: |input| match_two_chars(input, '|', '='),
        },
        Rule {
            kind: T![&=],
            matches: |input| match_two_chars(input, '&', '='),
        },
        Rule {
            kind: T![<<],
            matches: |input| match_two_chars(input, '<', '<'),
        },
        Rule {
            kind: T![>>],
            matches: |input| match_two_chars(input, '>', '>'),
        },
        Rule {
            kind: T![let],
            matches: |input| match_keyword(input, "let"),
        },
        Rule {
            kind: T![fn],
            matches: |input| match_keyword(input, "fn"),
        },
        Rule {
            kind: T![struct],
            matches: |input| match_keyword(input, "struct"),
        },
        Rule {
            kind: T![if],
            matches: |input| match_keyword(input, "if"),
        },
        Rule {
            kind: T![else],
            matches: |input| match_keyword(input, "else"),
        },
        Rule {
            kind: T![use],
            matches: |input| match_keyword(input, "use"),
        },
        Rule {
            kind: T![package],
            matches: |input| match_keyword(input, "package"),
        },
        Rule {
            kind: T![enum],
            matches: |input| match_keyword(input, "enum"),
        },
        Rule {
            kind: T![interface],
            matches: |input| match_keyword(input, "interface"),
        },
        Rule {
            kind: T![impl],
            matches: |input| match_keyword(input, "impl"),
        },
        Rule {
            kind: T![match],
            matches: |input| match_keyword(input, "match"),
        },
        Rule {
            kind: T![const],
            matches: |input| match_keyword(input, "const"),
        },
        Rule {
            kind: T![return],
            matches: |input| match_keyword(input, "return"),
        },
        Rule {
            kind: T![defer],
            matches: |input| match_keyword(input, "defer"),
        },
        Rule {
            kind: T![true],
            matches: |input| match_keyword(input, "true"),
        },
        Rule {
            kind: T![false],
            matches: |input| match_keyword(input, "false"),
        },
        Rule {
            kind: T![while],
            matches: |input| match_keyword(input, "while"),
        },
        Rule {
            kind: T![for],
            matches: |input| match_keyword(input, "for"),
        },
        Rule {
            kind: T![range],
            matches: |input| match_keyword(input, "range"),
        },
        Rule {
            kind: T![break],
            matches: |input| match_keyword(input, "break"),
        },
        Rule {
            kind: T![continue],
            matches: |input| match_keyword(input, "continue"),
        },
        Rule {
            kind: T![nil],
            matches: |input| match_keyword(input, "nil"),
        },
        Rule {
            kind: T![mut],
            matches: |input| match_keyword(input, "mut"),
        },
        Rule {
            kind: T![spawn],
            matches: |input| match_keyword(input, "spawn"),
        },
        Rule {
            kind: T![let],
            matches: |input| match_keyword(input, "let"),
        },
        Rule {
            kind: T![<<=],
            matches: |input| match_three_chars(input, '<', '<', '='),
        },
        Rule {
            kind: T![>>=],
            matches: |input| match_three_chars(input, '>', '>', '='),
        },
        Rule {
            kind: T![string],
            matches: move |input| match_regex(input, &STRING_REGEX),
        },
        Rule {
            kind: T![comment],
            matches: move |input| match_regex(input, &COMMENT_REGEX),
        },
        Rule {
            kind: T![block comment],
            matches: move |input| match_regex(input, &MULTILINE_COMMENT_REGEX),
        },
        Rule {
            kind: T![int],
            matches: |input| {
                input
                    .char_indices()
                    .take_while(|(_, c)| c.is_ascii_digit())
                    .last()
                    .map(|(pos, _)| pos as u32 + 1)
            },
        },
        Rule {
            kind: T![float],
            matches: |input| match_regex(input, &FLOAT_REGEX),
        },
        Rule {
            kind: T![char],
            matches: |input| match_regex(input, &CHAR_REGEX),
        },
        Rule {
            kind: T![bool],
            matches: |input| match_keyword(input, "bool"),
        },
        Rule {
            kind: T![ident],
            matches: |input| match_regex(input, &IDENTIFIER_REGEX),
        },
    ]
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, T};

    /// walks `$tokens` and compares them to the given kinds.
    macro_rules! assert_tokens {
    ($tokens:ident, [$($kind:expr,)*]) => {
        {
            let mut it = $tokens.iter();
            $(
                let token = it.next().expect("not enough tokens");
                assert_eq!(token.kind, $kind);
            )*
        }
    };
}

    #[test]
    fn single_char_tokens() {
        let input = "+-(.):";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert_tokens!(tokens, [T![+], T![-], T!['('], T![.], T![')'], T![:], T![EOF],]);
    }

    #[test]
    fn single_char_tokens_with_whitespace() {
        let input = "   + -  (.): ";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let leading_space = &tokens[0];
        assert_eq!(leading_space.kind, T![ws]);
        assert_eq!(leading_space.len(), 3);

        let space_after_minus = &tokens[4];
        assert_eq!(space_after_minus.kind, T![ws]);
        assert_eq!(space_after_minus.len(), 2);

        let trailing_space = &tokens[9];
        assert_eq!(trailing_space.kind, T![ws]);
        assert_eq!(trailing_space.len(), 1);

        let tokens: Vec<_> = tokens.into_iter().filter(|t| t.kind != T![ws]).collect();
        assert_tokens!(tokens, [T![+], T![-], T!['('], T![.], T![')'], T![:], T![EOF],]);
    }

    #[test]
    fn unknown_input() {
        let input = "{$$$$$$$+";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert_tokens!(tokens, [T!['{'], T![error], T![+], T![EOF],]);
    }

    #[test]
    fn token_spans() {
        {
            let input = "+-(.):";
            let mut lexer = Lexer::new(input);
            let tokens = lexer.tokenize();
            let dot = tokens[3];
            assert_eq!(dot.kind, T![.]);
            assert_eq!(dot.span, (3..4).into())
        }
        {
            let input = "{$$$$$$$+";
            let mut lexer = Lexer::new(input);
            let tokens = lexer.tokenize();
            let error = tokens[1];
            assert_eq!(error.kind, T![error]);
            assert_eq!(error.span, (1..8).into())
        }
    }

    #[test]
    fn maybe_multiple_char_tokens() {
        let input = "&&=<=_!=||>>=";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert_tokens!(
            tokens,
            [T![&&], T![=], T![<=], T![_], T![!=], T![||], T![>>=], T![EOF],]
        );
    }

    #[test]
    fn keywords() {
        let input = "if let = struct else fn match mut spawn package use";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().into_iter().filter(|t| t.kind != T![ws]).collect();
        assert_tokens!(
            tokens,
            [
                T![if],
                T![let],
                T![=],
                T![struct],
                T![else],
                T![fn],
                T![match],
                T![mut],
                T![spawn],
                T![package],
                T![use],
                T![EOF],
            ]
        );
    }
}
