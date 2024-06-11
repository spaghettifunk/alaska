mod rules;
mod token;

use crate::T;
pub use token::{Span, Token, TokenKind};

use self::rules::{unambiguous_single_char, Rule};

pub struct Lexer<'input> {
    input: &'input str,
    position: u32,
    eof: bool,
    rules: Vec<Rule>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            position: 0,
            eof: false,
            rules: rules::get_rules(),
        }
    }

    fn valid_token(&mut self, input: &str) -> Option<Token> {
        let next = input.chars().next().unwrap();
        let (len, kind) = if next.is_whitespace() {
            (
                input
                    .char_indices()
                    .take_while(|(_, c)| c.is_whitespace())
                    .last()
                    .unwrap() // we know there is at least one whitespace character
                    .0 as u32
                    + 1,
                T![ws],
            )
        } else if let Some(kind) = unambiguous_single_char(next) {
            (1, kind)
        } else {
            self.rules
                .iter()
                // `max_by_key` returns the last element if multiple
                // rules match, but we want earlier rules to "win"
                // against later ones
                .rev()
                .filter_map(|rule| Some(((rule.matches)(input)?, rule.kind)))
                .max_by_key(|&(len, _)| len)?
        };

        let start = self.position;
        self.position += len;
        Some(Token {
            kind,
            span: Span {
                start,
                end: start + len,
            },
        })
    }

    pub fn next_token(&mut self, input: &str) -> Token {
        self.valid_token(input).unwrap_or_else(|| self.invalid_token(input))
    }

    /// Always "succeeds", because it creates an error `Token`.
    fn invalid_token(&mut self, input: &str) -> Token {
        /*
        1) Iterate over each character and its position in the input string.
        2) For each character, check if the substring starting from that character is a valid token using self.valid_token.
        3) Find the position of the first character where self.valid_token returns Some.
        4) If such a position is found, use it. If not, use the length of the input string.
        */
        let start = self.position;
        let len = input
            .char_indices()
            .find(|(pos, _)| self.valid_token(&input[*pos..]).is_some())
            .map(|(pos, _)| pos)
            .unwrap_or_else(|| input.len());

        debug_assert!(len <= input.len());

        // Because `valid_token` advances our position,
        // we need to reset it to after the erroneous token.
        let len = len as u32;
        self.position = start + len;
        Token {
            kind: T![error],
            span: Span {
                start,
                end: start + len,
            },
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.collect()
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position as usize >= self.input.len() {
            if self.eof {
                return None;
            }
            self.eof = true;
            Some(Token {
                kind: T![EOF],
                span: Span {
                    start: self.position,
                    end: self.position,
                },
            })
        } else {
            Some(self.next_token(&self.input[self.position as usize..]))
        }
    }
}
