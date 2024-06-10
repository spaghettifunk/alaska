use crate::{
    lexer::{Token, TokenKind},
    T,
};

use super::{ast, error::ParseError, Parser};

trait Operator {
    /// Prefix operators bind their operand to the right.
    fn prefix_binding_power(&self) -> ((), u8);

    /// Infix operators bind two operands, lhs and rhs.
    fn infix_binding_power(&self) -> Option<(u8, u8)>;

    /// Postfix operators bind their operand to the left.
    fn postfix_binding_power(&self) -> Option<(u8, ())>;
}

impl Operator for TokenKind {
    fn prefix_binding_power(&self) -> ((), u8) {
        match self {
            T![+] | T![-] | T![!] => ((), 51),
            // Prefixes are the only operators we have already seen
            // when we call this, so we know the token must be
            // one of the above
            _ => unreachable!("Not a prefix operator: {:?}", self),
        }
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        let result = match self {
            T![=] | T![+=] | T![-=] | T![*=] | T![/=] | T![%=] | T![&=] | T![|=] | T![^=] | T![<<=] | T![>>=] => (1, 2),
            T![||] => (3, 4),
            T![&&] => (5, 6),
            T![==] | T![!=] => (7, 8),
            T![<] | T![>] | T![<=] | T![>=] => (9, 10),
            T![+] | T![-] => (11, 12),
            T![*] | T![/] => (13, 14),
            T![^] => (22, 21),
            T![.] => (31, 32), // <- This binds stronger to the left!
            _ => return None,
        };
        Some(result)
    }

    fn postfix_binding_power(&self) -> Option<(u8, ())> {
        let result = match self {
            T![!] => (101, ()),
            _ => return None,
        };
        Some(result)
    }
}

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    #[inline]
    pub fn expression(&mut self) -> ast::Expr {
        self.parse_expression(0)
    }

    pub fn parse_expression(&mut self, binding_power: u8) -> ast::Expr {
        let next = self.peek();
        let mut lhs = match next {
            // `lit @ T![int]` (and similar) gives a name to the kind that is matched, so that I can use it again in the second match.
            // The result is equivalent to calling let lit = next again at the start of the outer match
            lit @ T![int] | lit @ T![float] | lit @ T![string] | lit @ T![bool] | lit @ T![char] => {
                let literal_text = {
                    // the calls on `self` need to be split, because `next` takes
                    // `&mut self` if `peek` is not `T![EOF]`, then there must be
                    // a next token
                    let literal_token = self.next().unwrap();
                    self.text(literal_token)
                };
                let expr_lit = match lit {
                    T![int] => ast::Lit::Int(
                        literal_text
                            .parse()
                            .expect(&format!("invalid integer literal: `{}`", literal_text)),
                    ),
                    T![float] => ast::Lit::Float(
                        literal_text
                            .parse()
                            .expect(&format!("invalid floating point literal: `{}`", literal_text)),
                    ),
                    T![string] => ast::Lit::Str(
                        // trim the quotation marks
                        literal_text[1..(literal_text.len() - 1)].to_string(),
                    ),
                    T![bool] => ast::Lit::Bool(
                        literal_text
                            .parse()
                            .expect(&format!("invalid boolean literal: `{}`", literal_text)),
                    ),
                    T![char] => ast::Lit::Char(
                        // trim the quotation marks
                        literal_text[1..(literal_text.len() - 1)].chars().next().unwrap(),
                    ),
                    _ => unreachable!(),
                };
                ast::Expr::Literal(expr_lit)
            }
            T![ident] => {
                let name = {
                    let ident_token = self.next().unwrap();
                    self.text(ident_token).to_string() // <- now we need a copy
                };
                // self.consume(T![ident]);

                if self.at(T!['[']) {
                    // array access
                    self.consume(T!['[']);
                    let index = self.parse_expression(0);
                    self.consume(T![']']);
                    ast::Expr::ArrayAccess {
                        array: name,
                        index: Box::new(index),
                    }
                } else if self.at(T!['(']) {
                    //  function call
                    let mut args = Vec::new();
                    self.consume(T!['(']);
                    while !self.at(T![')']) {
                        let arg = self.parse_expression(0);
                        args.push(arg);
                        if self.at(T![,]) {
                            self.consume(T![,]);
                        }
                    }
                    self.consume(T![')']);
                    ast::Expr::FnCall { fn_name: name, args }
                } else {
                    // plain identifier
                    ast::Expr::Ident(name)
                }
            }
            T!['('] => {
                // There is no AST node for grouped expressions.
                // Parentheses just influence the tree structure.
                self.consume(T!['(']);
                let expr = self.parse_expression(0);
                self.consume(T![')']);
                expr
            }
            T!['['] => {
                self.consume(T!['[']);
                let mut elements = Vec::new();
                while !self.at(T![']']) {
                    let element = self.parse_expression(0);
                    elements.push(element);
                    if self.at(T![,]) {
                        self.consume(T![,]);
                    }
                }
                self.consume(T![']']);
                ast::Expr::Array { elements }
            }
            op @ T![+] | op @ T![-] | op @ T![!] => {
                self.consume(op);
                let ((), right_binding_power) = op.prefix_binding_power();
                let expr = self.parse_expression(right_binding_power);
                ast::Expr::PrefixOp {
                    op,
                    expr: Box::new(expr),
                }
            }
            kind => {
                // TODO: parser error here
                panic!("Unexpected token in expression: `{}`", kind);
            }
        };

        loop {
            let next = self.peek();
            let op = match next {
                T![+]
                | T![-]
                | T![*]
                | T![/]
                | T![^]
                | T![.]
                | T![=]
                | T![+=]
                | T![-=]
                | T![*=]
                | T![/=]
                | T![%=]
                | T![&=]
                | T![|=]
                | T![^=]
                | T![==]
                | T![!=]
                | T![&&]
                | T![||]
                | T![<]
                | T![<=]
                | T![>]
                | T![>=]
                | T![>>=]
                | T![<<=]
                | T![!] => next,
                T![EOF] => break,
                T!['('] | T![')'] | T!['['] | T![']'] | T!['{'] | T!['}'] | T![,] | T![;] => break,
                found => {
                    // TODO: parser error here
                    panic!("Unexpected token in expression: `{}`", found);
                }
            };

            if let Some((left_binding_power, ())) = op.postfix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power than
                    // new one --> end of expression
                    break;
                }

                self.consume(op);
                // no recursive call here, because we have already
                // parsed our operand `lhs`
                lhs = ast::Expr::PostfixOp {
                    op,
                    expr: Box::new(lhs),
                };
                // parsed an operator --> go round the loop again
                continue;
            }

            if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power than
                    // new one --> end of expression
                    break;
                }

                self.consume(op);
                let rhs = self.parse_expression(right_binding_power);
                lhs = ast::Expr::InfixOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                // parsed an operator --> go round the loop again
                continue;
            }
            break;
        }

        lhs
    }
}
