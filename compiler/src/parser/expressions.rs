use crate::{
    lexer::{Token, TokenKind},
    T,
};

use super::{ast, error::ParseError, Parser, Result};

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
    pub fn expression(&mut self) -> Result<ast::Stmt> {
        match self.parse_expression(0) {
            Ok(expr) => return Ok(expr),
            Err(found) => return Err(found),
        }
    }

    fn parse_expr_fn_call(&mut self, fn_name: String) -> Result<ast::Stmt> {
        let mut args = Vec::new();
        self.consume(T!['(']);
        while !self.at(T![')']) {
            let arg = self.expression();
            args.push(arg?);
            if self.at(T![,]) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);

        Ok(ast::Stmt::FnCall {
            function: fn_name,
            args,
        })
    }

    fn parse_exp_array_access(&mut self, array: String) -> Result<ast::Stmt> {
        self.consume(T!['[']);
        let index = self.expression();
        self.consume(T![']']);

        Ok(ast::Stmt::ArrayAccess {
            array,
            index: Box::new(index?),
        })
    }

    fn parse_expression(&mut self, binding_power: u8) -> Result<ast::Stmt> {
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
                Ok(ast::Stmt::Literal(expr_lit))
            }
            T![ident] => {
                let name = {
                    let ident_token = self.next().unwrap();
                    self.text(ident_token).to_string() // <- now we need a copy
                };

                if self.at(T!['[']) {
                    let arr = self.parse_exp_array_access(name);
                    arr
                } else if self.at(T!['(']) {
                    let fn_ = self.parse_expr_fn_call(name);
                    fn_
                } else if self.at(T![.]) {
                    // caller needs to consume the dot
                    self.consume(T![.]);

                    let expr = self.expression();
                    Ok(ast::Stmt::StructAccess {
                        struct_name: name,
                        field: Box::new(expr?),
                    })
                } else {
                    // plain identifier
                    Ok(ast::Stmt::Identifier(name))
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
                    elements.push(element?);
                    if self.at(T![,]) {
                        self.consume(T![,]);
                    }
                }
                self.consume(T![']']);
                Ok(ast::Stmt::Array { elements })
            }
            op @ T![+] | op @ T![-] | op @ T![!] => {
                self.consume(op);
                let ((), right_binding_power) = op.prefix_binding_power();
                let expr = self.parse_expression(right_binding_power);

                Ok(ast::Stmt::PrefixOp {
                    op,
                    expr: Box::new(expr?),
                })
            }
            T![nil] => {
                self.consume(T![nil]);

                Ok(ast::Stmt::Literal(ast::Lit::Nil()))
            }
            found => {
                return Err(ParseError::UnexpectedToken {
                    found,
                    expected: vec![
                        T![int],
                        T![float],
                        T![string],
                        T![bool],
                        T![char],
                        T![ident],
                        T!['('],
                        T!['['],
                        T![+],
                        T![-],
                        T![!],
                        T![nil],
                    ],
                    position: self.position(),
                });
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
                    return Err(ParseError::UnexpectedToken {
                        found,
                        expected: vec![
                            T![+],
                            T![-],
                            T![*],
                            T![/],
                            T![^],
                            T![.],
                            T![=],
                            T![+=],
                            T![-=],
                            T![*=],
                            T![/=],
                            T![%=],
                            T![&=],
                            T![|=],
                            T![^=],
                            T![==],
                            T![!=],
                            T![&&],
                            T![||],
                            T![<],
                            T![<=],
                            T![>],
                            T![>=],
                            T![>>=],
                            T![<<=],
                            T![!],
                        ],
                        position: self.position(),
                    });
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
                lhs = Ok(ast::Stmt::PostfixOp {
                    op,
                    expr: Box::new(lhs?),
                });
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
                lhs = Ok(ast::Stmt::InfixOp {
                    op,
                    lhs: Box::new(lhs?),
                    rhs: Box::new(rhs?),
                });
                // parsed an operator --> go round the loop again
                continue;
            }
            break;
        }
        lhs
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{ast, Parser},
        T,
    };

    #[test]
    fn parse_expression() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.expression().unwrap()
        }

        // Weird spaces are to test that whitespace gets filtered out
        let expr = parse("42");
        assert_eq!(expr, ast::Stmt::Literal(ast::Lit::Int(42)));
        let expr = parse("  2.7768");
        assert_eq!(expr, ast::Stmt::Literal(ast::Lit::Float(2.7768)));
        let expr = parse(r#""I am a String!""#);
        assert_eq!(expr, ast::Stmt::Literal(ast::Lit::Str("I am a String!".to_string())));
        let expr = parse("foo");
        assert_eq!(expr, ast::Stmt::Identifier("foo".to_string()));
        let expr = parse("bar (  x, 2)");
        assert_eq!(
            expr,
            ast::Stmt::FnCall {
                function: "bar".to_string(),
                args: vec![
                    ast::Stmt::Identifier("x".to_string()),
                    ast::Stmt::Literal(ast::Lit::Int(2)),
                ],
            }
        );
        let expr = parse("!  is_visible");
        assert_eq!(
            expr,
            ast::Stmt::PrefixOp {
                op: T![!],
                expr: Box::new(ast::Stmt::Identifier("is_visible".to_string())),
            }
        );
        let expr = parse("(-13)");
        assert_eq!(
            expr,
            ast::Stmt::PrefixOp {
                op: T![-],
                expr: Box::new(ast::Stmt::Literal(ast::Lit::Int(13))),
            }
        );
    }

    #[test]
    fn parse_binary_expressions() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.expression().unwrap()
        }

        let expr = parse("4 + 2 * 3");
        assert_eq!(expr.to_string(), "(4 + (2 * 3))");

        let expr = parse("4 * 2 + 3");
        assert_eq!(expr.to_string(), "((4 * 2) + 3)");

        let expr = parse("4 - 2 - 3");
        assert_eq!(expr.to_string(), "((4 - 2) - 3)");

        let expr = parse("4 ^ 2 ^ 3");
        assert_eq!(expr.to_string(), "(4 ^ (2 ^ 3))");

        let expr = parse(r#"45.7 + 3 + 5 * 4^8^9 / 6 > 4 && test - 7 / 4 == "Hallo""#);
        assert_eq!(
            expr.to_string(),
            r#"((((45.7 + 3) + ((5 * (4 ^ (8 ^ 9))) / 6)) > 4) && ((test - (7 / 4)) == "Hallo"))"#
        );

        let expr = parse("2.0 / ((3.0 + 4.0) * (5.4 - 6.0)) * 7.0");
        assert_eq!(expr.to_string(), "((2 / ((3 + 4) * (5.4 - 6))) * 7)");

        let expr = parse("min ( test + 4 , sin(2*PI ))");
        assert_eq!(expr.to_string(), "min((test + 4),sin((2 * PI),),)");

        let expr = parse(r#"a <<= 3 + 4 * 7 ^ 6 / 4"#);
        assert_eq!(expr.to_string(), "(a <<= (3 + ((4 * (7 ^ 6)) / 4)))");

        let expr = parse("array[10]");
        assert_eq!(expr.to_string(), "array[10]");

        let expr = parse("array[10 + 2]");
        assert_eq!(expr.to_string(), "array[(10 + 2)]");

        let expr = parse("a += array[10 + 2]");
        assert_eq!(expr.to_string(), "(a += array[(10 + 2)])");

        let expr = parse("res.abs()");
        assert_eq!(expr.to_string(), "(res . abs())");

        let expr = parse("res.abs(math.rand[2])");
        assert_eq!(expr.to_string(), "(res . abs((math . rand[2]),))");
    }

    #[test]
    fn parse_postfix_op() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.expression().unwrap()
        }

        let expr = parse("4 + -2! * 3");
        assert_eq!(expr.to_string(), "(4 + ((- (2 !)) * 3))");
    }
}
