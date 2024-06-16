use crate::{
    lexer::{Token, TokenKind},
    parser::ast::Type,
    T,
};

use super::{ast, error::ParseError, Parser, Result};

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    fn parse_package(&mut self) -> Result<ast::Stmt> {
        self.consume(T![package]);
        let ident = self.next().expect("Expected identifier after `package`");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier after `package`, but found `{}`",
            ident.kind
        );
        let path = self.text(ident).to_string();
        self.consume(T![;]);
        Ok(ast::Stmt::Package { path })
    }

    fn parse_use(&mut self) -> Result<ast::Stmt> {
        self.consume(T![use]);
        let ident = self.next().expect("Expected identifier after `use`");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier after `use`, but found `{}`",
            ident.kind
        );
        let name = self.text(ident).to_string();
        self.consume(T![;]);
        Ok(ast::Stmt::Use { name })
    }

    fn parse_fn_signature(&mut self) -> Result<ast::Stmt> {
        let ident = self.next().expect("Expected identifier");
        assert_eq!(ident.kind, T![ident], "Expected identifier, but found `{}`", ident.kind);
        let fn_name = self.text(ident).to_string();

        let mut generics = None;
        if self.at(T![<]) {
            self.consume(T![<]);
            generics = Some(Vec::new());
            loop {
                let generic = self.type_(&None);
                generics.as_mut().unwrap().push(Box::new(generic));

                match self.peek() {
                    T![>] => {
                        self.consume(T![>]);
                        break;
                    }
                    T![,] => {
                        self.consume(T![,]);
                    }
                    found => panic!("Expected `,` or `>` after generic type, found `{}` instead", found),
                }
            }
        }

        self.consume(T!['(']);
        let mut parameters = Vec::new();
        while !self.at(T![')']) {
            let parameter_ident = self.next().expect(
                "Tried to parse function parameter, 
                but there were no more tokens",
            );
            assert_eq!(
                parameter_ident.kind,
                T![ident],
                "Expected identifier as function parameter, but found `{}`",
                parameter_ident.kind
            );
            let parameter_name = self.text(parameter_ident).to_string();
            self.consume(T![:]);
            let parameter_type = self.type_(&None);
            parameters.push((parameter_name, parameter_type));
            if self.at(T![,]) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);

        // return statement
        let mut return_types: Vec<Box<Type>> = Vec::new();
        if self.at(T![->]) {
            self.consume(T![->]);
            let mut multiple_types: bool = false;
            if self.at(T!['(']) {
                self.consume(T!['(']);
                multiple_types = true;
            }

            return_types.push(Box::new(self.type_(&None)));
            if self.at(T![,]) {
                while self.at(T![,]) {
                    self.consume(T![,]);
                    return_types.push(Box::new(self.type_(&None)));
                }
            }

            if multiple_types {
                if !self.at(T![')']) {
                    return Err(ParseError::UnexpectedToken {
                        found: self.peek(),
                        expected: vec![T![')']],
                        position: self.position(),
                    });
                } else {
                    self.consume(T![')']);
                }
            }
        }
        if return_types.is_empty() {
            Ok(ast::Stmt::FunctionSignature {
                name: fn_name,
                generics,
                parameters,
                return_type: None,
            })
        } else {
            Ok(ast::Stmt::FunctionSignature {
                name: fn_name,
                generics,
                parameters,
                return_type: Some(return_types),
            })
        }
    }

    fn parse_fn(&mut self) -> Result<ast::Stmt> {
        self.consume(T![fn]);
        let ident = self.next().expect("Expected identifier after `fn`");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier after `fn`, but found `{}`",
            ident.kind
        );
        let fn_name = self.text(ident).to_string();

        self.consume(T!['(']);
        let mut parameters = Vec::new();
        while !self.at(T![')']) {
            let parameter_ident = self.next().expect(
                "Tried to parse function parameter, 
                but there were no more tokens",
            );
            assert_eq!(
                parameter_ident.kind,
                T![ident],
                "Expected identifier as function parameter, but found `{}`",
                parameter_ident.kind
            );
            let parameter_name = self.text(parameter_ident).to_string();
            self.consume(T![:]);
            let parameter_type = self.type_(&None);
            parameters.push((parameter_name, parameter_type));
            if self.at(T![,]) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);

        // return statement
        let mut return_type: Option<Box<Type>> = None;
        if self.at(T![->]) {
            self.consume(T![->]);
            return_type = Some(Box::new(self.type_(&None)));
        }

        assert!(self.at(T!['{']), "Expected a block after function header");
        self.consume(T!['{']);

        let mut body = Vec::new();
        while !self.at(T!['}']) {
            let stmt = match self.statement() {
                Ok(stmt) => stmt,
                Err(found) => return Err(found),
            };
            body.push(stmt);
        }

        self.consume(T!['}']);

        let return_stmt = Some(Box::new(ast::Stmt::Return {
            value: Box::new(body.last().unwrap().clone()),
        }));

        Ok(ast::Stmt::Function {
            name: fn_name,
            parameters,
            body,
            return_type,
            return_stmt,
        })
    }

    fn parse_interface(&mut self) -> Result<ast::Stmt> {
        self.consume(T![interface]);

        let ident = self.next().expect("Expected identifier after `interface`");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier after `interface`, but found `{}`",
            ident.kind
        );
        let interface_name = self.text(ident).to_string();

        let interface_type = self.type_(&Some(interface_name.clone()));

        let mut methods = Vec::new();

        self.consume(T!['{']);
        while !self.at(T!['}']) {
            let fn_signature = self.parse_fn_signature();

            if !self.at(T![;]) {
                return Err(ParseError::InvalidExpressionStatement {
                    position: self.position(),
                });
            }
            self.consume(T![;]);

            match fn_signature {
                Ok(signature) => {
                    methods.push(Box::new(signature));
                }
                Err(_) => {
                    return Err(ParseError::InvalidExpressionStatement {
                        position: self.position(),
                    });
                }
            }
        }

        self.consume(T!['}']);

        Ok(ast::Stmt::Interface {
            name: interface_name,
            type_: Some(Box::new(interface_type)),
            methods,
        })
    }

    fn parse_struct(&mut self) -> Result<ast::Stmt> {
        self.consume(T![struct]);

        let ident = self.next().expect("Expected identifier after `struct`");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier after `struct`, but found `{}`",
            ident.kind
        );

        let name = self.text(ident).to_string();

        let st_type = self.type_(&Some(name.clone()));

        let mut members = Vec::new();

        self.consume(T!['{']);
        while !self.at(T!['}']) {
            let member_ident = self
                .next()
                .expect("Tried to parse struct member, but there were no more tokens");
            assert_eq!(
                member_ident.kind,
                T![ident],
                "Expected identifier as struct member, but found `{}`",
                member_ident.kind
            );
            let member_name = self.text(member_ident).to_string();
            let member_type = self.type_(&None);
            members.push((member_name, member_type));
            if self.at(T![,]) {
                self.consume(T![,]);
            }
        }

        self.consume(T!['}']);

        Ok(ast::Stmt::Struct {
            name,
            type_: st_type,
            members,
        })
    }

    fn type_(&mut self, opt_name: &Option<String>) -> ast::Type {
        let mut type_name: Option<String> = opt_name.to_owned();
        if type_name.is_none() {
            let ident = self.next().expect("Tried to parse type, but there were no more tokens");
            assert_eq!(
                ident.kind,
                T![ident],
                "Expected identifier at start of type, but found `{}`",
                ident.kind
            );
            let val = self.text(ident).to_string();
            type_name = Some(val);
        }

        if self.at(T![<]) {
            self.consume(T![<]);
            let mut generics = Vec::new();

            loop {
                let generic = self.type_(&None);
                generics.push(Box::new(generic));

                match self.peek() {
                    T![>] => {
                        self.consume(T![>]);
                        break;
                    }
                    T![,] => {
                        self.consume(T![,]);
                    }
                    found => panic!("Expected `,` or `>` after generic type, found `{}` instead", found),
                }
            }
            ast::Type {
                name: type_name.unwrap(),
                generics: Some(generics),
            }
        } else {
            ast::Type {
                name: type_name.unwrap(),
                generics: None,
            }
        }
    }

    fn parse_let(&mut self) -> Result<ast::Stmt> {
        self.consume(T![let]);
        let ident = self.next().expect("Expected identifier after `let`");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier after `let`, but found `{}`",
            ident.kind
        );
        let name = self.text(ident).to_string();
        self.consume(T![=]);
        let value = self.statement();
        self.consume(T![;]);

        Ok(ast::Stmt::Let {
            identifier: name,
            statement: Box::new(value.unwrap()),
        })
    }

    fn parse_return(&mut self) -> Result<ast::Stmt> {
        self.consume(T![return]);
        let value = self.statement();
        self.consume(T![;]);

        Ok(ast::Stmt::Return {
            value: Box::new(value?),
        })
    }

    fn parse_if(&mut self) -> Result<ast::Stmt> {
        self.consume(T![if]);
        self.consume(T!['(']);
        let condition = self.expression();
        self.consume(T![')']);

        assert!(self.at(T!['{']), "Expected a block after `if` statement");
        let body = self.statement();
        let body = match body {
            Ok(ast::Stmt::Block { stmts }) => stmts,
            _ => unreachable!(),
        };

        let else_stmt = if self.at(T![else]) {
            self.consume(T![else]);
            assert!(
                self.at(T![if]) || self.at(T!['{']),
                "Expected a block or an `if` after `else` statement"
            );
            self.statement()
        } else {
            Ok(ast::Stmt::Empty)
        };

        Ok(ast::Stmt::IfStmt {
            condition: Box::new(condition?),
            body,
            else_stmt: Some(Box::new(else_stmt.unwrap_or(ast::Stmt::Empty))),
        })
    }

    fn parse_for(&mut self) -> Result<ast::Stmt> {
        self.consume(T![for]);

        let ident = self.next().expect("Expected identifier after `for`");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier after `for`, but found `{}`",
            ident.kind
        );
        let name = self.text(ident).to_string();

        self.consume(T![range]);
        let expr = self.expression();
        let range = Box::new(expr?);

        assert!(self.at(T!['{']), "Expected a block after `for` statement");
        self.consume(T!['{']);

        let mut body = Vec::new();
        while !self.at(T!['}']) {
            let stmt = self.statement();
            body.push(stmt?);
        }

        self.consume(T!['}']);

        Ok(ast::Stmt::RangeStmt {
            iterator: name,
            range,
            body,
        })
    }

    fn parse_block(&mut self) -> Result<ast::Stmt> {
        self.consume(T!['{']);
        let mut stmts = Vec::new();
        while !self.at(T!['}']) {
            let stmt = self.statement();
            match stmt {
                Ok(stmt) => stmts.push(stmt),
                Err(_) => break,
            }
        }
        self.consume(T!['}']);
        Ok(ast::Stmt::Block { stmts })
    }

    fn parse_fn_call(&mut self, name: String) -> Result<ast::Stmt> {
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
        Ok(ast::Stmt::FnCall { function: name, args })
    }

    fn parse_assignment(&mut self, name: String) -> Result<ast::Stmt> {
        self.consume(T![=]);

        let value = self.expression();

        self.consume(T![;]);

        Ok(ast::Stmt::Assignment {
            var_name: name,
            value: Box::new(value?),
        })
    }

    fn parse_dot(&mut self, struct_name: String) -> Result<ast::Stmt> {
        self.consume(T![.]);
        let stmt = self.statement();
        Ok(ast::Stmt::StructAccess {
            struct_name,
            field: Box::new(stmt?),
        })
    }

    fn parse_ident(&mut self) -> Result<ast::Stmt> {
        let ident = self.next().unwrap();
        let name = self.text(ident).to_string();

        let next = self.peek();
        match next {
            T![=] => {
                let assignment = self.parse_assignment(name);
                assignment
            }
            T![.] => {
                let field_access = self.parse_dot(name);
                field_access
            }
            T!['('] => {
                let fn_call = self.parse_fn_call(name);
                Ok(fn_call?)
            }
            found => {
                return Err(ParseError::UnexpectedToken {
                    found,
                    expected: vec![T![=], T![.], T!['(']],
                    position: self.position(),
                });
            }
        }
    }

    fn statement(&mut self) -> Result<ast::Stmt> {
        let next = self.peek();
        let ast = match next {
            T![int] | T![float] | T![string] | T![bool] | T![char] => {
                let lit = self.expression();
                Ok(lit?)
            }
            T![let] => {
                let let_stmt = self.parse_let();
                let_stmt
            }
            T![ident] => {
                let ident = self.parse_ident();
                ident
            }
            T![return] => {
                let ret = self.parse_return();
                ret
            }
            T![if] => {
                let if_stmt = self.parse_if();
                if_stmt
            }
            T!['{'] => {
                let block = self.parse_block();
                block
            }
            T![for] => {
                let for_stmt = self.parse_for();
                for_stmt
            }
            T![;] => {
                self.consume(T![;]);
                Ok(ast::Stmt::Empty)
            }
            T![defer] => {
                self.consume(T![defer]);
                let stmt = self.statement();
                Ok(ast::Stmt::Defer { stmt: Box::new(stmt?) })
            }
            T![nil] => {
                self.consume(T![nil]);
                Ok(ast::Stmt::Literal(ast::Lit::Nil()))
            }
            found => {
                return Err(ParseError::UnexpectedToken {
                    found,
                    expected: vec![T![let], T![ident], T![return], T![if], T!['{'], T![for], T![defer]],
                    position: self.position(),
                });
            }
        };
        ast
    }

    pub fn parse_input(&mut self, filename: &str) -> Result<ast::SourceFile> {
        let mut pkg_counter = 0;
        let mut stmts = Vec::new();
        loop {
            let next = self.peek();
            match next {
                T![package] => {
                    let stmt = self.parse_package();
                    stmts.push(stmt);
                    pkg_counter += 1;
                    if pkg_counter > 1 {
                        panic!("Only one package statement is allowed per file");
                    }
                }
                T![use] => {
                    let stmt = self.parse_use();
                    stmts.push(stmt);
                }
                T![fn] => {
                    let stmt = self.parse_fn();
                    stmts.push(stmt);
                }
                T![struct] => {
                    let stmt = self.parse_struct();
                    stmts.push(stmt);
                }
                T![interface] => {
                    let stmt = self.parse_interface();
                    stmts.push(stmt);
                }
                T![EOF] => break,
                found => match self.statement() {
                    Ok(stmt) => stmts.push(Ok(stmt)),
                    Err(_) => {
                        return Err(ParseError::UnexpectedToken {
                            found,
                            expected: vec![T![let], T![ident], T![return], T![if], T!['{'], T![for], T![defer]],
                            position: self.position(),
                        });
                    }
                },
            }
        }
        Ok(ast::SourceFile {
            name: filename.to_string(),
            statements: stmts,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Lexer,
        parser::{ast, Parser},
        T,
    };
    use unindent::unindent;

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
    fn function() {
        let input = r#"
    // tests stuff
    fn test(var Type, var2_ bool) -> bool {
        let x = "String content \" test" + 7 / 27.3e-2^4;
        let chars = x.chars();
        if let c = chars.next() {
            x = x + c;
        } else if !var2_ {
            x = x + ",";
        }
        return true;
    }
    /* end of file */
"#;
        let input = unindent(input);
        let mut lexer = Lexer::new(input.as_str());
        let tokens: Vec<_> = lexer.tokenize().into_iter().filter(|t| t.kind != T![ws]).collect();
        assert_tokens!(
            tokens,
            [
                // comment
                T![comment],
                // function signature
                T![fn],
                T![ident],
                T!['('],
                T![ident],
                T![ident],
                T![,],
                T![ident],
                T![bool],
                T![')'],
                T![->],
                T![bool],
                T!['{'],
                // `x` assignment
                T![let],
                T![ident],
                T![=],
                T![string],
                T![+],
                T![int],
                T![/],
                T![float],
                T![^],
                T![;],
                // `chars` assignment
                T![let],
                T![ident],
                T![=],
                T![ident],
                T![.],
                T![ident],
                T!['('],
                T![')'],
                T![;],
                // if
                T![if],
                T![let],
                T![ident],
                T![=],
                T![ident],
                T![.],
                T![ident],
                T!['('],
                T![')'],
                T!['{'],
                // `x` re-assignment
                T![ident],
                T![=],
                T![ident],
                T![+],
                T![ident],
                T![;],
                // else if
                T!['}'],
                T![else],
                T![if],
                T![!],
                T![ident],
                T!['{'],
                // `x` re-assignment
                T![ident],
                T![=],
                T![ident],
                T![+],
                T![string],
                T![;],
                T!['}'], // end if
                T![return],
                T![true],
                T![;],
                T!['}'],           // end fn
                T![block comment], // block comment
                T![EOF],
            ]
        );
    }

    #[test]
    fn struct_def() {
        let input = r#"
    struct Foo<T> {
        bar Bar<T>
    }
    let x = 'a';
"#;
        let input = unindent(input);
        let input = input.as_str();
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().into_iter().filter(|t| t.kind != T![ws]).collect();
        assert_tokens!(
            tokens,
            [
                // struct definition/type
                T![struct],
                T![ident],
                T![<],
                T![ident],
                T![>],
                T!['{'],
                // member `bar` of type `Bar<T>`
                T![ident],
                T![ident],
                T![<],
                T![ident],
                T![>],
                T!['}'], // end struct
                T![let],
                T![ident],
                T![=],
                T![char],
                T![;], // let statement
                T![EOF],
            ]
        );
        let bar = tokens[6];
        assert_eq!(bar.span, (20..23).into()); // unindented span
        assert_eq!(bar.text(input), "bar");
        let foo = tokens[1];
        assert_eq!(foo.text(input), "Foo");
    }

    #[test]
    fn parse_statements() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.statement().unwrap()
        }

        let stmt = parse(
            unindent(
                r#"
    {
       let x = 7 + sin(y);
        {
            x = 3;
            if (bar < 3) {
                x = x + 1;
                y = 3 * x;
            } else if (bar < 2) {
                let i = 2!;
                x = x + i;
            } else {
                x = 1;
            }
        }
    }
"#,
            )
            .as_str(),
        );

        let stmts = match stmt {
            ast::Stmt::Block { stmts } => stmts,
            _ => unreachable!(),
        };
        assert_eq!(stmts.len(), 2);

        let let_stmt = &stmts[0];
        match let_stmt {
            ast::Stmt::Let {
                identifier: var_name, ..
            } => assert_eq!(var_name, "x"),
            _ => unreachable!(),
        }

        let stmts = match &stmts[1] {
            ast::Stmt::Block { stmts } => stmts,
            _ => unreachable!(),
        };
        assert_eq!(stmts.len(), 2);

        let assignment_stmt = &stmts[0];
        match assignment_stmt {
            ast::Stmt::Assignment { var_name, .. } => {
                assert_eq!(var_name, "x");
            }
            _ => unreachable!(),
        }

        let if_stmt = &stmts[1];
        match if_stmt {
            ast::Stmt::IfStmt {
                condition,
                body,
                else_stmt,
            } => {
                assert!(matches!(
                    &**condition,
                    ast::Stmt::InfixOp {
                        op: T![<],
                        lhs: _lhs,
                        rhs: _rhs,
                    }
                ));
                assert_eq!(body.len(), 2);
                let x_assignment = &body[0];
                match x_assignment {
                    ast::Stmt::Assignment { var_name, .. } => assert_eq!(var_name, "x"),
                    _ => unreachable!(),
                }
                let y_assignment = &body[1];
                match y_assignment {
                    ast::Stmt::Assignment { var_name, .. } => assert_eq!(var_name, "y"),
                    _ => unreachable!(),
                }

                let else_stmt = match else_stmt {
                    Some(stmt) => &**stmt,
                    None => unreachable!(),
                };

                match else_stmt {
                    ast::Stmt::IfStmt {
                        condition,
                        body,
                        else_stmt,
                    } => {
                        assert!(matches!(
                            &**condition,
                            ast::Stmt::InfixOp {
                                op: T![<],
                                lhs: _lhs,
                                rhs: _rhs,
                            }
                        ));
                        assert_eq!(body.len(), 2);
                        let let_i = &body[0];
                        match let_i {
                            ast::Stmt::Let {
                                identifier: var_name, ..
                            } => assert_eq!(var_name, "i"),
                            _ => unreachable!(),
                        }
                        let x_assignment = &body[1];
                        match x_assignment {
                            ast::Stmt::Assignment { var_name, .. } => assert_eq!(var_name, "x"),
                            _ => unreachable!(),
                        }

                        let else_stmt = match else_stmt {
                            Some(stmt) => &**stmt,
                            None => unreachable!(),
                        };

                        let stmts = match else_stmt {
                            ast::Stmt::Block { stmts } => stmts,
                            _ => unreachable!(),
                        };
                        assert_eq!(stmts.len(), 1);

                        let x_assignment = &stmts[0];
                        match x_assignment {
                            ast::Stmt::Assignment { var_name, .. } => assert_eq!(var_name, "x"),
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                };
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn parse_range_statement() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.statement().unwrap()
        }

        let stmt = parse(
            unindent(
                r#"        
    for x range [0, 1, 2] {
        x = x + 1;
    }
"#,
            )
            .as_str(),
        );

        match stmt {
            ast::Stmt::RangeStmt { iterator, range, body } => {
                assert_eq!(iterator, "x");
                assert_eq!(
                    range,
                    Box::new(ast::Stmt::Array {
                        elements: vec![
                            ast::Stmt::Literal(ast::Lit::Int(0)),
                            ast::Stmt::Literal(ast::Lit::Int(1)),
                            ast::Stmt::Literal(ast::Lit::Int(2)),
                        ]
                    })
                );
                assert_eq!(body.len(), 1);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn parse_function() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.parse_fn().unwrap()
        }

        let item = parse(
            unindent(
                r#"
        fn wow_we_did_it(x: String, bar: Bar<Baz<T>, U>) {
            let x = 7 + sin(y);
            {
                x = 3;
                if (bar < 3) {
                    x = x + 1;
                    y = 3 * x;
                } else if (bar < 2) {
                    let i = 2!;
                    x = x + i;
                } else {
                    x = 1;
                }
            }
        }
    "#,
            )
            .as_str(),
        );

        match item {
            ast::Stmt::Function {
                name,
                parameters,
                body,
                return_type: _,
                return_stmt: _,
            } => {
                assert_eq!(name, "wow_we_did_it");
                assert_eq!(parameters.len(), 2);
                let (bar, bar_type) = &parameters[1];
                assert_eq!(bar, "bar");
                assert_eq!(body.len(), 2);
            }
            _ => unreachable!(),
        };
    }

    #[test]
    fn parse_function_with_return() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.parse_fn().unwrap()
        }

        let item = parse(
            unindent(
                r#"
        fn wow_we_did_it(x: String, bar: Bar<Baz<T>, U>) -> string {
            let x = 7 + sin(y);
            {
                x = 3;
                if (bar < 3) {
                    x = x + 1;
                    y = 3 * x;
                } else if (bar < 2) {
                    let i = 2!;
                    x = x + i;
                } else {
                    x = 1;
                }
            }
            return "hello, Alaska!";
        }
    "#,
            )
            .as_str(),
        );

        match item {
            ast::Stmt::Function {
                name,
                parameters,
                body,
                return_type,
                return_stmt: _,
            } => {
                assert_eq!(name, "wow_we_did_it");
                assert_eq!(parameters.len(), 2);

                let (bar, bar_type) = &parameters[1];
                assert_eq!(bar, "bar");

                assert_eq!(body.len(), 3);
                assert_eq!(
                    body.last(),
                    Some(&ast::Stmt::Return {
                        value: Box::new(ast::Stmt::Literal(ast::Lit::Str("hello, Alaska!".to_string())))
                    })
                );
            }
            _ => unreachable!(),
        };
    }

    #[test]
    fn parse_struct() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.parse_struct().unwrap()
        }

        let item = parse(
            unindent(
                r#"
        struct Foo<T, U> {
            x String,
            bar Bar<Baz<T>, U>
        }
    "#,
            )
            .as_str(),
        );

        match item {
            ast::Stmt::Struct { name, members, type_ } => {
                assert_eq!(name, "Foo");
                assert_eq!(members.len(), 2);
                let (x, x_type) = &members[0];

                assert_eq!(x, "x");

                let (bar, bar_type) = &members[1];
                assert_eq!(bar, "bar");
            }
            _ => unreachable!(),
        };
    }

    #[test]
    fn parse_input() {
        fn parse(input: &str) -> ast::SourceFile {
            let mut parser = Parser::new(input);
            parser.parse_input("test").unwrap()
        }

        let item = parse(
            unindent(
                r#"
        package main;

        fn wow_we_did_it(x: string, bar: Bar<Baz<T>, U>) {
            let x = 7 + sin(y);
            {
                x = 3;
                if (bar < 3) {
                    x = x + 1;
                    y = 3 * x;
                } else if (bar < 2) {
                    let i = 2!;
                    x = x + i;
                } else {
                    x = 1;
                }
            }
        }

        struct Foo<T, U> {
            x String,
            bar Bar<Baz<T>, U>
        }
    "#,
            )
            .as_str(),
        );

        match item {
            ast::SourceFile { name, statements } => {
                assert_eq!(name, "test");
                assert_eq!(statements.len(), 3);
                for stmt in statements {
                    assert!(stmt.is_ok());
                }
            }
        };
    }
}
