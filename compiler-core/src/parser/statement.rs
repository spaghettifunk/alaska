use std::collections::HashMap;

use crate::{lexer::Token, parser::ast::Type, T};

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
        Ok(ast::Stmt::PackageDeclaration(path.clone()))
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
        Ok(ast::Stmt::UseDeclaration(name.clone()))
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
                let generic = self.parse_type(&None, false, false);
                generics.as_mut().unwrap().push(generic);

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
        let mut parameters = HashMap::new();
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
            let parameter_type = self.parse_type(&None, false, false);
            parameters.insert(parameter_name, parameter_type);
            if self.at(T![,]) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);

        // return type
        let mut return_type: Option<Type> = None;
        if self.at(T![->]) {
            self.consume(T![->]);
            return_type = Some(self.parse_type(&None, false, false));
        }

        Ok(ast::Stmt::InterfaceFunctionSignature {
            name: fn_name,
            generics,
            parameters: if parameters.len() > 0 { Some(parameters) } else { None },
            return_type,
        })
    }

    fn parse_fn(&mut self, is_public: bool) -> Result<ast::Stmt> {
        self.consume(T![fn]);
        let ident = self.next().expect("Expected identifier after `fn`");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier after `fn`, but found `{}`",
            ident.kind
        );
        let fn_name = self.text(ident).to_string();

        // parse the generics if any
        let mut generics = None;
        if self.at(T![<]) {
            self.consume(T![<]);
            generics = Some(Vec::new());
            loop {
                let generic = self.parse_type(&None, false, false);
                generics.as_mut().unwrap().push(generic);

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

        // parse the parameters if any
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
            let parameter_type = self.parse_type(&None, false, false);
            parameters.push((parameter_name, parameter_type));
            if self.at(T![,]) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);

        // parse the return type if any
        let mut return_types: Option<Type> = None;
        if self.at(T![->]) {
            self.consume(T![->]);
            let typ = self.parse_type(&None, false, false);
            return_types = Some(typ);
        }

        assert!(self.at(T!['{']), "Expected a block after function header");
        self.consume(T!['{']);

        // parse the body of the function
        let mut body = Vec::new();
        while !self.at(T!['}']) {
            let stmt = match self.parse_statement() {
                Ok(stmt) => stmt,
                Err(found) => return Err(found),
            };
            body.push(Box::new(stmt));
        }
        self.consume(T!['}']);

        Ok(ast::Stmt::FunctionDeclaration {
            is_public,
            name: fn_name,
            generics,
            parameters: if parameters.len() > 0 { Some(parameters) } else { None },
            body,
            return_type: return_types,
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

        let interface_type = self.parse_type(&Some(interface_name.clone()), false, true);

        let mut methods = Vec::new();

        self.consume(T!['{']);
        while !self.at(T!['}']) {
            let fn_signature = self.parse_fn_signature();

            if !self.at(T![;]) {
                let line_col = self.line_column();
                return Err(ParseError::InvalidExpressionStatement {
                    line: line_col.0,
                    column: line_col.1,
                });
            }
            self.consume(T![;]);

            match fn_signature {
                Ok(signature) => {
                    methods.push(Box::new(signature));
                }
                Err(_) => {
                    let line_col = self.line_column();
                    return Err(ParseError::InvalidExpressionStatement {
                        line: line_col.0,
                        column: line_col.1,
                    });
                }
            }
        }

        self.consume(T!['}']);

        Ok(ast::Stmt::Interface {
            name: interface_name,
            type_: interface_type,
            methods,
        })
    }

    fn parse_struct(&mut self, is_public: bool) -> Result<ast::Stmt> {
        self.consume(T![struct]);

        let ident = self.next().expect("Expected identifier after `struct`");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier after `struct`, but found `{}`",
            ident.kind
        );

        let struct_name = self.text(ident).to_string();
        let struct_type = self.parse_type(&Some(struct_name.clone()), true, false);

        let mut members = Vec::new();

        self.consume(T!['{']);
        while !self.at(T!['}']) {
            let mut is_public: bool = false;
            if self.at(T![pub]) {
                self.consume(T![pub]);
                is_public = true;
            }

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
            let member_type = self.parse_type(&None, false, false);
            members.push(Box::new(ast::Expr::StructMember {
                is_public,
                name: member_name,
                type_: member_type,
            }));
        }

        self.consume(T!['}']);

        Ok(ast::Stmt::StructDeclaration {
            is_public,
            name: struct_name,
            type_: struct_type,
            members,
        })
    }

    fn parse_enum(&mut self, is_public: bool) -> Result<ast::Stmt> {
        self.consume(T![enum]);

        let ident = self.next().expect("Expected identifier after `enum`");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier after `enum`, but found `{}`",
            ident.kind
        );

        let name = self.text(ident).to_string();
        let mut members = Vec::new();
        self.consume(T!['{']);
        while !self.at(T!['}']) {
            let member_ident = self
                .next()
                .expect("Tried to parse enum member, but there were no more tokens");
            assert_eq!(
                member_ident.kind,
                T![ident],
                "Expected identifier as enum member, but found `{}`",
                member_ident.kind
            );
            let member_name = self.text(member_ident).to_string();
            members.push(member_name);

            if self.at(T![,]) {
                self.consume(T![,]);
            }
        }
        self.consume(T!['}']);

        Ok(ast::Stmt::Enum {
            is_public,
            name: name.clone(),
            members,
            type_: ast::Type::Enum,
        })
    }

    fn parse_type(&mut self, opt_name: &Option<String>, is_struct: bool, is_interface: bool) -> ast::Type {
        let mut type_name: Option<String> = opt_name.to_owned();
        if type_name.is_none() {
            let ident = self.next().expect("Tried to parse type, but there were no more tokens");

            assert_eq!(
                ident.kind,
                T![ident],
                "Expected identifier as type, but found `{}`",
                ident.kind
            );

            let val = self.text(ident).to_string();
            type_name = Some(val);
        }

        // check if the type is a generic type
        let mut generics = Vec::new();
        if self.at(T![<]) {
            self.consume(T![<]);
            loop {
                let generic = self.parse_type(&None, is_struct, is_interface);
                generics.push(generic);

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
        // check if the type is an array
        } else if self.at(T!['[']) {
            // array declaration
            self.consume(T!['[']);
            self.consume(T![']']);
            // we don't know the type yet. It depends on the elements
            return ast::Type::Array(Box::new(ast::Type::Unknown), 0);
        }
        // just a name of a type - probably coming from a struct or elsewhere
        if is_struct {
            return ast::Type::Struct {
                name: type_name.clone().unwrap(),
            };
        }
        if is_interface {
            return ast::Type::Interface {
                name: type_name.clone().unwrap(),
            };
        }

        // This is the case where I know the name of the type (like an interface, enum, struct, etc)
        // but I can't know that it's of
        match ast::Type::from_string_to_type(type_name.clone().unwrap()) {
            ast::Type::Unknown => {
                return ast::Type::Custom {
                    name: type_name.clone().unwrap(),
                }
            }
            typ => {
                return typ;
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
        // CHECK: why not a parse_statement here?
        let value = self.parse_expression();

        self.consume(T![;]);

        Ok(ast::Stmt::Let {
            name,
            type_: ast::Type::Unknown,
            expr: Box::new(value.unwrap()),
        })
    }

    fn parse_return(&mut self) -> Result<ast::Stmt> {
        self.consume(T![return]);

        // multiple statements need to be parsed?
        let mut multiple_stmts = Vec::new();
        if self.at(T!['(']) {
            self.consume(T!['(']);
            while !self.at(T![')']) {
                let stmt = self.parse_expression();
                multiple_stmts.push(Box::new(stmt?));
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T![')']);
        } else {
            let value = self.parse_expression();
            multiple_stmts.push(Box::new(value?));
        }

        self.consume(T![;]);

        Ok(ast::Stmt::Return { exprs: multiple_stmts })
    }

    fn parse_if(&mut self) -> Result<ast::Stmt> {
        self.consume(T![if]);
        self.consume(T!['(']);
        let condition = self.parse_expression();
        self.consume(T![')']);

        assert!(self.at(T!['{']), "Expected a block after `if` statement");
        let body = self.parse_statement();
        let body = match body {
            Ok(ast::Stmt::Block { stmts }) => stmts,
            _ => unreachable!(),
        };

        let mut else_stmt_empty = false;
        let else_stmt = if self.at(T![else]) {
            self.consume(T![else]);
            self.parse_statement()
        } else {
            else_stmt_empty = true;
            Ok(ast::Stmt::Empty)
        };

        if else_stmt_empty {
            return Ok(ast::Stmt::IfStmt {
                condition: Box::new(condition?),
                body,
                else_stmt: None,
            });
        }

        Ok(ast::Stmt::IfStmt {
            condition: Box::new(condition?),
            body,
            else_stmt: Some(Box::new(else_stmt?)),
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
        let expr = self.parse_expression();
        let range = Box::new(expr?);

        assert!(self.at(T!['{']), "Expected a block after `for` statement");
        self.consume(T!['{']);

        let mut body = Vec::new();
        while !self.at(T!['}']) {
            let stmt = self.parse_statement();
            body.push(Box::new(stmt?));
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
            let stmt = self.parse_statement();
            match stmt {
                Ok(stmt) => stmts.push(Box::new(stmt)),
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
            let arg = self.parse_expression();
            args.push(Box::new(arg?));
            if self.at(T![,]) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);
        Ok(ast::Stmt::Expression(Box::new(ast::Expr::FunctionCall { name, args })))
    }

    fn parse_assignment(&mut self, name: String) -> Result<ast::Stmt> {
        self.consume(T![=]);

        let value = self.parse_expression();

        self.consume(T![;]);

        Ok(ast::Stmt::Expression(Box::new(ast::Expr::Assignment {
            name,
            type_: Box::new(ast::Type::Unknown),
            value: Box::new(value?),
        })))
    }

    fn parse_dot(&mut self, struct_name: String) -> Result<ast::Stmt> {
        self.consume(T![.]);
        let expr = self.parse_expression();
        Ok(ast::Stmt::Expression(Box::new(ast::Expr::StructAccess {
            name: struct_name,
            field: Box::new(expr?),
        })))
    }

    fn parse_struct_instatiation(&mut self, struct_name: String) -> Result<ast::Stmt> {
        self.consume(T!['{']);
        let mut members = Vec::new();
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
            self.consume(T![:]);
            let member_value = self.parse_expression();
            members.push((member_name, Box::new(member_value?)));
            if self.at(T![,]) {
                self.consume(T![,]);
            }
        }
        self.consume(T!['}']);
        Ok(ast::Stmt::Expression(Box::new(ast::Expr::StructInstantiation {
            name: struct_name,
            members,
        })))
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
            T![,] => {
                self.consume(T![,]);
                Ok(ast::Stmt::Expression(Box::new(ast::Expr::Variable(name.clone()))))
            }
            T![;] => {
                // We don't consyme the semicolon here, because it's consumed in the caller function
                Ok(ast::Stmt::Expression(Box::new(ast::Expr::Variable(name.clone()))))
            }
            T!['('] => {
                let fn_call = self.parse_fn_call(name);
                Ok(fn_call?)
            }
            T!['{'] => {
                let struct_inst = self.parse_struct_instatiation(name);
                Ok(struct_inst?)
            }
            T![+] | T![-] | T![*] | T![/] | T![^] | T![!] => {
                let expr = self.parse_expression();
                Ok(ast::Stmt::Expression(Box::new(expr?)))
            }
            found => {
                let line_col = self.line_column();
                return Err(ParseError::UnexpectedToken {
                    found,
                    expected: vec![T![=], T![.], T![,], T!['(']],
                    line: line_col.0,
                    column: line_col.1,
                });
            }
        }
    }

    fn parse_impl(&mut self) -> Result<ast::Stmt> {
        self.consume(T![impl]);

        let ident = self.next().expect("Expected identifier after `impl`");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier after `impl`, but found `{}`",
            ident.kind
        );

        let name = self.text(ident).to_string();

        let mut generics = None;
        if self.at(T![<]) {
            self.consume(T![<]);
            generics = Some(Vec::new());
            loop {
                let generic = self.parse_type(&None, false, false);
                generics.as_mut().unwrap().push(generic);

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

        // with (Writer<T, Foo<E>>, Test)
        let mut interfaces: Option<Vec<Type>> = None;
        if self.at(T![with]) {
            self.consume(T![with]);

            interfaces = Some(Vec::new());

            let mut multiple_types: bool = false;
            if self.at(T!['(']) {
                self.consume(T!['(']);
                multiple_types = true;
            }

            interfaces.as_mut().unwrap().push(self.parse_type(&None, false, true));

            if self.at(T![,]) {
                while self.at(T![,]) {
                    self.consume(T![,]);
                    interfaces.as_mut().unwrap().push(self.parse_type(&None, false, true));
                }
            }

            if multiple_types {
                if !self.at(T![')']) {
                    let line_col = self.line_column();
                    return Err(ParseError::UnexpectedToken {
                        found: self.peek(),
                        expected: vec![T![')']],
                        line: line_col.0,
                        column: line_col.1,
                    });
                } else {
                    self.consume(T![')']);
                }
            }
        }

        let mut methods = Vec::new();
        self.consume(T!['{']);
        while !self.at(T!['}']) {
            let mut is_public = false;
            if self.at(T![pub]) {
                self.consume(T![pub]);
                is_public = true;
            }
            let fn_ = self.parse_fn(is_public);
            methods.push(Box::new(fn_?));
        }
        self.consume(T!['}']);

        Ok(ast::Stmt::ImplDeclaration {
            name,
            generics,
            interfaces,
            methods,
        })
    }

    fn parse_const(&mut self) -> Result<ast::Stmt> {
        let ident = self.next().expect("Expected identifier after `const`");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier after `const`, but found `{}`",
            ident.kind
        );
        let name = self.text(ident).to_string();

        self.consume(T![:]);

        let type_ = self.parse_type(&None, false, false);

        self.consume(T![=]);
        let value = self.parse_expression();
        self.consume(T![;]);

        Ok(ast::Stmt::Constant {
            is_public: false, // TODO: add public constants
            name,
            type_,
            value: Box::new(value?),
        })
    }

    fn parse_constant_group(&mut self) -> Result<ast::Stmt> {
        let mut constants = Vec::new();

        self.consume(T![const]);

        // multiple constants grouped together
        let next = self.peek();
        if next == T!['('] {
            self.consume(T!['(']);
            while !self.at(T![')']) {
                let c = self.parse_const();
                match c {
                    Ok(c) => constants.push(Box::new(c)),
                    Err(_) => {
                        let line_col = self.line_column();
                        return Err(ParseError::InvalidExpressionStatement {
                            line: line_col.0,
                            column: line_col.1,
                        });
                    }
                }
            }
            self.consume(T![')']);
        } else {
            // single constant definition
            let c = self.parse_const();
            match c {
                Ok(c) => constants.push(Box::new(c)),
                Err(_) => {
                    let line_col = self.line_column();
                    return Err(ParseError::InvalidExpressionStatement {
                        line: line_col.0,
                        column: line_col.1,
                    });
                }
            }
        }

        Ok(ast::Stmt::ConstantGroup { constants })
    }

    fn parse_statement(&mut self) -> Result<ast::Stmt> {
        let next = self.peek();
        let ast = match next {
            T![+] | T![-] | T![*] | T![/] | T![^] | T![!] => {
                let expr = self.parse_expression();
                Ok(ast::Stmt::Expression(Box::new(expr?)))
            }
            T![int] | T![float] | T![string] | T![bool] | T![char] => {
                let lit = self.parse_expression();
                Ok(ast::Stmt::Expression(Box::new(lit?)))
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
                let expr = self.parse_expression();
                match expr {
                    Ok(expr) => {
                        return Ok(ast::Stmt::Defer { expr: Box::new(expr) });
                    }
                    Err(_) => {
                        let line_col = self.line_column();
                        return Err(ParseError::InvalidExpressionStatement {
                            line: line_col.0,
                            column: line_col.1,
                        });
                    }
                }
            }
            T![nil] => {
                self.consume(T![nil]);
                Ok(ast::Stmt::Expression(Box::new(ast::Expr::Nil)))
            }
            found => {
                let line_col = self.line_column();
                return Err(ParseError::UnexpectedToken {
                    found,
                    expected: vec![T![let], T![ident], T![return], T![if], T!['{'], T![for], T![defer]],
                    line: line_col.0,
                    column: line_col.1,
                });
            }
        };
        ast
    }

    pub fn parse_input(&mut self, filename: &str) -> Result<ast::SourceFile> {
        let mut pkg_counter = 0;
        let mut is_pkg_first_defined = false;
        let mut stmts = Vec::new();
        loop {
            let next = self.peek();
            // Background: package statement must be the first statement in the file
            if self.line_column() == (1, 1) && !is_pkg_first_defined {
                is_pkg_first_defined = true;
                if next != T![package] {
                    return Err(ParseError::MissingPackageStatement);
                }
            }

            match next {
                T![package] => {
                    let stmt = self.parse_package();
                    if stmt.is_err() {
                        return Err(ParseError::MissingPackageStatement);
                    }
                    stmts.push(Box::new(stmt?));

                    pkg_counter += 1;
                    if pkg_counter > 1 {
                        panic!("Only one package statement is allowed per file");
                    }
                }
                T![use] => {
                    let stmt = self.parse_use();
                    if stmt.is_err() {
                        let line_col = self.line_column();
                        return Err(ParseError::InvalidExpressionStatement {
                            line: line_col.0,
                            column: line_col.1,
                        });
                    }
                    stmts.push(Box::new(stmt?));
                }
                T![pub] => {
                    self.consume(T![pub]);
                    match self.peek() {
                        T![fn] => {
                            let stmt = self.parse_fn(true);
                            stmts.push(Box::new(stmt?));
                        }
                        T![struct] => {
                            let stmt = self.parse_struct(true);
                            stmts.push(Box::new(stmt?));
                        }
                        T![enum] => {
                            let stmt = self.parse_enum(true);
                            stmts.push(Box::new(stmt?));
                        }
                        found => {
                            let line_col = self.line_column();
                            return Err(ParseError::UnexpectedToken {
                                found,
                                expected: vec![T![fn], T![struct], T![enum]],
                                line: line_col.0,
                                column: line_col.1,
                            });
                        }
                    }
                }
                T![fn] => {
                    let stmt = self.parse_fn(false);
                    stmts.push(Box::new(stmt?));
                }
                T![struct] => {
                    let stmt = self.parse_struct(false);
                    stmts.push(Box::new(stmt?));
                }
                T![enum] => {
                    let stmt = self.parse_enum(false);
                    stmts.push(Box::new(stmt?));
                }
                T![interface] => {
                    let stmt = self.parse_interface();
                    stmts.push(Box::new(stmt?));
                }
                T![impl] => {
                    let stmt = self.parse_impl();
                    stmts.push(Box::new(stmt?));
                }
                T![const] => {
                    let stmt = self.parse_constant_group();
                    stmts.push(Box::new(stmt?));
                }
                T![EOF] => break,
                _ => match self.parse_statement() {
                    Ok(stmt) => {
                        stmts.push(Box::new(stmt));
                    }
                    Err(e) => {
                        return Err(e);
                    }
                },
            }
        }

        if pkg_counter == 0 {
            return Err(ParseError::MissingPackageStatement);
        }

        Ok(ast::SourceFile {
            name: filename.to_string(),
            statements: stmts,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::{rc::Rc, sync::Arc, vec};

    use crate::{
        hashmap,
        lexer::{Lexer, TokenKind},
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
    fn test(var: Type, var2_: bool) -> bool {
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
                T![:],
                T![ident],
                T![,],
                T![ident],
                T![:],
                T![ident],
                T![')'],
                T![->],
                T![ident],
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
                T![int],
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
            parser.parse_statement().unwrap()
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
        match let_stmt.as_ref() {
            ast::Stmt::Let { name, .. } => assert_eq!(name, "x"),
            _ => unreachable!(),
        }

        let stmts = match stmts[1].as_ref() {
            ast::Stmt::Block { stmts } => stmts,
            _ => unreachable!(),
        };
        assert_eq!(stmts.len(), 2);

        let assignment_stmt = &stmts[0];
        match assignment_stmt.as_ref() {
            ast::Stmt::Expression(expr) => match expr.as_ref() {
                ast::Expr::Assignment { name, .. } => assert_eq!(name, "x"),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }

        let if_stmt = &stmts[1];
        match if_stmt.as_ref() {
            ast::Stmt::IfStmt {
                condition,
                body,
                else_stmt,
            } => {
                // assert!(matches!(
                //     condition,
                //     Box::new(ast::Expr::BinaryOp {
                //         op: T![<],
                //         lhs: _lhs,
                //         rhs: _rhs,
                //     })
                // ));

                assert_eq!(body.len(), 2);
                let x_assignment = &body[0];
                match x_assignment.as_ref() {
                    ast::Stmt::Expression(expr) => match expr.as_ref() {
                        ast::Expr::Assignment { name, .. } => assert_eq!(name, "x"),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
                let y_assignment = &body[1];
                match y_assignment.as_ref() {
                    ast::Stmt::Expression(expr) => match expr.as_ref() {
                        ast::Expr::Assignment { name, .. } => assert_eq!(name, "y"),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }

                let else_stmt = match else_stmt {
                    Some(stmt) => stmt,
                    None => unreachable!(),
                };

                match else_stmt.as_ref() {
                    ast::Stmt::IfStmt {
                        condition,
                        body,
                        else_stmt,
                    } => {
                        // assert!(matches!(
                        //     condition,
                        //     Box::new(ast::Expr::BinaryOp {
                        //         op: T![<],
                        //         lhs: _lhs,
                        //         rhs: _rhs,
                        //     })
                        // ));
                        assert_eq!(body.len(), 2);
                        let let_i = &body[0];
                        match let_i.as_ref() {
                            ast::Stmt::Let { name, .. } => assert_eq!(name, "i"),
                            _ => unreachable!(),
                        }
                        let x_assignment = &body[1];
                        match x_assignment.as_ref() {
                            ast::Stmt::Expression(expr) => match expr.as_ref() {
                                ast::Expr::Assignment { name, .. } => assert_eq!(name, "x"),
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        }

                        let else_stmt = match else_stmt {
                            Some(stmt) => stmt,
                            None => unreachable!(),
                        };

                        let stmts = match else_stmt.as_ref() {
                            ast::Stmt::Block { stmts } => stmts,
                            _ => unreachable!(),
                        };
                        assert_eq!(stmts.len(), 1);

                        let x_assignment = &stmts[0];
                        match x_assignment.as_ref() {
                            ast::Stmt::Expression(expr) => match expr.as_ref() {
                                ast::Expr::Assignment { name, .. } => assert_eq!(name, "x"),
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                };
            }
            _ => unreachable!(),
        }
    }

    //    #[test]
    //     fn parse_range_statement() {
    //         fn parse(input: &str) -> ast::Stmt {
    //             let mut parser = Parser::new(input);
    //             parser.parse_statement().unwrap()
    //         }

    //         let stmt = parse(
    //             unindent(
    //                 r#"
    //     let iter = {0, 1, 2};
    //     for x range iter {
    //         x = x + 1;
    //     }
    // "#,
    //             )
    //             .as_str(),
    //         );

    //         match stmt {
    //             ast::Stmt::RangeStmt { iterator, range, body } => {
    //                 assert_eq!(iterator, "x");
    //                 assert_eq!(
    //                     range,
    //                     Rc::new(Arc::new(ast::Stmt::ArrayInitialization {
    //                         elements: vec![
    //                             Rc::new(Arc::new(ast::Stmt::Literal(ast::Expr::Int(0)))),
    //                             Rc::new(Arc::new(ast::Stmt::Literal(ast::Expr::Int(1)))),
    //                             Rc::new(Arc::new(ast::Stmt::Literal(ast::Expr::Int(2)))),
    //                         ]
    //                     }))
    //                 );
    //                 assert_eq!(body.len(), 1);
    //             }
    //             _ => unreachable!(),
    //         }
    //     }

    #[test]
    fn parse_function() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.parse_fn(false).unwrap()
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
            ast::Stmt::FunctionDeclaration {
                is_public: false,
                name,
                generics,
                parameters,
                body,
                return_type: _,
            } => {
                assert_eq!(name, "wow_we_did_it");
                assert_eq!(generics, None);
                assert_eq!(parameters.clone().unwrap().len(), 2);
                let (bar, bar_type) = &parameters.unwrap()[1];
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
            parser.parse_fn(false).unwrap()
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
            ast::Stmt::FunctionDeclaration {
                is_public: false,
                name,
                generics,
                parameters,
                body,
                return_type,
            } => {
                assert_eq!(name, "wow_we_did_it");
                assert_eq!(generics, None);
                assert_eq!(parameters.clone().unwrap().len(), 2);

                let (bar, bar_type) = &parameters.unwrap()[1];
                assert_eq!(bar, "bar");
                assert_eq!(
                    bar_type,
                    &ast::Type::Custom {
                        name: "Bar".to_string(),
                    }
                );

                assert_eq!(body.len(), 3);
                assert_eq!(return_type, Some(ast::Type::String));
            }
            _ => unreachable!(),
        };
    }

    #[test]
    fn parse_struct() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.parse_struct(false).unwrap()
        }

        let item = parse(
            unindent(
                r#"
        struct Foo<T, U> {
            x String
            bar Bar<Baz<T>, U>
        }
    "#,
            )
            .as_str(),
        );

        match item {
            ast::Stmt::StructDeclaration {
                is_public,
                name,
                members,
                type_,
            } => {
                assert_eq!(is_public, false);
                assert_eq!(name, "Foo");
                assert_eq!(
                    type_,
                    ast::Type::Custom {
                        name: "Foo".to_string(),
                    }
                );
                assert_eq!(members.len(), 2);
                let member = &members[0];

                match member.as_ref() {
                    ast::Expr::StructMember { is_public, name, type_ } => {
                        assert_eq!(*is_public, false);
                        assert_eq!(name, "x");
                        assert_eq!(type_, &ast::Type::String);
                    }
                    _ => unreachable!(),
                }

                let member2 = &members[1];
                match member2.as_ref() {
                    ast::Expr::StructMember { is_public, name, type_ } => {
                        assert_eq!(*is_public, false);
                        assert_eq!(name, "bar");
                        assert_eq!(
                            type_,
                            &ast::Type::Custom {
                                name: "Bar".to_string(),
                            }
                        );
                    }
                    _ => unreachable!(),
                }
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

        struct Foo {
            x string
            bar bool
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
                    match *stmt {
                        ast::Stmt::FunctionDeclaration {
                            is_public,
                            name,
                            generics,
                            parameters,
                            body,
                            return_type,
                        } => {
                            assert_eq!(is_public, false);
                            assert_eq!(name, "wow_we_did_it");
                            assert_eq!(generics, None);
                            assert_eq!(parameters.clone().unwrap().len(), 2);
                            assert_eq!(body.len(), 2);
                            assert_eq!(return_type, None);
                        }
                        ast::Stmt::StructDeclaration {
                            is_public,
                            name,
                            type_,
                            members,
                        } => {
                            assert_eq!(is_public, false);
                            assert_eq!(name, "Foo");
                            assert_eq!(members.len(), 2);
                            assert_eq!(
                                type_,
                                ast::Type::Custom {
                                    name: "Foo".to_string(),
                                }
                            );
                        }
                        ast::Stmt::PackageDeclaration(name) => {
                            assert_eq!(name, "main");
                        }
                        _ => unreachable!(),
                    }
                }
            }
        };
    }

    #[test]
    fn parse_statement_with_assignment_and_calculation() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.parse_statement().unwrap()
        }

        let expr = parse("let k = x * 4 + -2! * 3 + z;");
        assert_eq!(
            expr,
            ast::Stmt::Let {
                name: "k".to_string(),
                type_: ast::Type::Unknown,
                expr: Box::new(ast::Expr::BinaryOp {
                    op: TokenKind::Plus,
                    lhs: Box::new(ast::Expr::BinaryOp {
                        op: TokenKind::Plus,
                        lhs: Box::new(ast::Expr::BinaryOp {
                            op: TokenKind::Star,
                            lhs: Box::new(ast::Expr::Variable("x".to_string())),
                            rhs: Box::new(ast::Expr::IntegerLiteral(4))
                        }),
                        rhs: Box::new(ast::Expr::BinaryOp {
                            op: TokenKind::Star,
                            lhs: Box::new(ast::Expr::PrefixOp {
                                op: TokenKind::Minus,
                                expr: Box::new(ast::Expr::PostfixOp {
                                    op: TokenKind::Bang,
                                    expr: Box::new(ast::Expr::IntegerLiteral(2))
                                })
                            }),
                            rhs: Box::new(ast::Expr::IntegerLiteral(3))
                        })
                    }),
                    rhs: Box::new(ast::Expr::Variable("z".to_string()))
                })
            }
        );
    }

    #[test]
    fn parse_return_statement() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.parse_statement().unwrap()
        }

        let stmt = parse("return 7 + 3;");
        assert_eq!(
            stmt,
            ast::Stmt::Return {
                exprs: vec![Box::new(ast::Expr::BinaryOp {
                    op: TokenKind::Plus,
                    lhs: Box::new(ast::Expr::IntegerLiteral(7)),
                    rhs: Box::new(ast::Expr::IntegerLiteral(3))
                })]
            }
        );
    }

    #[test]
    fn parse_enum_statement() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.parse_enum(false).unwrap()
        }

        let stmt = parse(
            unindent(
                r#"
                enum Foo {
                    Bar,
                    Baz,
                    Qux
                }"#,
            )
            .as_str(),
        );

        assert_eq!(
            stmt,
            ast::Stmt::Enum {
                is_public: false,
                type_: ast::Type::Custom {
                    name: "Foo".to_string(),
                },
                name: "Foo".to_string(),
                members: vec!["Bar".to_string(), "Baz".to_string(), "Qux".to_string()]
            }
        );
    }

    #[test]
    fn parse_interface_statement() {
        fn parse(input: &str) -> ast::Stmt {
            let mut parser = Parser::new(input);
            parser.parse_interface().unwrap()
        }

        let stmt = parse(
            unindent(
                r#"
                interface Foo {
                    bar(x: int) -> int;
                    baz(y: string) -> string;
                }"#,
            )
            .as_str(),
        );

        assert_eq!(
            stmt,
            ast::Stmt::Interface {
                name: "Foo".to_string(),
                type_: ast::Type::Custom {
                    name: "Foo".to_string(),
                },
                methods: vec![
                    Box::new(ast::Stmt::InterfaceFunctionSignature {
                        name: "bar".to_string(),
                        generics: None,
                        parameters: Some(hashmap!["x".to_string() => ast::Type::Int]),
                        return_type: Some(ast::Type::Int)
                    }),
                    Box::new(ast::Stmt::InterfaceFunctionSignature {
                        name: "baz".to_string(),
                        generics: None,
                        parameters: Some(hashmap!["y".to_string() => ast::Type::String]),
                        return_type: Some(ast::Type::String)
                    })
                ]
            }
        );
    }
}
