mod sym;
mod typ;

use core::fmt;
use std::borrow::BorrowMut;

use typ::TypeChecker;
use uuid::Uuid;

use sym::{Context, GlobalSymbolTable, Symbol, SymbolTable};

use crate::parser::ast::{self, Expr, Stmt, AST};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    pub global_symbol_table: GlobalSymbolTable,
    pub forward_references_symbol_table: SymbolTable,
}

impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer {
            global_symbol_table: GlobalSymbolTable::new(),
            forward_references_symbol_table: {
                let name = "forward_references".to_string();
                SymbolTable::new(name.clone())
            },
        }
    }

    // first pass to collect as many symbols as possible
    pub fn analyze(&mut self, ast: AST) -> Result<(), Vec<String>> {
        let mut errors: Vec<String> = Vec::new();
        let mut context = Context::new();

        let files = ast.files.clone();
        files.iter().all(|sourcefile| {
            println!("Analyzing file: {}", sourcefile.name);

            for stmt in &sourcefile.statements {
                let pkg_decl = stmt.is_package_declaration();
                if pkg_decl.0 {
                    let pkg_name = format!("pkg.{}", pkg_decl.1);
                    context.enter_scope(pkg_name.clone());

                    let current_package_symbol_table = self.global_symbol_table.get_package_symbol_table(&pkg_name);

                    if let Some(current_table) = context.current_scope_mut() {
                        *current_table = current_package_symbol_table.clone();
                    }
                }

                if let Err(e) = self.process_stmt(context.borrow_mut(), *stmt.clone()) {
                    errors.push(e);
                }
            }

            if let Some(pkg_name) = context.current_scope().map(|table| table.name.clone()) {
                if let Some(exited_table) = context.exit_scope() {
                    self.global_symbol_table.packages.insert(pkg_name, exited_table);
                }
            }

            true
        });

        println!("First pass (symbols collection) - completed");
        match self.forward_references_pass() {
            Ok(_) => {}
            Err(mut es) => {
                errors.append(&mut es);
            }
        }

        println!("Second pass (forward references) - completed");

        let mut type_checker = TypeChecker::new();
        match type_checker.type_check_pass(ast, &context) {
            Ok(_) => {}
            Err(mut es) => {
                errors.append(&mut es);
            }
        }

        println!("Third pass (type checking) - completed");

        if errors.len() > 0 {
            return Err(errors);
        }

        Ok(())
    }

    fn process_stmt(&mut self, context: &mut Context, stmt: Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Expression(expr) => self.collect_expr_symbols(context, *expr),
            Stmt::Defer { expr } => self.collect_expr_symbols(context, *expr),
            Stmt::Constant {
                is_public,
                name,
                type_,
                value,
            } => {
                let constant_symbol = format!("const.{}", name);
                if context.lookup(&constant_symbol).is_some() {
                    Err(format!("error: constant `{}` already declared", name))?
                } else {
                    // At this point, the immutable borrow has ended
                    if let Some(symbol_table) = context.current_scope_mut() {
                        symbol_table.add_symbol(
                            constant_symbol.clone(),
                            Symbol::Constant {
                                name: name.clone(),
                                type_: type_.clone(),
                                value: String::new(), // TODO: how do I save the value???
                            },
                        );
                    }
                    Ok(())
                }
            }
            Stmt::Interface { name, type_, methods } => {
                let interface_symbol = format!("interface.{}", name);
                // Check if the interface symbol already exists
                if context.lookup(&interface_symbol).is_some() {
                    return Err(format!("error: interface `{}` already declared", name));
                }

                context.enter_scope(name.clone());
                for method in methods {
                    self.process_stmt(context, *method)?;
                }
                let interface_table = context.current_scope().cloned().unwrap();
                context.exit_scope();

                // Now we can safely take a mutable borrow
                if let Some(symbol_table) = context.current_scope_mut() {
                    symbol_table.add_symbol(
                        name.clone(),
                        Symbol::Interface {
                            name: name.clone(),
                            type_: type_.clone(),
                            table: interface_table,
                        },
                    );
                }

                Ok(())
            }
            Stmt::InterfaceFunctionSignature {
                name,
                generics,
                parameters,
                return_type,
            } => {
                if context.lookup(&name).is_some() {
                    return Err(format!("error: method `{}` in interface already declared", name));
                }
                if let Some(symbol_table) = context.current_scope_mut() {
                    let mut params: Vec<Symbol> = Vec::new();
                    if parameters.is_some() {
                        for (name, typ) in parameters.unwrap() {
                            params.push(Symbol::Identifier {
                                name: name.clone(),
                                type_: typ.clone(),
                                value: String::new(),
                            })
                        }
                    }

                    symbol_table.add_symbol(
                        name.clone(),
                        Symbol::InterfaceMethod {
                            name: name.clone(),
                            parameters: if params.len() > 0 { Some(params) } else { None },
                            return_type: return_type.clone(),
                        },
                    );
                }
                Ok(())
            }
            Stmt::Let {
                name,
                type_,
                expr: statement,
            } => {
                let let_symbol = format!("let.{}", name);
                if context.lookup_current_scope(&let_symbol).is_some() {
                    return Err(format!("error: identifier `{}` already declared", name));
                }
                if let Some(symbol_table) = context.current_scope_mut() {
                    symbol_table.add_symbol(
                        let_symbol.clone(),
                        Symbol::Identifier {
                            name: name.clone(),
                            type_: type_.clone(),
                            value: String::new(),
                        },
                    );
                }
                // Process the statement's expression
                self.collect_expr_symbols(context, *statement)
            }
            Stmt::IfStmt {
                condition,
                body,
                else_stmt,
            } => self.process_if_stmt(context, condition, body, else_stmt),
            Stmt::RangeStmt { iterator, range, body } => self.process_range_stmt(context, range, iterator, body),
            Stmt::WhileStmt { condition, body } => self.process_while_stmt(context, condition, body),
            Stmt::Block { stmts } => {
                let block_id = format!("block.{}", Uuid::new_v4().to_string());
                // Create a new block symbol table with the parent set to the current symbol table
                context.enter_scope(block_id.clone());
                // Process each statement within the new block scope
                for stmt in stmts {
                    self.process_stmt(context, *stmt)?;
                }
                let block_symbol_table = context.current_scope().cloned();
                context.exit_scope();

                if let Some(symbol_table) = context.current_scope_mut() {
                    // Now, we can safely borrow the parent symbol table mutably and add the new block symbol table
                    symbol_table.add_symbol(block_id.clone(), Symbol::Block(block_symbol_table.clone()));
                }
                Ok(())
            }
            Stmt::Return { exprs } => {
                for expr in exprs {
                    self.collect_expr_symbols(context, *expr)?;
                }
                Ok(())
            }
            Stmt::Enum {
                is_public,
                name,
                type_,
                members,
            } => self.process_enum_stmt(name, context, members, is_public, type_),
            Stmt::StructDeclaration {
                is_public,
                name,
                type_,
                members,
            } => self.process_structdecl_stmt(name, context, members, is_public, type_),
            Stmt::StructMember { is_public, name, type_ } => {
                if context.lookup(&name).is_some() {
                    return Err(format!("error: struct member `{}` already declared", name));
                }
                if let Some(symbol_table) = context.current_scope_mut() {
                    symbol_table.add_symbol(
                        name.clone(),
                        Symbol::StructMember {
                            is_public,
                            name: name.clone(),
                            type_: type_.clone(),
                        },
                    );
                }
                Ok(())
            }
            Stmt::FunctionDeclaration {
                is_public,
                name,
                generics,
                parameters,
                body,
                return_type,
            } => self.process_funcdeclr_stmt(name, context, parameters, body, is_public, return_type),
            Stmt::ImplDeclaration {
                name,
                generics,
                interfaces,
                methods,
            } => {
                let impl_symbol = format!("impl.{}", name);
                // Check if the impl symbol already exists
                if context.lookup(&impl_symbol).is_some() {
                    return Err(format!("error: impl `{}` already declared", name));
                }

                context.enter_scope(name.clone());
                for method in methods {
                    self.process_stmt(context, *method)?;
                }
                let impl_methods_table = context.current_scope().cloned().unwrap();
                context.exit_scope();

                if let Some(symbol_table) = context.current_scope_mut() {
                    symbol_table.add_symbol(
                        impl_symbol.clone(),
                        Symbol::Impl {
                            name: name.clone(),
                            type_: ast::Type::Custom {
                                name: name.clone(),
                                generics: if generics.is_some() {
                                    Some(generics.unwrap())
                                } else {
                                    None
                                },
                            },
                            interfaces: interfaces.clone(),
                            table: impl_methods_table,
                        },
                    );
                }

                Ok(())
            }
            Stmt::ConstantGroup { constants } => {
                for constant in constants {
                    self.process_stmt(context, *constant)?;
                }
                Ok(())
            }
            Stmt::UseDeclaration(name) => {
                let use_symbol = format!("use.{}", name);
                // Check if the use symbol already exists
                if context.lookup(&use_symbol).is_some() {
                    return Err(format!("error: use `{}` already declared", name))?;
                }
                if let Some(symbol_table) = context.current_scope_mut() {
                    symbol_table.add_symbol(use_symbol.clone(), Symbol::Use(name.clone()));
                }
                Ok(())
            }
            _ => Ok(()), // empty and package are here
        }
    }

    fn process_funcdeclr_stmt(
        &mut self,
        name: String,
        context: &mut Context,
        parameters: Option<Vec<(String, ast::Type)>>,
        body: Vec<Box<Stmt>>,
        is_public: bool,
        return_type: Option<Vec<ast::Type>>,
    ) -> Result<(), String> {
        let function_symbol = format!("fn.{}", name);

        // Check if the function symbol already exists
        if context.lookup(&function_symbol).is_some() {
            return Err(format!("error: function `{}` already declared", name));
        }

        context.enter_scope(name.clone());

        let mut parameters_symbol = Vec::new();
        if let Some(fn_body_table) = context.current_scope_mut() {
            if let Some(params) = parameters.clone() {
                for (name, type_) in &params {
                    let ident_symbol = format!("let.{}", name);
                    let ident = Symbol::Identifier {
                        name: name.clone(),
                        type_: type_.clone(),
                        value: String::new(), // Empty value for now
                    };
                    fn_body_table.add_symbol(ident_symbol.clone(), ident.clone());
                    parameters_symbol.push(ident);
                }
            }
        }

        // Process the function body
        for stmt in body {
            self.process_stmt(context, *stmt)?;
        }

        let fn_body_table = context.current_scope().cloned().unwrap();
        context.exit_scope();

        // Now add the function symbol to the parent symbol table
        if let Some(symbol_table) = context.current_scope_mut() {
            symbol_table.add_symbol(
                function_symbol.clone(),
                Symbol::Function {
                    is_public,
                    name: name.clone(),
                    parameters: if parameters_symbol.len() > 0 {
                        Some(parameters_symbol)
                    } else {
                        None
                    },
                    return_type: return_type.clone(),
                    body: fn_body_table,
                },
            );
        }

        Ok(())
    }

    fn process_structdecl_stmt(
        &mut self,
        name: String,
        context: &mut Context,
        members: Vec<Box<Stmt>>,
        is_public: bool,
        type_: ast::Type,
    ) -> Result<(), String> {
        let struct_symbol = format!("struct.{}", name);
        // Check if the struct symbol already exists
        if context.lookup(&struct_symbol).is_some() {
            return Err(format!("error: struct `{}` already declared", name));
        }

        context.enter_scope(name.clone());

        // Process each struct member
        for member in members {
            self.process_stmt(context, *member)?;
        }
        let struct_members_table = context.current_scope().cloned().unwrap();
        context.exit_scope();

        // Add the struct symbol to the parent symbol table
        if let Some(symbol_table) = context.current_scope_mut() {
            symbol_table.add_symbol(
                struct_symbol.clone(),
                Symbol::Struct {
                    is_public,
                    type_: type_.clone(),
                    name: name.clone(),
                    table: struct_members_table,
                },
            );
        }

        Ok(())
    }

    fn process_if_stmt(
        &mut self,
        context: &mut Context,
        condition: Box<Expr>,
        body: Vec<Box<Stmt>>,
        else_stmt: Option<Box<Stmt>>,
    ) -> Result<(), String> {
        let if_id: String = format!("if.{}", Uuid::new_v4().to_string());
        if context.lookup(&if_id).is_some() {
            Err(format!("error: symbol `{}` already declared", if_id))?;
        }

        self.collect_expr_symbols(context, *condition)?;

        context.enter_scope(if_id.clone());

        for stmt in body {
            self.process_stmt(context, *stmt)?;
        }

        let then_symbol_table = context.current_scope().cloned().unwrap();
        context.exit_scope();

        let mut else_symbol_table: Option<SymbolTable> = None;
        if let Some(else_stmt) = else_stmt {
            let else_id = format!("else.{}", Uuid::new_v4().to_string());
            context.enter_scope(else_id.clone());

            self.process_stmt(context, *else_stmt)?;

            else_symbol_table = context.current_scope().cloned();
            context.exit_scope();

            true
        } else {
            false
        };

        if let Some(symbol_table) = context.current_scope_mut() {
            symbol_table.add_symbol(
                if_id.clone(),
                Symbol::IfStatement {
                    condition: String::new(), // TODO: is this correct?
                    then_body: then_symbol_table,
                    else_body: else_symbol_table,
                },
            );
        }

        Ok(())
    }

    fn process_while_stmt(
        &mut self,
        context: &mut Context,
        condition: Box<Expr>,
        body: Vec<Box<Stmt>>,
    ) -> Result<(), String> {
        // TODO: does this need to go to the parent symbol table or
        // the one that is being created?
        self.collect_expr_symbols(context, *condition)?;

        let while_id = format!("while.{}", Uuid::new_v4().to_string());

        // Check if the while symbol already exists
        if context.lookup(&while_id).is_some() {
            return Err(format!("error: symbol `{}` already declared", while_id));
        }

        context.enter_scope(while_id.clone());

        // Process each statement in the while loop body
        for stmt in body {
            self.process_stmt(context, *stmt)?;
        }

        let while_symbol_table = context.current_scope().cloned().unwrap();
        context.exit_scope();

        if let Some(symbol_table) = context.current_scope_mut() {
            symbol_table.add_symbol(
                while_id.clone(),
                Symbol::WhileLoop {
                    condition: String::new(), // TODO: Adjust this as per your logic
                    body: while_symbol_table,
                },
            );
        }
        Ok(())
    }

    fn process_range_stmt(
        &mut self,
        context: &mut Context,
        range: Box<Expr>,
        iterator: String,
        body: Vec<Box<Stmt>>,
    ) -> Result<(), String> {
        // TODO: does this need to go to the parent symbol table or
        // the one that is being created?
        self.collect_expr_symbols(context, *range)?;

        let range_id = format!("range.{}", Uuid::new_v4().to_string());

        context.enter_scope(range_id.clone());

        let mut range_symbol_table = context.current_scope().cloned().unwrap();
        range_symbol_table.add_symbol(
            iterator.clone(),
            Symbol::Identifier {
                name: iterator.clone(),
                type_: ast::Type::Unknown,
                value: String::new(),
            },
        );

        // Process each statement in the range loop body
        for stmt in body {
            self.process_stmt(context, *stmt)?;
        }

        context.exit_scope();

        if let Some(symbol_table) = context.current_scope_mut() {
            symbol_table.add_symbol(
                range_id.clone(),
                Symbol::RangeLoop {
                    iterator,
                    iterable: String::new(), // TODO: Adjust this as per your logic
                    body: range_symbol_table,
                },
            );
        }

        Ok(())
    }

    fn process_enum_stmt(
        &self,
        name: String,
        context: &mut Context,
        members: Vec<String>,
        is_public: bool,
        type_: ast::Type,
    ) -> Result<(), String> {
        let enum_symbol = format!("enum.{}", name);

        // Borrow the symbol table immutably to check if the enum symbol already exists
        let symbol = context.lookup(&enum_symbol);

        match symbol {
            Some(_) => Err(format!("error: enum `{}` already declared", name))?,
            None => {
                context.enter_scope(name.clone());

                let mut enum_member_symbol_table = context.current_scope_mut().cloned().unwrap();
                // Add each enum member to the enum member symbol table
                for (idx, member) in members.iter().enumerate() {
                    // Check if the enum member is already declared in the enum member symbol table
                    if enum_member_symbol_table.lookup(&member).is_some() {
                        Err(format!("error: enum member `{}` already declared", member))?;
                    }
                    // Add the enum member to the enum member symbol table
                    enum_member_symbol_table.add_symbol(
                        member.clone(),
                        Symbol::EnumMember {
                            name: member.clone(),
                            type_: ast::Type::Int,
                            value: idx.to_string(),
                        },
                    );
                }
                context.exit_scope();

                if let Some(symbol_table) = context.current_scope_mut() {
                    symbol_table.add_symbol(
                        enum_symbol.clone(),
                        Symbol::Enum {
                            is_public,
                            type_: type_.clone(),
                            name: name.clone(),
                            table: enum_member_symbol_table,
                        },
                    );
                }

                Ok(())
            }
        }
    }

    fn collect_expr_symbols(&mut self, context: &mut Context, expr: Expr) -> Result<(), String> {
        match expr {
            Expr::Variable(name) => {
                let variable_symbol = format!("let.{}", name);
                // First, check if the symbol exists
                let symbol = context.lookup(&variable_symbol);
                // Add the symbol
                match symbol {
                    Some(_) => Ok(()),
                    None => {
                        if let Some(symbol_table) = context.current_scope_mut() {
                            symbol_table.add_symbol(
                                variable_symbol.clone(),
                                Symbol::Identifier {
                                    name: name.clone(),
                                    type_: ast::Type::Unknown,
                                    value: String::new(), // Placeholder for the value
                                },
                            );
                        }
                        Ok(())
                    }
                }
            }
            Expr::FunctionCall { name, args } => {
                let fn_call_symbol = format!("fn.{}", name);

                let symbol = context.lookup(&fn_call_symbol);

                if symbol.is_none() {
                    // If symbol does not exist, add it to forward_references_symbol_table
                    self.forward_references_symbol_table.add_symbol(
                        fn_call_symbol.clone(),
                        Symbol::FunctionCall {
                            name: name.clone(),
                            arguments: None, // TODO: this requires binding in the forward references pass
                        },
                    );
                }

                // collect expression symbols for each argument
                for arg in args {
                    self.collect_expr_symbols(context, *arg.clone())?;
                }
                Ok(())
            }
            Expr::ArrayInitialization { elements } => {
                for element in elements {
                    self.collect_expr_symbols(context, *element)?;
                }
                Ok(())
            }
            Expr::ArrayAccess { name, index } => {
                let array_symbol = format!("array.{}", name);

                // Borrow symbol_table immutably to check if the array symbol exists
                let symbol = context.lookup(&array_symbol);

                match symbol {
                    Some(_) => {
                        // If symbol exists, collect expression symbols for the index
                        self.collect_expr_symbols(context, *index)
                    }
                    None => {
                        // If symbol does not exist, return an error message
                        Err(format!(
                            "error: identifier `{}` for array access not declared",
                            array_symbol
                        ))
                    }
                }
            }
            Expr::StructAccess { name, field } => {
                // search in the global table if the struct is declared
                let struct_symbol = format!("struct.{}", name);
                // Immutable borrow to check if the struct symbol exists
                let symbol = context.lookup(&struct_symbol);
                match symbol {
                    Some(_) => {
                        // If symbol exists, collect expression symbols for the field
                        self.collect_expr_symbols(context, *field)
                    }
                    None => {
                        // If symbol does not exist, return an error message
                        Err(format!("error: struct `{}` not declared", name))
                    }
                }
            }
            Expr::PrefixOp { op, expr } => self.collect_expr_symbols(context, *expr),
            Expr::PostfixOp { op, expr } => self.collect_expr_symbols(context, *expr),
            Expr::BinaryOp { op, lhs, rhs } => {
                self.collect_expr_symbols(context, *lhs)?;
                self.collect_expr_symbols(context, *rhs)
            }
            Expr::Assignment { name, type_, value } => {
                let variable_symbol = format!("let.{}", name);
                // Immutable borrow to check if the variable symbol exists
                let symbol = context.lookup(&variable_symbol);
                match symbol {
                    Some(_) => {
                        // If symbol exists, collect expression symbols for the value
                        self.collect_expr_symbols(context, *value)
                    }
                    None => {
                        // If symbol does not exist, return an error message
                        Err(format!("error: identifier `{}` not declared", name))
                    }
                }
            }
            Expr::StructInstantiation { name, members } => {
                // search in the global table if the struct is declared
                let struct_symbol = format!("struct.{}", name);
                // Immutable borrow to check if the struct symbol exists
                let symbol = context.lookup(&struct_symbol);
                if symbol.is_none() {
                    // If symbol does not exist, add it to the forward references symbol table
                    self.forward_references_symbol_table.add_symbol(
                        struct_symbol.clone(),
                        Symbol::Struct {
                            is_public: false,
                            name: name.clone(),
                            type_: ast::Type::Custom {
                                name: name.clone(),
                                generics: None,
                            },
                            // TODO: do I need a new context here???
                            table: {
                                let name = name.clone();
                                SymbolTable::new(name.clone())
                            },
                        },
                    );
                }
                // collect expression symbols for each member
                for (_, member) in members {
                    self.collect_expr_symbols(context, *member.clone())?;
                }
                Ok(())
            }
            _ => Ok(()), // these are all the types like int, float, string, etc.
        }
    }

    // forward references pass to check if all symbols are defined
    pub fn forward_references_pass(&mut self) -> Result<(), Vec<String>> {
        let result: Vec<String> = self
            .forward_references_symbol_table
            .get_symbols()
            .iter()
            .map(|(symbol_name, _)| {
                let found = self.global_symbol_table.lookup_symbol(&symbol_name);
                match found {
                    Some(_) => "".to_string(),
                    None => format!("error: symbol `{}` undefined", symbol_name),
                }
            })
            .filter(|s| s.len() > 0)
            .collect();

        if result.len() > 0 {
            return Err(result);
        }
        Ok(())
    }
}

impl fmt::Display for SemanticAnalyzer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.global_symbol_table)
    }
}
