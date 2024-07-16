mod sym;

use std::{
    cell::RefCell,
    mem::{self, take},
    rc::Rc,
};

use uuid::Uuid;

use sym::{GlobalSymbolTable, Symbol, SymbolTable};

use crate::parser::ast::{self, Expr, Stmt, AST};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    pub forward_references_symbol_table: SymbolTable,
}

impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer {
            forward_references_symbol_table: SymbolTable::new("forward_references".to_string(), None),
        }
    }

    // first pass to collect as many symbols as possible
    pub fn analyze(&mut self, ast: AST) -> Result<(), Vec<String>> {
        let mut errors: Vec<String> = Vec::new();
        let mut global_symbol_table = GlobalSymbolTable::new();

        println!("First pass (symbols collection)...");

        let files = ast.files.clone();
        files.iter().all(|sourcefile| {
            let mut current_package_symbol_table = &mut SymbolTable::new("unknown".to_string(), None);

            println!("analyzing file: {}", sourcefile.name);

            for stmt in &sourcefile.statements {
                let pkg_decl = stmt.is_package_declaration();
                if pkg_decl.0 {
                    let pkg_name = format!("pkg.{}", pkg_decl.1);
                    current_package_symbol_table = global_symbol_table.get_package_symbol_table(&pkg_name);
                }

                match self.process_stmt(&mut current_package_symbol_table, *stmt.clone()) {
                    Ok(_) => {}
                    Err(e) => {
                        errors.push(e);
                    }
                }
            }

            true
        });

        println!("First pass (symbols collection) - completed");
        println!("Second pass (forward references)...");
        match self.forward_references_pass(&global_symbol_table) {
            Ok(_) => {}
            Err(mut es) => {
                errors.append(&mut es);
            }
        }

        println!("Second pass (forward references) - completed");

        if errors.len() > 0 {
            return Err(errors);
        }

        // TODO: remove this
        // println!("{:#?}", global_symbol_table);

        Ok(())
    }

    fn process_stmt(&mut self, symbol_table: &mut SymbolTable, stmt: Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Expression(expr) => self.collect_expr_symbols(symbol_table, *expr),
            Stmt::Defer { expr } => self.collect_expr_symbols(symbol_table, *expr),
            Stmt::Constant {
                is_public,
                name,
                type_,
                value,
            } => {
                let constant_symbol = format!("const.{}", name);
                if symbol_table.lookup_local_scope(&constant_symbol).is_some() {
                    Err(format!("error: constant `{}` already declared", name))?
                } else {
                    // At this point, the immutable borrow has ended
                    symbol_table.add_symbol(
                        constant_symbol.clone(),
                        Symbol::Constant {
                            name: name.clone(),
                            type_: type_.clone(),
                            value: String::new(), // TODO: how do I save the value???
                        },
                    );
                    Ok(())
                }
            }
            Stmt::Interface { name, type_, methods } => {
                let interface_symbol = format!("interface.{}", name);
                // Check if the interface symbol already exists
                if symbol_table.lookup_local_scope(&interface_symbol).is_some() {
                    return Err(format!("error: interface `{}` already declared", name));
                }

                let mut interface_table = SymbolTable::new(name.clone(), None);

                for method in methods {
                    self.process_stmt(&mut interface_table, *method)?;
                }

                // Now we can safely take a mutable borrow
                symbol_table.add_symbol(
                    name.clone(),
                    Symbol::Interface {
                        name: name.clone(),
                        type_: type_.clone(),
                        table: interface_table,
                    },
                );
                Ok(())
            }
            Stmt::InterfaceFunctionSignature {
                name,
                generics,
                parameters,
                return_type,
            } => {
                if symbol_table.lookup_local_scope(&name).is_some() {
                    return Err(format!("error: method `{}` in interface already declared", name));
                }
                symbol_table.add_symbol(
                    name.clone(),
                    Symbol::InterfaceMethod {
                        name: name.clone(),
                        parameters: parameters.clone(),
                        return_type: return_type.clone(),
                    },
                );
                Ok(())
            }
            Stmt::Let {
                name,
                type_,
                expr: statement,
            } => {
                let let_symbol = format!("let.{}", name);
                if symbol_table.lookup_local_scope(&let_symbol).is_some() {
                    return Err(format!("error: identifier `{}` already declared", name));
                }
                // Mutable borrow to add the symbol
                symbol_table.add_symbol(
                    let_symbol.clone(),
                    Symbol::Identifier {
                        name: name.clone(),
                        type_: type_.clone(),
                        value: String::new(),
                    },
                );
                // Process the statement's expression
                self.collect_expr_symbols(symbol_table, *statement)
            }
            Stmt::IfStmt {
                condition,
                body,
                else_stmt,
            } => self.process_if_stmt(symbol_table, condition, body, else_stmt),
            Stmt::RangeStmt { iterator, range, body } => self.process_range_stmt(symbol_table, range, iterator, body),
            Stmt::WhileStmt { condition, body } => self.process_while_stmt(symbol_table, condition, body),
            Stmt::Block { stmts } => {
                let block_id = format!("block.{}", Uuid::new_v4().to_string());
                // Create a new block symbol table with the parent set to the current symbol table

                let mut block_symbol_table = SymbolTable::new(
                    block_id.clone(),
                    mem::replace(
                        &mut Some(Rc::new(SymbolTable::new("unknown".to_string(), None))),
                        Some(Rc::new(symbol_table.clone())),
                    ),
                );

                // Process each statement within the new block scope
                for stmt in stmts {
                    self.process_stmt(&mut block_symbol_table, *stmt)?;
                }
                // Now, we can safely borrow the parent symbol table mutably and add the new block symbol table
                symbol_table.add_symbol(block_id.clone(), Symbol::Block(Some(block_symbol_table)));

                Ok(())
            }
            Stmt::Return { exprs } => {
                for expr in exprs {
                    self.collect_expr_symbols(symbol_table, *expr)?;
                }
                Ok(())
            }
            Stmt::Enum {
                is_public,
                name,
                type_,
                members,
            } => self.process_enum_stmt(name, symbol_table, members, is_public, type_),
            Stmt::StructDeclaration {
                is_public,
                name,
                type_,
                members,
            } => self.process_structdecl_stmt(name, symbol_table, members, is_public, type_),
            Stmt::StructMember { is_public, name, type_ } => {
                if symbol_table.lookup_local_scope(&name).is_some() {
                    return Err(format!("error: struct member `{}` already declared", name));
                }
                symbol_table.add_symbol(
                    name.clone(),
                    Symbol::StructMember {
                        is_public,
                        name: name.clone(),
                        type_: type_.clone(),
                    },
                );
                Ok(())
            }
            Stmt::FunctionDeclaration {
                is_public,
                name,
                generics,
                parameters,
                body,
                return_type,
            } => self.process_funcdeclr_stmt(name, symbol_table, parameters, body, is_public, return_type),
            Stmt::ImplDeclaration {
                name,
                generics,
                interfaces,
                methods,
            } => {
                let impl_symbol = format!("impl.{}", name);

                // Check if the impl symbol already exists
                if symbol_table.lookup_local_scope(&impl_symbol).is_some() {
                    return Err(format!("error: impl `{}` already declared", name));
                }
                // At this point, the immutable borrow has ended

                let mut impl_methods_table = SymbolTable::new(
                    name.clone(),
                    mem::replace(
                        &mut Some(Rc::new(SymbolTable::new("unknown".to_string(), None))),
                        Some(Rc::new(symbol_table.clone())),
                    ),
                );

                for method in methods {
                    self.process_stmt(&mut impl_methods_table, *method)?;
                }

                // Now we can safely take a mutable borrow
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

                Ok(())
            }
            Stmt::ConstantGroup { constants } => {
                for constant in constants {
                    self.process_stmt(symbol_table, *constant)?;
                }
                Ok(())
            }
            Stmt::UseDeclaration(name) => {
                let use_symbol = format!("use.{}", name);
                // Check if the use symbol already exists
                if symbol_table.lookup_local_scope(&use_symbol).is_some() {
                    return Err(format!("error: use `{}` already declared", name))?;
                }
                // At this point, the immutable borrow has ended, so we can take a mutable borrow
                symbol_table.add_symbol(use_symbol.clone(), Symbol::Use(name.clone()));
                Ok(())
            }
            _ => Ok(()), // empty and package are here
        }
    }

    fn process_funcdeclr_stmt(
        &mut self,
        name: String,
        symbol_table: &mut SymbolTable,
        parameters: Option<Vec<(String, ast::Type)>>,
        body: Vec<Box<Stmt>>,
        is_public: bool,
        return_type: Option<Vec<ast::Type>>,
    ) -> Result<(), String> {
        let function_symbol = format!("fn.{}", name);

        // Check if the function symbol already exists
        if symbol_table.lookup_local_scope(&function_symbol).is_some() {
            return Err(format!("error: function `{}` already declared", name));
        }

        let mut fn_body_table = SymbolTable::new(
            name.clone(),
            mem::replace(
                &mut Some(Rc::new(SymbolTable::new("unknown".to_string(), None))),
                Some(Rc::new(symbol_table.clone())),
            ),
        );

        let mut parameters_symbol = Vec::new();
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

        // Process the function body
        for stmt in body {
            self.process_stmt(&mut fn_body_table, *stmt)?;
        }

        // Now add the function symbol to the parent symbol table
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

        Ok(())
    }

    fn process_structdecl_stmt(
        &mut self,
        name: String,
        symbol_table: &mut SymbolTable,
        members: Vec<Box<Stmt>>,
        is_public: bool,
        type_: ast::Type,
    ) -> Result<(), String> {
        let struct_symbol = format!("struct.{}", name);
        // Check if the struct symbol already exists
        if symbol_table.lookup_local_scope(&struct_symbol).is_some() {
            return Err(format!("error: struct `{}` already declared", name));
        }

        // Create a new symbol table for struct members with the current symbol_table as parent
        let mut struct_members_table = SymbolTable::new(
            name.clone(),
            mem::replace(
                &mut Some(Rc::new(SymbolTable::new("unknown".to_string(), None))),
                Some(Rc::new(symbol_table.clone())),
            ),
        );
        // Process each struct member
        for member in members {
            self.process_stmt(&mut struct_members_table, *member)?;
        }
        // Add the struct symbol to the parent symbol table
        symbol_table.add_symbol(
            struct_symbol.clone(),
            Symbol::Struct {
                is_public,
                type_: type_.clone(),
                name: name.clone(),
                table: struct_members_table,
            },
        );
        Ok(())
    }

    fn process_if_stmt(
        &mut self,
        symbol_table: &mut SymbolTable,
        condition: Box<Expr>,
        body: Vec<Box<Stmt>>,
        else_stmt: Option<Box<Stmt>>,
    ) -> Result<(), String> {
        let if_id: String = format!("if.{}", Uuid::new_v4().to_string());
        if symbol_table.lookup_local_scope(&if_id).is_some() {
            Err(format!("error: symbol `{}` already declared", if_id))?;
        }

        self.collect_expr_symbols(symbol_table, *condition)?;

        let mut then_symbol_table = SymbolTable::new(
            if_id.clone(),
            mem::replace(
                &mut Some(Rc::new(SymbolTable::new("unknown".to_string(), None))),
                Some(Rc::new(symbol_table.clone())),
            ),
        );

        for stmt in body {
            self.process_stmt(&mut then_symbol_table, *stmt)?;
        }

        let else_id = format!("else.{}", Uuid::new_v4().to_string());
        let sy = Rc::new(take(symbol_table));
        let mut else_symbol_table = SymbolTable::new(else_id.clone(), Some(sy));
        let else_body_present = if let Some(else_stmt) = else_stmt {
            self.process_stmt(&mut else_symbol_table, *else_stmt)?;
            true
        } else {
            false
        };

        symbol_table.add_symbol(
            if_id.clone(),
            Symbol::IfStatement {
                condition: String::new(), // TODO: is this correct?
                then_body: then_symbol_table,
                else_body: if else_body_present {
                    Some(else_symbol_table)
                } else {
                    None
                },
            },
        );

        Ok(())
    }

    fn process_while_stmt(
        &mut self,
        symbol_table: &mut SymbolTable,
        condition: Box<Expr>,
        body: Vec<Box<Stmt>>,
    ) -> Result<(), String> {
        // TODO: does this need to go to the parent symbol table or
        // the one that is being created?
        self.collect_expr_symbols(symbol_table, *condition)?;

        let while_id = format!("while.{}", Uuid::new_v4().to_string());

        // Check if the while symbol already exists
        if symbol_table.lookup_local_scope(&while_id).is_some() {
            return Err(format!("error: symbol `{}` already declared", while_id));
        }

        // Create a new symbol table for the while loop with the current symbol_table as parent
        let mut while_symbol_table = SymbolTable::new(
            while_id.clone(),
            mem::replace(
                &mut Some(Rc::new(SymbolTable::new("unknown".to_string(), None))),
                Some(Rc::new(symbol_table.clone())),
            ),
        );

        // Process each statement in the while loop body
        for stmt in body {
            self.process_stmt(&mut while_symbol_table, *stmt)?;
        }

        // Add the while symbol to the parent symbol table
        symbol_table.add_symbol(
            while_id.clone(),
            Symbol::WhileLoop {
                condition: String::new(), // TODO: Adjust this as per your logic
                body: while_symbol_table,
            },
        );

        Ok(())
    }

    fn process_range_stmt(
        &mut self,
        symbol_table: &mut SymbolTable,
        range: Box<Expr>,
        iterator: String,
        body: Vec<Box<Stmt>>,
    ) -> Result<(), String> {
        // TODO: does this need to go to the parent symbol table or
        // the one that is being created?
        self.collect_expr_symbols(symbol_table, *range)?;

        let range_id = format!("range.{}", Uuid::new_v4().to_string());

        // Create a new symbol table for the range loop with the current symbol_table as parent
        let mut range_symbol_table = SymbolTable::new(
            range_id.clone(),
            mem::replace(
                &mut Some(Rc::new(SymbolTable::new("unknown".to_string(), None))),
                Some(Rc::new(symbol_table.clone())),
            ),
        );

        // Add the iterator symbol to the range symbol table
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
            self.process_stmt(&mut range_symbol_table, *stmt)?;
        }

        // Add the range symbol to the parent symbol table
        symbol_table.add_symbol(
            range_id.clone(),
            Symbol::RangeLoop {
                iterator,
                iterable: String::new(), // TODO: Adjust this as per your logic
                body: range_symbol_table,
            },
        );

        Ok(())
    }

    fn process_enum_stmt(
        &self,
        name: String,
        symbol_table: &mut SymbolTable,
        members: Vec<String>,
        is_public: bool,
        type_: ast::Type,
    ) -> Result<(), String> {
        let enum_symbol = format!("enum.{}", name);

        // Borrow the symbol table immutably to check if the enum symbol already exists
        let symbol = symbol_table.lookup_local_scope(&enum_symbol);

        match symbol {
            Some(_) => Err(format!("error: enum `{}` already declared", name))?,
            None => {
                // Create a new symbol table for the enum members with the current symbol_table as parent
                let mut enum_member_symbol_table = SymbolTable::new(name.clone(), None);
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
                            value: member.clone(),
                        },
                    );
                }
                // Add the enum symbol to the parent symbol table
                symbol_table.add_symbol(
                    enum_symbol.clone(),
                    Symbol::Enum {
                        is_public,
                        type_: type_.clone(),
                        name: name.clone(),
                        table: enum_member_symbol_table,
                    },
                );
                Ok(())
            }
        }
    }

    fn collect_expr_symbols(&mut self, symbol_table: &mut SymbolTable, expr: Expr) -> Result<(), String> {
        match expr {
            Expr::Variable(name) => {
                let variable_symbol = format!("let.{}", name);
                // First, check if the symbol exists
                let symbol = symbol_table.lookup(&variable_symbol);
                // Add the symbol
                match symbol {
                    Some(_) => Ok(()),
                    None => {
                        symbol_table.add_symbol(
                            variable_symbol.clone(),
                            Symbol::Identifier {
                                name: name.clone(),
                                type_: ast::Type::Unknown,
                                value: String::new(), // Placeholder for the value
                            },
                        );
                        Ok(())
                    }
                }
            }
            Expr::FunctionCall { name, args } => {
                let fn_call_symbol = format!("fn.{}", name);

                // Borrow symbol_table to check if the function call symbol already exists
                let symbol = symbol_table.lookup(&fn_call_symbol);

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
                    self.collect_expr_symbols(symbol_table, *arg.clone())?;
                }
                Ok(())
            }
            Expr::ArrayInitialization { elements } => {
                for element in elements {
                    self.collect_expr_symbols(symbol_table, *element)?;
                }
                Ok(())
            }
            Expr::ArrayAccess { name, index } => {
                let array_symbol = format!("array.{}", name);

                // Borrow symbol_table immutably to check if the array symbol exists
                let symbol = symbol_table.lookup_local_scope(&array_symbol);

                match symbol {
                    Some(_) => {
                        // If symbol exists, collect expression symbols for the index
                        self.collect_expr_symbols(symbol_table, *index)
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
                let symbol = symbol_table.lookup(&struct_symbol);
                match symbol {
                    Some(_) => {
                        // If symbol exists, collect expression symbols for the field
                        self.collect_expr_symbols(symbol_table, *field)
                    }
                    None => {
                        // If symbol does not exist, return an error message
                        Err(format!("error: struct `{}` not declared", name))
                    }
                }
            }
            Expr::PrefixOp { op, expr } => self.collect_expr_symbols(symbol_table, *expr),
            Expr::PostfixOp { op, expr } => self.collect_expr_symbols(symbol_table, *expr),
            Expr::BinaryOp { op, lhs, rhs } => {
                self.collect_expr_symbols(symbol_table, *lhs)?;
                self.collect_expr_symbols(symbol_table, *rhs)
            }
            Expr::Assignment { name, type_, value } => {
                let variable_symbol = format!("let.{}", name);
                // Immutable borrow to check if the variable symbol exists
                let symbol = symbol_table.lookup(&variable_symbol);
                match symbol {
                    Some(_) => {
                        // If symbol exists, collect expression symbols for the value
                        self.collect_expr_symbols(symbol_table, *value)
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
                let symbol = symbol_table.lookup(&struct_symbol);
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
                            table: SymbolTable::new(name.clone(), None),
                        },
                    );
                }
                // collect expression symbols for each member
                for (_, member) in members {
                    self.collect_expr_symbols(symbol_table, *member.clone())?;
                }
                Ok(())
            }
            _ => Ok(()), // these are all the types like int, float, string, etc.
        }
    }

    // forward references pass to check if all symbols are defined
    pub fn forward_references_pass(&mut self, global_symbol_table: &GlobalSymbolTable) -> Result<(), Vec<String>> {
        let result: Vec<String> = self
            .forward_references_symbol_table
            .get_symbols()
            .iter()
            .map(|(symbol_name, _)| {
                let found = global_symbol_table.lookup_symbol(&symbol_name);
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

    // type checking pass
    pub fn type_check_pass(&mut self, ast: AST) -> Result<(), Vec<String>> {
        let mut errors: Vec<String> = Vec::new();
        for sourcefile in ast.files {
            let mut current_package = String::new();
            for stmt in sourcefile.statements {
                let result = self.type_check_stmt(*stmt.clone(), &mut current_package, &None);
                match result {
                    Ok(_) => {}
                    Err(e) => {
                        errors.push(e);
                    }
                }
            }
        }

        if errors.len() > 0 {
            return Err(errors);
        }

        Ok(())
    }

    fn type_check_stmt(
        &mut self,
        stmt: Stmt,
        current_package: &mut String,
        symbol_table: &Option<Box<SymbolTable>>,
    ) -> Result<(), String> {
        match stmt {
            _ => Ok(()),
        }
    }

    fn type_check_expr(&mut self, expr: Expr) -> Result<(), String> {
        match expr {
            _ => Ok(()),
        }
    }
}
