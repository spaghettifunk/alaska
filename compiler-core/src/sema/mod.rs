mod sym;

use core::fmt;
use std::{borrow::BorrowMut, collections::HashMap};

use uuid::Uuid;

use sym::{Context, GlobalSymbolTable, Symbol, SymbolInfo, SymbolTable};

use crate::{
    parser::ast::{Expr, ExprInfo, Stmt, Type, AST},
    T,
};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    global_symbol_table: GlobalSymbolTable,
    forward_references_symbol_table: SymbolTable,
    // Key: String -> SymbolTable name of the current context,
    // Value: List of (String -> Symbol name, CustomType -> the type of the symbol)
    //
    // Since we are doing type checking while building the symbol table
    // some symbols may not be defined yet and there is not a specific type but
    // rather a `Type::Custom`. We can do a pass later on to change the type in the
    // symbol table to the actual type
    undefined_types: HashMap<String, Vec<(String, Type)>>,
}

impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer {
            global_symbol_table: GlobalSymbolTable::new(),
            forward_references_symbol_table: SymbolTable::new("forward_references".to_string()),
            undefined_types: HashMap::new(),
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

        println!("First pass (symbols collection and type checking) - completed");
        match self.forward_references_pass() {
            Ok(_) => {}
            Err(mut es) => {
                errors.append(&mut es);
            }
        }

        println!("Second pass (forward references) - completed");

        if errors.len() > 0 {
            return Err(errors);
        }

        Ok(())
    }

    fn process_stmt(&mut self, context: &mut Context, stmt: Stmt) -> Result<Option<Symbol>, String> {
        match stmt {
            Stmt::Expression(expr) => {
                let e = expr.clone();
                let result = self.collect_expr_symbols(context, *expr);

                match self.type_check_expr(context, *e.clone()) {
                    Ok(typ) => match typ {
                        Some(typ) => {
                            if typ.is_equal(&Type::Unknown) {
                                return Err(format!("error: type of expression `{}` is unknown", e));
                            } else {
                                // If the type is still "custom", add it to the undefined types
                                if typ.variant_eq(&Type::Custom { name: "".to_string() }) {
                                    self.undefined_types
                                        .entry(context.current_scope().unwrap().name.clone())
                                        .or_insert(Vec::new())
                                        .push(("".to_string(), typ.clone()));
                                }
                            }
                        }
                        None => {}
                    },
                    Err(e) => {
                        return Err(e);
                    }
                }
                result
            }
            Stmt::Defer { expr } => {
                let e = expr.clone();
                let result = self.collect_expr_symbols(context, *expr);

                match self.type_check_expr(context, *e.clone()) {
                    Ok(typ) => match typ {
                        Some(typ) => {
                            if typ.is_equal(&Type::Unknown) {
                                return Err(format!("error: type of expression `{}` is unknown", e));
                            }
                        }
                        None => {}
                    },
                    Err(e) => {
                        return Err(e);
                    }
                }
                result
            }
            Stmt::Constant { name, type_, .. } => {
                let constant_symbol = format!("const.{}", name);
                if context.lookup(&constant_symbol).is_some() {
                    Err(format!("error: constant `{}` already declared", name))?
                } else {
                    // At this point, the immutable borrow has ended
                    if let Some(symbol_table) = context.current_scope_mut() {
                        // if the type is still "custom" then we need to add it to the undefined types
                        // in another pass we will replace this with the actual type
                        if type_.variant_eq(&Type::Custom { name: "".to_string() }) {
                            self.undefined_types
                                .entry(symbol_table.name.clone())
                                .or_insert(Vec::new())
                                .push((name.clone(), type_.clone()));
                        }

                        let sym = Symbol::Constant {
                            name: name.clone(),
                            type_: type_.clone(),
                            value: String::new(), // TODO: how do I save the value???
                        };
                        symbol_table.add_symbol(constant_symbol.clone(), sym.clone());
                        return Ok(Some(sym.clone()));
                    }
                    Err(format!("error: constant `{}` not declared", name))?
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

                if let Some(symbol_table) = context.current_scope_mut() {
                    assert_eq!(type_.variant_eq(&Type::Interface { name: "".to_string() }), true);
                    let sym = Symbol::Interface {
                        name: name.clone(),
                        type_: type_.clone(),
                        table: interface_table,
                    };
                    symbol_table.add_symbol(name.clone(), sym.clone());
                    return Ok(Some(sym.clone()));
                }
                Err(format!("error: interface `{}` not declared", name))
            }
            Stmt::InterfaceFunctionSignature {
                name,
                parameters,
                return_type,
                ..
            } => {
                if context.lookup(&name).is_some() {
                    return Err(format!("error: method `{}` in interface already declared", name));
                }
                if let Some(symbol_table) = context.current_scope_mut() {
                    let mut params: Vec<Symbol> = Vec::new();
                    if parameters.is_some() {
                        for (name, typ) in parameters.unwrap() {
                            // if the type is still "custom" then we need to add it to the undefined types
                            // in another pass we will replace this with the actual type
                            if typ.variant_eq(&Type::Custom { name: "".to_string() }) {
                                self.undefined_types
                                    .entry(symbol_table.name.clone())
                                    .or_insert(Vec::new())
                                    .push((name.clone(), typ.clone()));
                            }

                            params.push(Symbol::Identifier {
                                name: name.clone(),
                                type_: typ.clone(),
                                value: String::new(),
                            })
                        }
                    }

                    if return_type.is_some()
                        && return_type
                            .clone()
                            .unwrap()
                            .variant_eq(&Type::Custom { name: "".to_string() })
                    {
                        let rt = return_type.clone().unwrap();
                        // if the type is still "custom" then we need to add it to the undefined types
                        // in another pass we will replace this with the actual type

                        self.undefined_types
                            .entry(symbol_table.name.clone())
                            .or_insert(Vec::new())
                            .push((name.clone(), rt));
                    }

                    let sym = Symbol::InterfaceMethod {
                        name: name.clone(),
                        parameters: if params.len() > 0 { Some(params) } else { None },
                        return_type: return_type.clone(),
                    };
                    symbol_table.add_symbol(name.clone(), sym.clone());
                    return Ok(Some(sym.clone()));
                }
                Err(format!("error: method `{}` in interface not declared", name))
            }
            Stmt::Let { name, type_, expr } => {
                let let_symbol = format!("let.{}", name);
                if context.lookup_current_scope(&let_symbol).is_some() {
                    return Err(format!("error: identifier `{}` already declared", name));
                }

                let result_type = self.type_check_expr(context, *expr.clone());
                match result_type {
                    Ok(ref typ) => match typ {
                        Some(typ) => {
                            if typ.is_equal(&Type::Unknown) {
                                return Err(format!("error: type of expression `{}` is unknown", expr));
                            }
                        }
                        None => {}
                    },
                    Err(e) => {
                        return Err(e);
                    }
                }

                if let Some(symbol_table) = context.current_scope_mut() {
                    match result_type.clone().unwrap() {
                        Some(typ) => {
                            if typ.variant_eq(&Type::Custom { name: "".to_string() }) {
                                // If the type is still "custom", add it to the undefined types
                                if typ.variant_eq(&Type::Custom { name: "".to_string() }) {
                                    self.undefined_types
                                        .entry(symbol_table.name.clone())
                                        .or_insert(Vec::new())
                                        .push((name.clone(), typ.clone()));
                                }
                            }

                            symbol_table.add_symbol(
                                let_symbol.clone(),
                                Symbol::Identifier {
                                    name: name.clone(),
                                    type_: if type_.is_equal(&typ) { type_.clone() } else { typ },
                                    value: String::new(),
                                },
                            );
                        }
                        None => {}
                    }
                }
                // Process the statement's expression
                self.collect_expr_symbols(context, *expr)
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
                    let sym = Symbol::Block(block_symbol_table.clone());
                    symbol_table.add_symbol(block_id.clone(), sym.clone());
                    return Ok(Some(sym.clone()));
                }
                Err(format!("error: block `{}` not declared", block_id))
            }
            Stmt::Return { exprs } => {
                for expr in exprs {
                    self.collect_expr_symbols(context, *expr)?;
                }
                Ok(None)
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
            Stmt::FunctionDeclaration {
                is_public,
                name,
                parameters,
                body,
                return_type,
                ..
            } => self.process_funcdeclr_stmt(name, context, parameters, body, is_public, return_type),
            Stmt::ImplDeclaration {
                name,
                interfaces,
                methods,
                ..
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
                    if let Some(ref interfaces) = interfaces {
                        // we espect that the type in Type::Interface but it could happen that the
                        // interface has not yet defined so we need to add it to the undefined types
                        // in another pass we will replace this with the actual type
                        for it in interfaces {
                            if it.variant_eq(&Type::Custom { name: "".to_string() }) {
                                self.undefined_types
                                    .entry(symbol_table.name.clone())
                                    .or_insert(Vec::new())
                                    .push((name.clone(), it.clone()));
                            }
                        }
                    }

                    let sym = Symbol::Impl {
                        name: name.clone(),
                        type_: Type::Struct { name: name.clone() },
                        interfaces: interfaces.clone(),
                        table: impl_methods_table,
                    };
                    symbol_table.add_symbol(impl_symbol.clone(), sym.clone());
                    return Ok(Some(sym.clone()));
                }
                Err(format!("error: impl `{}` not declared", name))
            }
            Stmt::ConstantGroup { constants } => {
                for constant in constants {
                    self.process_stmt(context, *constant)?;
                }
                Ok(None)
            }
            Stmt::UseDeclaration(name) => {
                let use_symbol = format!("use.{}", name);
                // Check if the use symbol already exists
                if context.lookup(&use_symbol).is_some() {
                    return Err(format!("error: use `{}` already declared", name))?;
                }
                if let Some(symbol_table) = context.current_scope_mut() {
                    let sym = Symbol::Use(name.clone());
                    symbol_table.add_symbol(use_symbol.clone(), sym.clone());
                    return Ok(Some(sym.clone()));
                }
                Err(format!("error: use `{}` not declared", name))
            }
            _ => Ok(None), // empty and package are here
        }
    }

    fn process_funcdeclr_stmt(
        &mut self,
        name: String,
        context: &mut Context,
        parameters: Option<Vec<(String, Type)>>,
        body: Vec<Box<Stmt>>,
        is_public: bool,
        return_type: Option<Type>,
    ) -> Result<Option<Symbol>, String> {
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

                    // if the type is still "custom" then we need to add it to the undefined types
                    if type_.variant_eq(&Type::Custom { name: "".to_string() }) {
                        self.undefined_types
                            .entry(fn_body_table.name.clone())
                            .or_insert(Vec::new())
                            .push((name.clone(), type_.clone()));
                    }

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
            if return_type.is_some() {
                let rt = return_type.clone().unwrap();
                // if the type is still "custom" then we need to add it to the undefined types
                if rt.variant_eq(&Type::Custom { name: "".to_string() }) {
                    self.undefined_types
                        .entry(fn_body_table.name.clone())
                        .or_insert(Vec::new())
                        .push((name.clone(), rt.clone()));
                }
            }
            let sym = Symbol::Function {
                is_public,
                name: name.clone(),
                parameters: if parameters_symbol.len() > 0 {
                    Some(parameters_symbol)
                } else {
                    None
                },
                return_type: return_type.clone(),
                body: fn_body_table,
            };
            symbol_table.add_symbol(function_symbol.clone(), sym.clone());
            return Ok(Some(sym.clone()));
        }
        Err(format!("error: function `{}` not declared", name))
    }

    fn process_structdecl_stmt(
        &mut self,
        name: String,
        context: &mut Context,
        members: Vec<Box<Expr>>,
        is_public: bool,
        type_: Type,
    ) -> Result<Option<Symbol>, String> {
        let struct_symbol = format!("struct.{}", name);
        // Check if the struct symbol already exists
        if context.lookup(&struct_symbol).is_some() {
            return Err(format!("error: struct `{}` already declared", name));
        }

        context.enter_scope(name.clone());

        // Process each struct member
        for member in members {
            let m = member.clone();
            let result = self.collect_expr_symbols(context, *member);
            match result {
                Ok(_) => {
                    let member_type = self.type_check_expr(context, *m.clone());
                    match member_type {
                        Ok(typ) => match typ {
                            Some(typ) => {
                                if typ.is_equal(&Type::Unknown) {
                                    return Err(format!("error: type of struct member `{}` is unknown", *m));
                                } else if typ.variant_eq(&Type::Custom { name: "".to_string() }) {
                                    // If the type is still "custom", add it to the undefined types
                                    self.undefined_types
                                        .entry(context.current_scope().unwrap().name.clone())
                                        .or_insert(Vec::new())
                                        .push(("".to_string(), typ.clone()));
                                }
                            }
                            None => {}
                        },
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
        let struct_members_table = context.current_scope().cloned().unwrap();
        context.exit_scope();

        // Add the struct symbol to the parent symbol table
        if let Some(symbol_table) = context.current_scope_mut() {
            assert_eq!(type_.is_equal(&Type::Struct { name: name.clone() }), true);
            let sym = Symbol::Struct {
                is_public,
                type_: type_.clone(),
                name: name.clone(),
                table: struct_members_table,
            };
            symbol_table.add_symbol(struct_symbol.clone(), sym.clone());
            return Ok(Some(sym.clone()));
        }

        Err(format!("error: struct `{}` not declared", name))
    }

    fn process_if_stmt(
        &mut self,
        context: &mut Context,
        condition: Box<Expr>,
        body: Vec<Box<Stmt>>,
        else_stmt: Option<Box<Stmt>>,
    ) -> Result<Option<Symbol>, String> {
        // TODO: I don't think I should use UUID here but something else to rebuild the name
        let if_id: String = format!("if.{}", Uuid::new_v4().to_string());
        if context.lookup(&if_id).is_some() {
            Err(format!("error: symbol `{}` already declared", if_id))?;
        }

        self.collect_expr_symbols(context, *condition.clone())?;
        let condition_type = self.type_check_expr(context, *condition.clone());
        match condition_type {
            Ok(typ) => match typ {
                Some(typ) => {
                    if !typ.is_equal(&Type::Bool) {
                        return Err(format!(
                            "error: type of the condition `{}` is not a boolean",
                            condition.clone()
                        ));
                    }
                }
                None => {}
            },
            Err(e) => {
                return Err(e);
            }
        }

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
            let sym = Symbol::IfStatement {
                condition: String::new(), // TODO: what should I do here?
                then_body: then_symbol_table,
                else_body: else_symbol_table,
            };
            symbol_table.add_symbol(if_id.clone(), sym.clone());
            return Ok(Some(sym.clone()));
        }
        Err(format!("error: if statement `{}` not declared", if_id))
    }

    fn process_while_stmt(
        &mut self,
        context: &mut Context,
        condition: Box<Expr>,
        body: Vec<Box<Stmt>>,
    ) -> Result<Option<Symbol>, String> {
        // TODO: does this need to go to the parent symbol table or
        // the one that is being created?
        self.collect_expr_symbols(context, *condition.clone())?;
        let condition_type = self.type_check_expr(context, *condition.clone());
        match condition_type {
            Ok(typ) => match typ {
                Some(typ) => {
                    if !typ.is_equal(&Type::Bool) {
                        return Err(format!(
                            "error: type of the condition `{}` is not a boolean",
                            condition.clone()
                        ));
                    }
                }
                None => {}
            },
            Err(e) => {
                return Err(e);
            }
        }

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
            let sym = Symbol::WhileLoop {
                condition: String::new(), // TODO: what should I do here?
                body: while_symbol_table,
            };
            symbol_table.add_symbol(while_id.clone(), sym.clone());
            return Ok(Some(sym.clone()));
        }
        Err(format!("error: while statement `{}` not declared", while_id))
    }

    fn process_range_stmt(
        &mut self,
        context: &mut Context,
        range: Box<Expr>,
        iterator: String,
        body: Vec<Box<Stmt>>,
    ) -> Result<Option<Symbol>, String> {
        // TODO: does this need to go to the parent symbol table or
        // the one that is being created?
        self.collect_expr_symbols(context, *range.clone())?;
        // TODO: range should be type Itearable but how do you do it??
        let _range_type = self.type_check_expr(context, *range.clone());

        let range_id = format!("range.{}", Uuid::new_v4().to_string());

        context.enter_scope(range_id.clone());

        let mut range_symbol_table = context.current_scope().cloned().unwrap();
        range_symbol_table.add_symbol(
            iterator.clone(),
            Symbol::Identifier {
                name: iterator.clone(),
                type_: Type::Unknown,
                value: String::new(),
            },
        );

        // Process each statement in the range loop body
        for stmt in body {
            self.process_stmt(context, *stmt)?;
        }

        context.exit_scope();

        if let Some(symbol_table) = context.current_scope_mut() {
            let sym = Symbol::RangeLoop {
                iterator,
                iterable: String::new(), // TODO: what should I do here?
                body: range_symbol_table,
            };
            symbol_table.add_symbol(range_id.clone(), sym.clone());
            return Ok(Some(sym.clone()));
        }
        Err(format!("error: range statement `{}` not declared", range_id))
    }

    fn process_enum_stmt(
        &self,
        name: String,
        context: &mut Context,
        members: Vec<String>,
        is_public: bool,
        type_: Type,
    ) -> Result<Option<Symbol>, String> {
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
                            type_: Type::Int,
                            value: idx.to_string(),
                        },
                    );
                }
                context.exit_scope();

                if let Some(symbol_table) = context.current_scope_mut() {
                    assert_eq!(type_.is_equal(&Type::Enum), true);
                    let sym = Symbol::Enum {
                        is_public,
                        type_: type_.clone(),
                        name: name.clone(),
                        table: enum_member_symbol_table,
                    };
                    symbol_table.add_symbol(enum_symbol.clone(), sym.clone());
                    return Ok(Some(sym.clone()));
                }
                Err(format!("error: enum `{}` not declared", name))
            }
        }
    }

    fn collect_expr_symbols(&mut self, context: &mut Context, expr: Expr) -> Result<Option<Symbol>, String> {
        let e = expr.clone();
        match expr {
            Expr::Variable(name) => {
                let variable_symbol = format!("let.{}", name);
                // First, check if the symbol exists
                let symbol = context.lookup(&variable_symbol);
                // Add the symbol
                match symbol {
                    Some(sym) => Ok(Some(sym.clone())),
                    None => {
                        let typ = self.type_check_expr(context, e);
                        if typ.is_err() {
                            return Err(typ.unwrap_err());
                        }
                        if let Some(symbol_table) = context.current_scope_mut() {
                            let sym = Symbol::Identifier {
                                name: name.clone(),
                                type_: typ.unwrap().unwrap(),
                                value: String::new(), // Placeholder for the value
                            };
                            symbol_table.add_symbol(variable_symbol.clone(), sym.clone());
                            return Ok(Some(sym.clone()));
                        }
                        Err(format!("error: identifier `{}` not declared", name))
                    }
                }
            }
            Expr::StructMember { is_public, name, type_ } => {
                if context.lookup(&name).is_some() {
                    return Err(format!("error: struct member `{}` already declared", name));
                }
                match self.type_check_expr(context, e) {
                    Ok(typ) => match typ {
                        Some(typ) => {
                            if let Some(symbol_table) = context.current_scope_mut() {
                                let sym = Symbol::StructMember {
                                    is_public,
                                    name: name.clone(),
                                    type_: if type_.is_equal(&typ) { type_.clone() } else { typ },
                                };
                                symbol_table.add_symbol(name.clone(), sym.clone());
                                return Ok(Some(sym.clone()));
                            }
                        }
                        None => {}
                    },
                    Err(e) => {
                        return Err(e);
                    }
                }
                Err(format!("error: struct member `{}` not declared", name))
            }
            Expr::FunctionCall { name, args } => {
                let fn_call_symbol = format!("fn.{}", name);

                let mut arguments: Option<Vec<Symbol>> = None;
                for arg in args.clone() {
                    match self.collect_expr_symbols(context, *arg.clone()) {
                        Ok(s) => match s {
                            Some(symbol) => {
                                arguments.get_or_insert(Vec::new()).push(symbol);
                            }
                            None => {
                                // TODO: what should I do here?
                            }
                        },
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }

                let symbol = context.lookup_mut(&fn_call_symbol);
                match symbol {
                    Some(symbol) => {
                        if arguments.is_some() {
                            symbol.update_arguments(arguments.unwrap());
                        }
                        Ok(Some(symbol.clone()))
                    }
                    None => {
                        // If symbol does not exist, add it to forward_references_symbol_table
                        self.forward_references_symbol_table.add_symbol(
                            fn_call_symbol.clone(),
                            Symbol::FunctionCall {
                                name: name.clone(),
                                arguments,
                            },
                        );
                        return Ok(None);
                    }
                }
            }
            Expr::ArrayInitialization { elements } => {
                for element in elements {
                    self.collect_expr_symbols(context, *element)?;
                }
                Ok(None)
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
                // we use `let` because this is the identifier that has the Struct type
                let struct_symbol = format!("let.{}", name);
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
            Expr::PrefixOp { expr, .. } => self.collect_expr_symbols(context, *expr),
            Expr::PostfixOp { expr, .. } => self.collect_expr_symbols(context, *expr),
            Expr::BinaryOp { lhs, rhs, .. } => {
                self.collect_expr_symbols(context, *lhs)?;
                self.collect_expr_symbols(context, *rhs)
            }
            Expr::Assignment { name, value, .. } => {
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
                // collect expression symbols for each member
                for (_, member) in members {
                    self.collect_expr_symbols(context, *member.clone())?;
                }
                // Immutable borrow to check if the struct symbol exists

                match context.lookup(&struct_symbol) {
                    None => {
                        let sym = Symbol::Struct {
                            is_public: false,
                            name: name.clone(),
                            type_: Type::Custom { name: name.clone() },
                            // TODO: do I need a new context here???
                            table: {
                                let name = name.clone();
                                SymbolTable::new(name.clone())
                            },
                        };
                        // If symbol does not exist, add it to the forward references symbol table
                        self.forward_references_symbol_table
                            .add_symbol(struct_symbol.clone(), sym.clone());

                        Ok(None)
                    }
                    Some(symbol) => Ok(Some(symbol.clone())),
                }
            }
            _ => Ok(None), // these are all the types like int, float, string, etc.
        }
    }

    // forward references pass to check if all symbols are defined
    fn forward_references_pass(&mut self) -> Result<(), Vec<String>> {
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

    fn type_check_expr(&mut self, context: &Context, expr: Expr) -> Result<Option<Type>, String> {
        match expr {
            Expr::IntegerLiteral(_) => Ok(Some(Type::Int)),
            Expr::FloatLiteral(_) => Ok(Some(Type::Float)),
            Expr::StringLiteral(_) => Ok(Some(Type::String)),
            Expr::BoolLiteral(_) => Ok(Some(Type::Bool)),
            Expr::CharLiteral(_) => Ok(Some(Type::Char)),
            Expr::Nil => Ok(Some(Type::Optional(Box::new(Type::Nil)))),
            Expr::Variable(name) => {
                let symbol_name = format!("let.{}", name);
                match context.lookup(&symbol_name) {
                    Some(var) => {
                        let opt_name = var.name().expect("Variable name not found");
                        let typ = var.type_().expect("Variable type not found");
                        if *typ == Type::Unknown {
                            return Err(format!("Variable {} has no type", opt_name));
                        }
                        Ok(Some(typ.clone()))
                    }
                    None => {
                        return Err(format!("Variable {} not found", name));
                    }
                }
            }
            Expr::PrefixOp { op, expr } => match self.type_check_expr(context, *expr.clone())? {
                Some(typ) => match op {
                    T![!] => {
                        if typ == Type::Bool {
                            Ok(Some(Type::Bool))
                        } else {
                            Err(format!("Expected bool, got {}", typ))
                        }
                    }
                    T![-] => {
                        if typ == Type::Int || typ == Type::Float {
                            Ok(Some(typ))
                        } else {
                            Err(format!("Expected int or float, got {}", typ))
                        }
                    }
                    _ => Err(format!("Unknown prefix operator {}", op)),
                },
                None => {
                    return Ok(Some(Type::Unknown));
                }
            },
            Expr::BinaryOp { op, lhs, rhs } => {
                let lhs_typ = self.type_check_expr(context, *lhs.clone())?;
                let rhs_typ = self.type_check_expr(context, *rhs.clone())?;
                if rhs_typ.is_none() || lhs_typ.is_none() {
                    return Ok(Some(Type::Unknown));
                }

                let lhs_typ_copy = lhs_typ.clone().unwrap();
                let rhs_typ_copy = rhs_typ.clone().unwrap();
                match op {
                    T![+] | T![-] | T![*] | T![/] | T![%] => {
                        if lhs_typ_copy == Type::Int && rhs_typ_copy == Type::Int {
                            Ok(Some(Type::Int))
                        } else if lhs_typ_copy == Type::Float && rhs_typ_copy == Type::Float {
                            Ok(Some(Type::Float))
                        } else {
                            Err(format!(
                                "Expected int or float, got {} and {}",
                                lhs_typ_copy, rhs_typ_copy
                            ))
                        }
                    }
                    T![==] | T![!=] | T![<] | T![>] | T![<=] | T![>=] => {
                        if lhs_typ_copy == rhs_typ_copy {
                            Ok(Some(Type::Bool))
                        } else {
                            Err(format!(
                                "Expected same types, got {} and {}",
                                lhs_typ_copy, rhs_typ_copy
                            ))
                        }
                    }
                    T![&&] | T![||] => {
                        if lhs_typ_copy == Type::Bool && rhs_typ_copy == Type::Bool {
                            Ok(Some(Type::Bool))
                        } else {
                            Err(format!("Expected bool, got {} and {}", lhs_typ_copy, rhs_typ_copy))
                        }
                    }
                    _ => Err(format!("Unknown binary operator {}", op)),
                }
            }
            Expr::PostfixOp { op, expr } => match self.type_check_expr(context, *expr.clone())? {
                Some(typ) => match op {
                    T![++] | T![--] => {
                        if typ == Type::Int {
                            Ok(Some(Type::Int))
                        } else {
                            Err(format!("Expected int, got {}", typ))
                        }
                    }
                    _ => Err(format!("Unknown postfix operator {}", op)),
                },
                None => return Ok(Some(Type::Unknown)),
            },
            Expr::FunctionCall { name, args } => {
                let mut arg_types: Vec<Type> = Vec::new();
                for arg in args {
                    let typ = self.type_check_expr(context, *arg.clone())?;
                    arg_types.push(typ.unwrap());
                }
                let symbol_name = format!("fn.{}", name);
                match context.lookup(&symbol_name) {
                    Some(func_symbol) => {
                        if let Some(params) = func_symbol.parameters() {
                            if params.len() != arg_types.len() {
                                return Err(format!("Expected {} arguments, got {}", params.len(), arg_types.len()));
                            }
                            for (i, param) in params.iter().enumerate() {
                                let p_type = param.type_().expect("Parameter type not found");
                                if !p_type.is_equal(&arg_types[i]) {
                                    return Err(format!("Expected argument of type {}, got {}", param, arg_types[i]));
                                }
                            }
                        }

                        if let Some(ret_type) = func_symbol.return_type() {
                            Ok(Some(ret_type.clone()))
                        } else {
                            Ok(Some(Type::Unknown))
                        }
                    }
                    None => {
                        // this can happen if the function is not yet defined
                        // in fact, the type of a FnCall should be the return type of the function
                        self.undefined_types
                            .entry(context.current_scope().unwrap().name.clone())
                            .or_insert(Vec::new())
                            .push((symbol_name.clone(), Type::Custom { name: "".to_string() }));
                        Ok(Some(Type::Unknown))
                    }
                }
            }
            Expr::ArrayInitialization { elements } => {
                let mut elem_types: Vec<Type> = Vec::new();
                for elem in elements.clone() {
                    let typ = self.type_check_expr(context, *elem.clone())?;
                    elem_types.push(typ.unwrap());
                }
                if elem_types.len() == 0 {
                    return Ok(Some(Type::Array(Box::new(Type::Unknown), 0)));
                }
                // the first element determines the type of the array
                let typ = elem_types[0].clone();
                for elem in elem_types {
                    if elem != typ {
                        return Err(format!("Expected array of type {}, got {}", typ, elem));
                    }
                }
                Ok(Some(Type::Array(Box::new(typ), elements.len())))
            }
            Expr::ArrayAccess { name, index } => match self.type_check_expr(context, *index.clone())? {
                Some(index_typ) => {
                    if index_typ != Type::Int {
                        return Err(format!("Expected type int, got {}", index_typ));
                    }

                    let symbol_name = format!("let.{}", name);
                    match context.lookup(&symbol_name) {
                        Some(array_symbol) => {
                            let typ = array_symbol.type_().expect("Variable type not found");
                            if let Type::Array(inner, 0) = typ {
                                Ok(Some(*inner.clone()))
                            } else {
                                return Err(format!("Expected array, got {}", typ));
                            }
                        }
                        None => {
                            return Err(format!("Variable {} not found", name));
                        }
                    }
                }
                None => return Err("Index type not found".to_string()),
            },
            Expr::StructAccess { name, field } => {
                let symbol_name = format!("let.{}", name);
                match context.lookup(&symbol_name) {
                    Some(sym) => {
                        // this always returns Type::Unknown
                        let typ = field.type_().expect("field type not found");
                        Ok(Some(typ.clone()))
                    }
                    None => {
                        return Err(format!("Variable {} not found", name));
                    }
                }
            }
            Expr::StructInstantiation { name, members } => {
                let symbol_name = format!("struct.{}", name);
                match context.lookup(&symbol_name) {
                    Some(_) => {
                        for (member_name, member) in members {
                            match self.type_check_expr(context, *member.clone())? {
                                Some(typ) => {
                                    if typ.variant_eq(&Type::Custom { name: "".to_string() }) {
                                        self.undefined_types
                                            .entry(context.current_scope().unwrap().name.clone())
                                            .or_insert(Vec::new())
                                            .push((member_name.to_string(), typ.clone()));
                                    }
                                }
                                None => {
                                    return Ok(Some(Type::Unknown));
                                }
                            }
                        }
                        Ok(Some(Type::Struct { name }))
                    }
                    None => {
                        return Err(format!("Struct {} not found", name));
                    }
                }
            }
            Expr::StructMember { type_, .. } => {
                return Ok(Some(type_));
            }
            Expr::Assignment { type_, .. } => {
                return Ok(Some(*type_));
            }
            _ => Ok(None),
        }
    }
}

impl fmt::Display for SemanticAnalyzer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.global_symbol_table)
    }
}
