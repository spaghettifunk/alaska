mod sym;

use core::borrow;
use std::{
    borrow::BorrowMut,
    cell::RefCell,
    fmt::{self, Formatter},
    rc::Rc,
};

use uuid::Uuid;

use sym::{GlobalSymbolTable, Symbol, SymbolTable};

use crate::parser::ast::{self, Expr, Stmt, AST};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    pub global_symbol_table: GlobalSymbolTable,
    pub forward_references_symbol_table: Rc<RefCell<SymbolTable>>,
}

impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer {
            global_symbol_table: GlobalSymbolTable::new(),
            forward_references_symbol_table: Rc::new(RefCell::new(SymbolTable::new(
                "forward_references".to_string(),
                None,
            ))),
        }
    }

    // first pass to collect as many symbols as possible
    pub fn symbols_collection_pass(&mut self, ast: AST) -> Result<(), Vec<String>> {
        let mut errors: Vec<String> = Vec::new();

        // add all the packages to the global symbol table
        let files = ast.files.clone();
        files.iter().all(|sourcefile| {
            let mut current_package_symbol_table =
                Rc::new(RefCell::new(SymbolTable::new("pkg.main".to_string(), None)));

            println!("analyzing file: {}", sourcefile.name);

            for stmt in &sourcefile.statements {
                let pkg_decl = stmt.is_package_declaration();
                if pkg_decl.0 {
                    let pkg_name = format!("pkg.{}", pkg_decl.1);
                    current_package_symbol_table = self
                        .global_symbol_table
                        .get_package_symbol_table(&pkg_name)
                        .borrow_mut()
                        .clone();
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

        if errors.len() > 0 {
            return Err(errors);
        }

        Ok(())
    }

    fn process_stmt(&mut self, symbol_table: &mut Rc<RefCell<SymbolTable>>, stmt: Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Expression(expr) => self.collect_expr_symbols(symbol_table, *expr),
            Stmt::Defer { expr } => self.collect_expr_symbols(symbol_table, *expr),
            Stmt::Empty => Ok(()),
            Stmt::Constant {
                is_public,
                name,
                type_,
                value,
            } => {
                let constant_symbol = format!("const.{}", name);

                let symbol_table_clone = Rc::clone(&symbol_table);

                let borrowed_symbol_table = symbol_table_clone.borrow();
                let symbol = borrowed_symbol_table.lookup_local_scope(&constant_symbol);
                match symbol {
                    Some(_) => Err(format!("error: constant `{}` already declared", name))?,
                    None => {
                        let symbol_table_clone = Rc::clone(&symbol_table);
                        symbol_table_clone.as_ref().borrow_mut().add_symbol(
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
            }
            Stmt::Interface { name, type_, methods } => {
                let interface_symbol = format!("interface.{}", name);

                let symbol_table_clone = Rc::clone(&symbol_table);

                let borrowed_symbol_table = symbol_table_clone.borrow();
                let symbol = borrowed_symbol_table.lookup_local_scope(&interface_symbol);
                match symbol {
                    Some(_) => Err(format!("error: interface `{}` already declared", name)),
                    None => {
                        let sy = Rc::clone(&symbol_table);
                        let mut interface_table = Rc::new(RefCell::new(SymbolTable::new(name.clone(), Some(sy))));

                        for method in methods {
                            self.process_stmt(&mut interface_table, *method)?;
                        }

                        symbol_table.as_ref().borrow_mut().add_symbol(
                            name.clone(),
                            Symbol::Interface {
                                name: name.clone(),
                                type_: type_.clone(),
                                table: interface_table,
                            },
                        );
                        Ok(())
                    }
                }
            }
            Stmt::InterfaceFunctionSignature {
                name,
                generics,
                parameters,
                return_type,
            } => {
                let borrowed_symbol_table = symbol_table.borrow();
                let symbol = borrowed_symbol_table.lookup_local_scope(&name);
                match symbol {
                    Some(_) => {
                        Err(format!("error: method `{}` in interface already declared", name))?;
                    }
                    None => {
                        symbol_table.as_ref().borrow_mut().add_symbol(
                            name.clone(),
                            Symbol::InterfaceMethod {
                                name: name.clone(),
                                parameters: parameters.clone(),
                                return_type: return_type.clone(),
                            },
                        );
                    }
                }
                Ok(())
            }
            Stmt::Let {
                name,
                type_,
                expr: statement,
            } => {
                let let_symbol = format!("let.{}", name);

                {
                    // Immutable borrow to check for the symbol
                    let borrowed_symbol_table = symbol_table.borrow();
                    if let Some(_) = borrowed_symbol_table.lookup_local_scope(&let_symbol) {
                        return Err(format!("error: identifier `{}` already declared", name));
                    }
                } // Immutable borrow ends here

                {
                    // Mutable borrow to add the symbol
                    let borrowed_symbol_table = symbol_table.borrow_mut();
                    borrowed_symbol_table.as_ref().borrow_mut().add_symbol(
                        let_symbol.clone(),
                        Symbol::Identifier {
                            name: name.clone(),
                            type_: type_.clone(),
                            value: String::new(),
                        },
                    );
                } // Mutable borrow ends here

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

                let sy = Rc::clone(&symbol_table);
                let mut block_symbol_table = Rc::new(RefCell::new(SymbolTable::new(block_id.clone(), Some(sy))));

                for stmt in stmts {
                    self.process_stmt(&mut block_symbol_table, *stmt)?;
                }

                symbol_table
                    .as_ref()
                    .borrow_mut()
                    .add_symbol(block_id.clone(), Symbol::Block(Some(block_symbol_table)));

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
                let symbol_exists = {
                    let borrowed_symbol_table = symbol_table.borrow();
                    borrowed_symbol_table.lookup_local_scope(&name).is_some()
                };

                if symbol_exists {
                    Err(format!("error: struct member `{}` already declared", name))
                } else {
                    let borrowed_symbol_table = symbol_table.borrow_mut();
                    borrowed_symbol_table.as_ref().borrow_mut().add_symbol(
                        name.clone(),
                        Symbol::StructMember {
                            is_public,
                            name: name.clone(),
                            type_: type_.clone(),
                        },
                    );
                    Ok(())
                }
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
                let borrowed_symbol_table = symbol_table.borrow();
                let symbol = borrowed_symbol_table.lookup_local_scope(&impl_symbol);
                match symbol {
                    Some(_) => Err(format!("error: impl `{}` already declared", name))?,
                    None => {
                        let sy = Rc::clone(&symbol_table);
                        let mut impl_methods_table = Rc::new(RefCell::new(SymbolTable::new(name.clone(), Some(sy))));

                        for method in methods {
                            self.process_stmt(&mut impl_methods_table, *method)?;
                        }

                        symbol_table.as_ref().borrow_mut().add_symbol(
                            impl_symbol.clone(),
                            Symbol::Impl {
                                name: name.clone(),
                                type_: ast::Type::new(name.clone(), ast::Expr::Struct(name.clone()), None),
                                interfaces: interfaces.clone(),
                                table: impl_methods_table,
                            },
                        );
                        Ok(())
                    }
                }
            }
            Stmt::ConstantGroup { constants } => {
                for constant in constants {
                    self.process_stmt(symbol_table, *constant)?;
                }
                Ok(())
            }
            Stmt::UseDeclaration(name) => {
                let use_symbol = format!("use.{}", name);

                let symbol_table_clone = Rc::clone(&symbol_table);

                let borrowed_symbol_table = symbol_table_clone.borrow();
                let symbol = borrowed_symbol_table.lookup_local_scope(&use_symbol);
                match symbol {
                    Some(_) => Err(format!("error: use `{}` already declared", name))?,
                    None => {
                        symbol_table
                            .as_ref()
                            .borrow_mut()
                            .add_symbol(use_symbol.clone(), Symbol::Use(name.clone()));
                        Ok(())
                    }
                }
            }
            _ => Ok(()),
        }
    }

    fn process_funcdeclr_stmt(
        &mut self,
        name: String,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        parameters: Option<Vec<(String, ast::Type)>>,
        body: Vec<Box<Stmt>>,
        is_public: bool,
        return_type: Option<Vec<ast::Type>>,
    ) -> Result<(), String> {
        let function_symbol = format!("fn.{}", name);

        // Check if the function symbol already exists
        let symbol_exists = {
            let borrowed_symbol_table = symbol_table.borrow();
            borrowed_symbol_table.lookup_local_scope(&function_symbol).is_some()
        };

        if symbol_exists {
            Err(format!("error: function `{}` already declared", name))?
        } else {
            let sy = Rc::clone(&symbol_table);
            let mut fn_body_table = Rc::new(RefCell::new(SymbolTable::new(name.clone(), Some(sy))));

            let mut parameters_symbol = Vec::new();
            if let Some(params) = parameters.clone() {
                for (name, type_) in &params {
                    let ident = Symbol::Identifier {
                        name: name.clone(),
                        type_: type_.clone(),
                        value: String::new(), // Empty value for now
                    };
                    fn_body_table
                        .as_ref()
                        .borrow_mut()
                        .add_symbol(name.clone(), ident.clone());
                    parameters_symbol.push(ident);
                }
            }

            // Process the function body
            for stmt in body {
                self.process_stmt(&mut fn_body_table, *stmt)?;
            }

            // Now add the function symbol to the parent symbol table
            symbol_table.as_ref().borrow_mut().add_symbol(
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
    }

    fn process_structdecl_stmt(
        &mut self,
        name: String,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        members: Vec<Box<Stmt>>,
        is_public: bool,
        type_: ast::Type,
    ) -> Result<(), String> {
        let struct_symbol = format!("struct.{}", name);
        let borrowed_symbol_table = symbol_table.borrow();
        let symbol = borrowed_symbol_table.lookup_local_scope(&struct_symbol);
        match symbol {
            Some(_) => Err(format!("error: struct `{}` already declared", name)),
            None => {
                let sy = Rc::clone(&symbol_table);
                let mut struct_members_table = Rc::new(RefCell::new(SymbolTable::new(name.clone(), Some(sy))));

                for member in members {
                    self.process_stmt(&mut struct_members_table, *member)?;
                }

                symbol_table.as_ref().borrow_mut().add_symbol(
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
        }
    }

    fn process_if_stmt(
        &mut self,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        condition: Box<Expr>,
        body: Vec<Box<Stmt>>,
        else_stmt: Option<Box<Stmt>>,
    ) -> Result<(), String> {
        let if_id: String = format!("if.{}", Uuid::new_v4().to_string());
        let symbol_exists = {
            let borrowed_symbol_table = symbol_table.borrow();
            borrowed_symbol_table.lookup_local_scope(&if_id).is_some()
        };

        if symbol_exists {
            Err(format!("error: symbol `{}` already declared", if_id))?;
        } else {
            self.collect_expr_symbols(symbol_table, *condition)?;

            let sy = Rc::clone(&symbol_table);
            let mut then_symbol_table = Rc::new(RefCell::new(SymbolTable::new(if_id.clone(), Some(sy))));
            for stmt in body {
                self.process_stmt(&mut then_symbol_table, *stmt)?;
            }

            let else_id = format!("else.{}", Uuid::new_v4().to_string());
            let sy = Rc::clone(&symbol_table);
            let mut else_symbol_table = Rc::new(RefCell::new(SymbolTable::new(else_id.clone(), Some(sy))));
            let else_body_present = if let Some(else_stmt) = else_stmt {
                self.process_stmt(&mut else_symbol_table, *else_stmt)?;
                true
            } else {
                false
            };

            symbol_table.as_ref().borrow_mut().add_symbol(
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
        }

        Ok(())
    }

    fn process_while_stmt(
        &mut self,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        condition: Box<Expr>,
        body: Vec<Box<Stmt>>,
    ) -> Result<(), String> {
        // TODO: does this need to go to the parent symbol table or
        // the one that is being created?
        self.collect_expr_symbols(symbol_table, *condition)?;

        let while_id = format!("while.{}", Uuid::new_v4().to_string());
        let borrowed_symbol_table = symbol_table.borrow();
        let symbol = borrowed_symbol_table.lookup_local_scope(&while_id);

        match symbol {
            Some(_) => Err(format!("error: symbol `{}` already declared", while_id)),
            None => {
                let sy = Rc::clone(&symbol_table);
                let mut while_symbol_table = Rc::new(RefCell::new(SymbolTable::new(while_id.clone(), Some(sy))));

                for stmt in body {
                    self.process_stmt(&mut while_symbol_table, *stmt)?;
                }

                symbol_table.as_ref().borrow_mut().add_symbol(
                    while_id.clone(),
                    Symbol::WhileLoop {
                        condition: String::new(), // TODO: is this correct?
                        body: while_symbol_table,
                    },
                );

                Ok(())
            }
        }
    }

    fn process_range_stmt(
        &mut self,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        range: Box<Expr>,
        iterator: String,
        body: Vec<Box<Stmt>>,
    ) -> Result<(), String> {
        // TODO: does this need to go to the parent symbol table or
        // the one that is being created?
        self.collect_expr_symbols(symbol_table, *range)?;

        let range_id = format!("range.{}", Uuid::new_v4().to_string());

        let sy = Rc::clone(&symbol_table);
        let mut range_symbol_table = Rc::new(RefCell::new(SymbolTable::new(range_id.clone(), Some(sy))));

        range_symbol_table.as_ref().borrow_mut().add_symbol(
            iterator.clone(),
            Symbol::Identifier {
                name: iterator.clone(),
                type_: ast::Type::new("nil".to_string(), ast::Expr::Nil, None),
                value: String::new(),
            },
        );

        for stmt in body {
            self.process_stmt(&mut range_symbol_table, *stmt)?;
        }

        symbol_table.as_ref().borrow_mut().add_symbol(
            range_id.clone(),
            Symbol::RangeLoop {
                iterator,
                iterable: String::new(),
                body: range_symbol_table,
            },
        );

        Ok(())
    }

    fn process_enum_stmt(
        &self,
        name: String,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        members: Vec<String>,
        is_public: bool,
        type_: ast::Type,
    ) -> Result<(), String> {
        let enum_symbol = format!("enum.{}", name);

        let symbol_table_clone = Rc::clone(&symbol_table);

        let borrowed_symbol_table = symbol_table_clone.borrow();
        let symbol = borrowed_symbol_table.lookup_local_scope(&enum_symbol);

        match symbol {
            Some(_) => Err(format!("error: enum `{}` already declared", name))?,
            None => {
                let sy = Rc::clone(&symbol_table);
                let enum_member_symbol_table = Rc::new(RefCell::new(SymbolTable::new(name.clone(), Some(sy))));

                for (idx, member) in members.iter().enumerate() {
                    if enum_member_symbol_table.as_ref().borrow_mut().lookup(&member).is_some() {
                        Err(format!("error: enum member `{}` already declared", member))?;
                    }

                    enum_member_symbol_table.as_ref().borrow_mut().add_symbol(
                        member.clone(),
                        Symbol::EnumMember {
                            name: member.clone(),
                            type_: ast::Type::new(member.clone(), ast::Expr::Int(idx), None),
                            value: member.clone(),
                        },
                    );
                }

                symbol_table.as_ref().borrow_mut().add_symbol(
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

    fn collect_expr_symbols(&mut self, symbol_table: &Rc<RefCell<SymbolTable>>, expr: Expr) -> Result<(), String> {
        match expr {
            Expr::Comment(_) => Ok(()),
            Expr::BlockComment(_) => Ok(()),
            Expr::Variable(name) => {
                let variable_symbol = format!("let.{}", name);

                // First, check if the symbol exists
                let symbol_exists = {
                    let borrowed_symbol_table = symbol_table.as_ref().borrow();
                    borrowed_symbol_table.lookup(&variable_symbol).is_some()
                };

                if symbol_exists {
                    return Ok(());
                }
                // Add the symbol in a separate scope
                {
                    let mut borrowed_symbol_table = symbol_table.as_ref().borrow_mut();
                    borrowed_symbol_table.add_symbol(
                        variable_symbol.clone(),
                        Symbol::Identifier {
                            name: name.clone(),
                            type_: ast::Type::new(name.clone(), ast::Expr::Nil, None),
                            value: String::new(), // TODO: how do I put a value here?? later stage maybe?
                        },
                    );
                }
                Ok(())
            }
            Expr::FunctionCall { name, args } => {
                let fn_call_symbol = format!("fn.{}", name);
                let symbol = symbol_table.as_ref().borrow_mut().lookup(&fn_call_symbol);
                match symbol {
                    Some(_) => {
                        for arg in args {
                            self.collect_expr_symbols(symbol_table, *arg.clone())?;
                        }
                    }
                    None => {
                        self.forward_references_symbol_table.as_ref().borrow_mut().add_symbol(
                            fn_call_symbol.clone(),
                            Symbol::FunctionCall {
                                name: name.clone(),
                                arguments: None, // TODO: this requires binding in the forward references pass
                            },
                        );
                    }
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
                let borrowed_symbol_table = symbol_table.borrow();
                let symbol = borrowed_symbol_table.lookup_local_scope(&array_symbol);
                match symbol {
                    Some(_) => self.collect_expr_symbols(symbol_table, *index),
                    None => Err(format!(
                        "error: identifier `{}`for array access not declared",
                        array_symbol
                    )),
                }
            }
            Expr::StructAccess { name, field } => {
                // search in the global table if the struct is declared
                let struct_symbol = format!("struct.{}", name);
                let symbol = symbol_table.as_ref().borrow_mut().lookup(&struct_symbol);
                match symbol {
                    Some(_) => self.collect_expr_symbols(symbol_table, *field),
                    None => Err(format!("error: struct `{}` not declared", name)),
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
                let symbol = symbol_table.as_ref().borrow_mut().lookup(&variable_symbol);
                match symbol {
                    Some(_) => self.collect_expr_symbols(symbol_table, *value),
                    None => Err(format!("error: identifier `{}` not declared", name)),
                }
            }
            Expr::StructInstantiation { name, members } => {
                // search in the global table if the struct is declared
                let struct_symbol = format!("struct.{}", name);
                let symbol = symbol_table.as_ref().borrow_mut().lookup(&struct_symbol);
                match symbol {
                    Some(_) => {
                        for (_, member) in members {
                            self.collect_expr_symbols(symbol_table, *member.clone())?;
                        }
                        Ok(())
                    }
                    None => {
                        // this is a potential instantiation of a struct where the struct definition
                        // is not yet defined
                        self.forward_references_symbol_table.as_ref().borrow_mut().add_symbol(
                            struct_symbol.clone(),
                            Symbol::Struct {
                                is_public: false,
                                name: name.clone(),
                                type_: ast::Type::new(name.clone(), ast::Expr::Struct(name.clone()), None),
                                table: Rc::new(RefCell::new(SymbolTable::new(name.clone(), None))),
                            },
                        );
                        Ok(())
                    }
                }
            }
            Expr::Int(_) => Ok(()),
            Expr::Float(_) => Ok(()),
            Expr::Str(_) => Ok(()),
            Expr::Bool(_) => Ok(()),
            Expr::Char(_) => Ok(()),
            Expr::Nil => Ok(()),
            Expr::Struct(_) => Ok(()),
            Expr::Interface(_) => Ok(()),
            Expr::Enum(_) => Ok(()),
        }
    }

    // forward references pass to check if all symbols are defined
    pub fn forward_references_pass(&mut self) -> Result<(), Vec<String>> {
        let result: Vec<String> = self
            .forward_references_symbol_table
            .borrow()
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
}

impl fmt::Display for SemanticAnalyzer {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.global_symbol_table.debug_print_table())
    }
}
