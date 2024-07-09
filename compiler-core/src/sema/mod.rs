mod sym;

use std::{
    cell::RefCell,
    fmt::{self, Formatter},
    rc::Rc,
};
use uuid::Uuid;

use sym::{GlobalSymbolTable, Symbol, SymbolTable};

use crate::parser::ast::{self, Stmt, AST};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    global_symbol_table: GlobalSymbolTable,
    forward_references_symbol_table: SymbolTable,
}

impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer {
            global_symbol_table: GlobalSymbolTable::new(),
            forward_references_symbol_table: SymbolTable::new("forward_references".to_string(), None),
        }
    }

    // first pass to collect as many symbols as possible
    pub fn symbols_collection(&mut self, ast: AST) -> Result<(), Vec<String>> {
        let mut errors: Vec<String> = Vec::new();
        for sourcefile in ast.files {
            let mut current_package = String::new();
            println!("Symbols collection for file `{:?}`....", sourcefile.name);
            for stmt in sourcefile.statements {
                let s = &**stmt;
                // if the symbol_table is None it means we are using the global scope
                let result = self.build_symbol_table(s.clone(), &mut current_package, &None);
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

    fn build_symbol_table(
        &mut self,
        stmt: Stmt,
        current_package: &mut String,
        symbol_table: &Option<Rc<RefCell<SymbolTable>>>,
    ) -> Result<(), String> {
        match stmt {
            Stmt::Literal(_) => Ok(()),
            Stmt::Comment(_) => Ok(()),
            Stmt::BlockComment(_) => Ok(()),
            // TODO: match is not yet implemented
            Stmt::MatchStmt { value, arms } => Ok(()),
            Stmt::Empty => Ok(()),
            Stmt::Identifier { name, type_ } => {
                match symbol_table {
                    Some(table) => {
                        if table.as_ref().borrow().lookup(&name).is_none() {
                            let ident_symbol = format!("identifier.{}", name);
                            self.forward_references_symbol_table.add_symbol(
                                ident_symbol.clone(),
                                Symbol::Identifier {
                                    name: name.clone(),
                                    type_: type_.clone(),
                                    value: String::new(), // TODO: how do I put a value here?? later stage maybe?
                                },
                            );
                        }
                        table.as_ref().borrow_mut().add_symbol(
                            name.clone(),
                            Symbol::Identifier {
                                name: name.clone(),
                                type_: type_.clone(),
                                value: String::new(), // TODO: how do I put a value here?? later stage maybe?
                            },
                        );
                    }
                    None => {
                        Err(format!("error: symbol table not found"))?;
                    }
                }
                Ok(())
            }
            Stmt::Interface { name, type_, methods } => {
                let interface_symbol = format!("interface.{}", name);
                if self.global_symbol_table.lookup_symbol(&interface_symbol).is_none() {
                    let package_symbol_table = self
                        .global_symbol_table
                        .get_symbol_table_by_name(current_package.as_str());
                    match package_symbol_table {
                        Some(table) => {
                            let interface_functions_signatures_symbol_table =
                                Rc::new(RefCell::new(SymbolTable::new(name.clone(), Some(table.clone()))));

                            for method in methods {
                                let m = &**method;
                                self.build_symbol_table(
                                    m.clone(),
                                    current_package,
                                    &Some(interface_functions_signatures_symbol_table.clone()),
                                )?;
                            }

                            table.as_ref().borrow_mut().add_symbol(
                                name.clone(),
                                Symbol::Interface {
                                    name: name.clone(),
                                    table: interface_functions_signatures_symbol_table,
                                },
                            );
                        }
                        None => {
                            Err(format!("error: could not find current package symbol table"))?;
                        }
                    }
                } else {
                    Err(format!("error: interface `{}` already declared", name))?;
                }

                Ok(())
            }
            Stmt::InterfaceFunctionSignature {
                name,
                generics,
                parameters,
                return_type,
            } => {
                if symbol_table.is_none() {
                    Err(format!("error: missing interface symbol table"))?;
                }

                let table = symbol_table.as_ref().unwrap();
                if table.as_ref().borrow().lookup(&name).is_some() {
                    Err(format!("error: method `{}` in interface already declared", name))?;
                }
                table.as_ref().borrow_mut().add_symbol(
                    name.clone(),
                    Symbol::InterfaceMethod {
                        name: name.clone(),
                        parameters: parameters.clone(),
                        return_type: return_type.clone(),
                    },
                );
                Ok(())
            }
            Stmt::FunctionCall { name, args } => {
                match symbol_table {
                    Some(table) => {
                        let fn_symbol = format!("fn.{}", name);
                        if table.as_ref().borrow().lookup(&fn_symbol).is_none() {
                            self.forward_references_symbol_table.add_symbol(
                                fn_symbol.clone(),
                                Symbol::FunctionCall {
                                    name: name.clone(),
                                    arguments: None, // TODO: this requires binding in the forward references pass
                                },
                            );
                        }
                        for arg in args {
                            let a = &**arg;
                            self.build_symbol_table(a.clone(), current_package, &Some(table.clone()))?;
                        }
                    }
                    None => {
                        Err(format!("error: symbol table not found"))?;
                    }
                }
                Ok(())
            }
            Stmt::ArrayInitialization { elements } => {
                for element in elements {
                    let e = &**element;
                    self.build_symbol_table(e.clone(), current_package, symbol_table)?;
                }
                Ok(())
            }
            Stmt::ArrayAccess { name, index } => {
                match symbol_table {
                    Some(table) => {
                        if table.as_ref().borrow().lookup(&name).is_none() {
                            Err(format!(
                                "error: identifier `{}` for array access not declared",
                                name.clone()
                            ))?;
                        }
                        let s = &**index;
                        self.build_symbol_table(s.clone(), current_package, symbol_table)?;
                    }
                    None => {
                        Err(format!("error: symbol table not found"))?;
                    }
                }
                Ok(())
            }
            Stmt::StructAccess { name, field } => {
                // search in the global table if the struct is declared
                // let struct_symbol = format!("struct.{}", name);
                if self.global_symbol_table.lookup_symbol(&name).is_none() {
                    Err(format!("error: struct `{}` not declared", name))?;
                }
                // since we need to access the field of the struct we need to find the symbol table
                if symbol_table.is_none() {
                    Err(format!("error: missing struct `{}` symbol table", name))?;
                }

                let struct_field = &**field;
                self.build_symbol_table(struct_field.clone(), current_package, symbol_table)
            }
            Stmt::PrefixOp { op, expr } => {
                // TODO: what to do with op?
                let e = &**expr;

                println!("`prefix op` --> {}", e);

                self.build_symbol_table(e.clone(), current_package, symbol_table)
            }
            Stmt::PostfixOp { op, expr } => {
                // TODO: what to do with op?
                let e = &**expr;
                self.build_symbol_table(e.clone(), current_package, symbol_table)
            }
            Stmt::InfixOp { op, lhs, rhs } => {
                // TODO: what to do with op?
                let l = &**lhs;
                self.build_symbol_table(l.clone(), current_package, symbol_table)?;

                let r = &**rhs;
                self.build_symbol_table(r.clone(), current_package, symbol_table)?;

                Ok(())
            }
            Stmt::Defer { stmt } => {
                let s = &**stmt;
                self.build_symbol_table(s.clone(), current_package, symbol_table)
            }
            Stmt::Let { name, statement } => {
                // TODO: need to validate that this is the correct name for the symbol
                match symbol_table {
                    Some(table) => {
                        // we need to check if there is a redeclaration of the identifier
                        // otherwise it is shadowing
                        if table.as_ref().borrow().lookup_local_scope(&name).is_some() {
                            Err(format!("error: identifier `{}` already declared", name))?;
                        }
                        table.as_ref().borrow_mut().add_symbol(
                            name.clone(),
                            Symbol::Identifier {
                                name: name.clone(),
                                type_: ast::Type {
                                    is_array: false,
                                    name: String::from("unknown"),
                                    generics: None,
                                },
                                value: String::new(),
                            },
                        );
                    }
                    None => {
                        Err(format!("error: missing symbol table",))?;
                    }
                }

                let s = &**statement;
                self.build_symbol_table(s.clone(), current_package, symbol_table)
            }
            Stmt::Assignment { name, value } => {
                match symbol_table {
                    Some(table) => {
                        if table.as_ref().borrow().lookup(&name).is_none() {
                            Err(format!("error: identifier `{}` not declared", name))?;
                        }
                        // TODO: add value in the table
                    }
                    None => {
                        Err(format!("error: symbol table not found"))?;
                    }
                }
                let s = &**value;
                self.build_symbol_table(s.clone(), current_package, symbol_table)
            }
            Stmt::IfStmt {
                condition,
                body,
                else_stmt,
            } => {
                let if_id = format!("if.{}", Uuid::new_v4().to_string());
                match symbol_table {
                    Some(table) => {
                        let c = &**condition;
                        self.build_symbol_table(c.clone(), current_package, symbol_table)?;

                        let then_symbol_table =
                            Rc::new(RefCell::new(SymbolTable::new(if_id.clone(), symbol_table.clone())));

                        for stmt in body {
                            let s = &**stmt;
                            self.build_symbol_table(s.clone(), current_package, &Some(then_symbol_table.clone()))?;
                        }

                        let else_id = format!("else.{}", Uuid::new_v4().to_string());
                        let mut else_symbol_table = None;
                        match else_stmt {
                            Some(else_stmt) => {
                                else_symbol_table = Some(Rc::new(RefCell::new(SymbolTable::new(
                                    else_id.clone(),
                                    symbol_table.clone(),
                                ))));

                                let e = &**else_stmt;
                                self.build_symbol_table(e.clone(), current_package, &else_symbol_table.clone())?;
                            }
                            None => {}
                        }

                        table.as_ref().borrow_mut().add_symbol(
                            if_id.clone(),
                            Symbol::IfStatement {
                                condition: String::new(), // TODO: is this correct?
                                then_body: then_symbol_table.clone(),
                                else_body: if else_symbol_table.is_some() {
                                    else_symbol_table.clone()
                                } else {
                                    None
                                },
                            },
                        );
                    }
                    None => {
                        Err(format!("error: symbol table not found"))?;
                    }
                }

                Ok(())
            }
            Stmt::RangeStmt { iterator, range, body } => {
                // TODO: does this need to go to the parent symbol table or
                // the one that is being created?
                let r = &**range;
                self.build_symbol_table(r.clone(), current_package, symbol_table)?;

                let range_id = Uuid::new_v4().to_string();
                let range_symbol_table = Rc::new(RefCell::new(SymbolTable::new(
                    format!("range.{}", range_id),
                    symbol_table.clone(),
                )));

                range_symbol_table.as_ref().borrow_mut().add_symbol(
                    iterator.clone(),
                    Symbol::Identifier {
                        name: iterator.clone(),
                        type_: ast::Type {
                            is_array: false,
                            name: String::from("unknown"),
                            generics: None,
                        },
                        value: String::new(),
                    },
                );

                for stmt in body {
                    let s = &**stmt;
                    self.build_symbol_table(s.clone(), current_package, &Some(range_symbol_table.clone()))?;
                }

                match symbol_table {
                    Some(table) => {
                        table.as_ref().borrow_mut().add_symbol(
                            range_id.clone(),
                            Symbol::RangeLoop {
                                iterator,
                                iterable: String::new(),
                                body: range_symbol_table,
                            },
                        );
                    }
                    None => {
                        Err(format!("error: symbol table not found"))?;
                    }
                }
                Ok(())
            }
            Stmt::WhileStmt { condition, body } => {
                // TODO: does this need to go to the parent symbol table or
                // the one that is being created?
                let c = &**condition;
                self.build_symbol_table(c.clone(), current_package, symbol_table)?;

                let while_id = Uuid::new_v4().to_string();
                let while_symbol_table = Rc::new(RefCell::new(SymbolTable::new(
                    format!("while.{}", while_id),
                    symbol_table.clone(),
                )));

                for stmt in body {
                    let s = &**stmt;
                    self.build_symbol_table(s.clone(), current_package, &Some(while_symbol_table.clone()))?;
                }

                match symbol_table {
                    Some(table) => {
                        table.as_ref().borrow_mut().add_symbol(
                            while_id.clone(),
                            Symbol::WhileLoop {
                                condition: String::new(), // TODO: is this correct?
                                body: while_symbol_table,
                            },
                        );
                    }
                    None => {
                        Err(format!("error: symbol table not found"))?;
                    }
                }

                Ok(())
            }
            Stmt::Block { stmts } => {
                let block_id = format!("block.{}", Uuid::new_v4().to_string());
                let block_symbol_table = Some(Rc::new(RefCell::new(SymbolTable::new(
                    block_id.clone(),
                    symbol_table.clone(),
                ))));

                for stmt in stmts {
                    let s = &**stmt;
                    self.build_symbol_table(s.clone(), current_package, &block_symbol_table.clone())?;
                }

                match symbol_table {
                    Some(table) => {
                        table
                            .as_ref()
                            .borrow_mut()
                            .add_symbol(block_id.clone(), Symbol::Block(block_symbol_table));
                    }
                    None => {
                        Err(format!("error: symbol table not found"))?;
                    }
                }

                Ok(())
            }
            Stmt::Return { value } => {
                for val in value {
                    let v = &**val;
                    println!("`return stmt` --> {}", v);
                    self.build_symbol_table(v.clone(), current_package, symbol_table)?;
                }
                Ok(())
            }
            Stmt::Enum {
                is_public,
                name,
                members,
            } => {
                let enum_symbol = format!("enum.{}", name);
                if self.global_symbol_table.lookup_symbol(&enum_symbol).is_none() {
                    let package_symbol_table = self
                        .global_symbol_table
                        .get_symbol_table_by_name(current_package.as_str());
                    match package_symbol_table {
                        Some(table) => {
                            let enum_member_symbol_table =
                                Rc::new(RefCell::new(SymbolTable::new(name.clone(), Some(table.clone()))));

                            for member in members {
                                if enum_member_symbol_table.as_ref().borrow().lookup(&member).is_some() {
                                    Err(format!("error: enum member `{}` already declared", member))?;
                                }

                                enum_member_symbol_table.as_ref().borrow_mut().add_symbol(
                                    member.clone(),
                                    Symbol::EnumMember {
                                        name: member.clone(),
                                        value: String::new(), // TODO: increase a sort of counter for the enums
                                    },
                                );
                            }

                            table.as_ref().borrow_mut().add_symbol(
                                enum_symbol.clone(),
                                Symbol::Enum {
                                    is_public,
                                    name: name.clone(),
                                    table: enum_member_symbol_table,
                                },
                            );
                        }
                        None => {
                            Err(format!("error: package `{}` symbol table not found", current_package))?;
                        }
                    }
                } else {
                    Err(format!("error: enum `{}` already declared", name))?;
                }
                Ok(())
            }
            Stmt::StructDeclaration {
                is_public,
                name,
                type_,
                members,
            } => {
                let struct_symbol = format!("struct.{}", name);
                if self.global_symbol_table.lookup_symbol(&struct_symbol).is_none() {
                    let package_symbol_table = self
                        .global_symbol_table
                        .get_symbol_table_by_name(current_package.as_str());
                    match package_symbol_table {
                        Some(table) => {
                            let struct_members_table =
                                Rc::new(RefCell::new(SymbolTable::new(name.clone(), Some(table.clone()))));

                            for member in members {
                                let m = &**member;
                                self.build_symbol_table(
                                    m.clone(),
                                    current_package,
                                    &Some(struct_members_table.clone()),
                                )?;
                            }

                            table.as_ref().borrow_mut().add_symbol(
                                struct_symbol.clone(),
                                Symbol::Struct {
                                    is_public,
                                    name: name.clone(),
                                    table: struct_members_table,
                                },
                            );
                        }
                        None => {
                            Err(format!("error: package `{}` symbol table not found", current_package))?;
                        }
                    }
                } else {
                    Err(format!("error: struct `{}` already declared", name))?;
                }

                Ok(())
            }
            Stmt::StructMember { is_public, name, type_ } => {
                if symbol_table.is_none() {
                    Err(format!("error: missing struct symbol table"))?;
                }

                let table = symbol_table.as_ref().unwrap();
                if table.as_ref().borrow_mut().lookup(&name).is_some() {
                    Err(format!("error: struct member `{}` already declared", name))?;
                }
                table.as_ref().borrow_mut().add_symbol(
                    name.clone(),
                    Symbol::StructMember {
                        is_public,
                        name: name.clone(),
                        type_: String::new(), // TODO: how do I save the type???
                    },
                );

                Ok(())
            }
            Stmt::StructInstantiation { name, members } => {
                // search in the global table if the struct is declared
                let struct_symbol = format!("struct.{}", name);
                if self.global_symbol_table.lookup_symbol(&struct_symbol).is_none() {
                    // this is a potential instantiation of a struct where the struct definition
                    // is not yet defined
                    self.forward_references_symbol_table.add_symbol(
                        struct_symbol.clone(),
                        Symbol::Struct {
                            is_public: false,
                            name: name.clone(),
                            table: Rc::new(RefCell::new(SymbolTable::new(name.clone(), None))),
                        },
                    );
                }

                for (name, member) in members {
                    let m = &**member;
                    self.build_symbol_table(m.clone(), current_package, symbol_table)?;
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
            } => self.analyse_fn_declaration(
                name,
                symbol_table,
                body,
                current_package,
                is_public,
                parameters,
                return_type,
            ),
            Stmt::ImplDeclaration {
                name,
                generics,
                interfaces,
                methods,
            } => {
                let impl_symbol = format!("impl.{}", name);
                if self.global_symbol_table.lookup_symbol(&impl_symbol).is_none() {
                    let package_symbol_table = self
                        .global_symbol_table
                        .get_symbol_table_by_name(current_package.as_str());
                    match package_symbol_table {
                        Some(table) => {
                            let impl_methods_table =
                                Rc::new(RefCell::new(SymbolTable::new(name.clone(), Some(table.clone()))));

                            for method in methods {
                                let m = &**method;
                                self.build_symbol_table(m.clone(), current_package, &Some(impl_methods_table.clone()))?;
                            }

                            table.as_ref().borrow_mut().add_symbol(
                                impl_symbol.clone(),
                                Symbol::Impl {
                                    name: name.clone(),
                                    interfaces: interfaces.clone(),
                                    table: impl_methods_table,
                                },
                            );
                        }
                        None => {
                            Err(format!("error: package `{}` symbol table not found", current_package))?;
                        }
                    }
                } else {
                    Err(format!("error: impl `{}` already declared", name))?;
                }

                Ok(())
            }
            Stmt::Constant { name, type_, value } => {
                let constant_symbol = format!("const.{}", name);
                if self.global_symbol_table.lookup_symbol(&constant_symbol).is_none() {
                    let package_symbol_table = self
                        .global_symbol_table
                        .get_symbol_table_by_name(current_package.as_str());
                    match package_symbol_table {
                        Some(scope) => {
                            scope.as_ref().borrow_mut().add_symbol(
                                constant_symbol.clone(),
                                Symbol::Constant {
                                    name: name.clone(),
                                    type_: type_.clone(),
                                    value: String::new(), // TODO: how do I save the value???
                                },
                            );
                        }
                        None => {
                            Err(format!("error: package `{}` symbol table not found", current_package))?;
                        }
                    }
                } else {
                    Err(format!("error: constant `{}` already declared", name))?;
                }
                Ok(())
            }
            Stmt::ConstantGroup { constants } => {
                for constant in constants {
                    let s = &**constant;
                    self.build_symbol_table(s.clone(), current_package, &None)?;
                }
                Ok(())
            }
            Stmt::PackageDeclaration { name } => {
                let pkg_symbol = format!("pkg.{}", name);
                if self.global_symbol_table.lookup_symbol(&pkg_symbol).is_none() {
                    self.global_symbol_table.new_package(pkg_symbol.clone(), name.clone());
                    *current_package = name.clone();
                } else {
                    *current_package = name.clone();
                }
                Ok(())
            }
            Stmt::UseDeclaration { name } => {
                let use_symbol = format!("use.{}", name);
                if self.global_symbol_table.lookup_symbol(&use_symbol).is_none() {
                    let package_symbol_table = self
                        .global_symbol_table
                        .get_symbol_table_by_name(current_package.as_str());
                    match package_symbol_table {
                        Some(scope) => {
                            scope
                                .as_ref()
                                .borrow_mut()
                                .add_symbol(use_symbol.clone(), Symbol::Use(name.clone()));
                        }
                        None => {
                            Err(format!("error: package `{}` symbol table not found", current_package))?;
                        }
                    }
                }
                Ok(())
            }
        }
    }

    fn analyse_fn_declaration(
        &mut self,
        name: String,
        symbol_table: &Option<Rc<RefCell<SymbolTable>>>,
        body: Vec<Rc<std::sync::Arc<Stmt>>>,
        current_package: &mut String,
        is_public: bool,
        parameters: Option<Vec<(String, ast::Type)>>,
        return_type: Option<Vec<ast::Type>>,
    ) -> Result<(), String> {
        let function_symbol = format!("fn.{}", name);
        match symbol_table {
            // we are inside an `impl` block
            Some(table) => {
                if table.as_ref().borrow().lookup(&function_symbol).is_none() {
                    let fn_body_table = Rc::new(RefCell::new(SymbolTable::new(name.clone(), Some(table.clone()))));

                    if parameters.is_some() {
                        for (name, type_) in &parameters.clone().unwrap() {
                            fn_body_table.as_ref().borrow_mut().add_symbol(
                                name.clone(),
                                Symbol::FunctionParameter {
                                    name: name.clone(),
                                    type_: type_.clone(),
                                },
                            );
                        }
                    }

                    for stmt in body {
                        let s = &**stmt;
                        self.build_symbol_table(s.clone(), current_package, &Some(fn_body_table.clone()))?;
                    }

                    table.as_ref().borrow_mut().add_symbol(
                        function_symbol.clone(),
                        Symbol::Function {
                            is_public,
                            name: name.clone(),
                            parameters: parameters.clone(),
                            return_type: return_type.clone(),
                            body: fn_body_table,
                        },
                    );
                } else {
                    Err(format!("error: function `{}` already declared", name))?;
                }
            }
            // we are in the global scope
            None => {
                if self.global_symbol_table.lookup_symbol(&function_symbol).is_none() {
                    let package_symbol_table = self
                        .global_symbol_table
                        .get_symbol_table_by_name(current_package.as_str());
                    match package_symbol_table {
                        Some(table) => {
                            let fn_body_table =
                                Rc::new(RefCell::new(SymbolTable::new(name.clone(), Some(table.clone()))));

                            if parameters.is_some() {
                                for (name, type_) in &parameters.clone().unwrap() {
                                    fn_body_table.as_ref().borrow_mut().add_symbol(
                                        name.clone(),
                                        Symbol::FunctionParameter {
                                            name: name.clone(),
                                            type_: type_.clone(),
                                        },
                                    );
                                }
                            }
                            for stmt in body {
                                let s = &**stmt;

                                println!("`fn body stmt` --> {}", s);

                                self.build_symbol_table(s.clone(), current_package, &Some(fn_body_table.clone()))?;
                            }

                            table.as_ref().borrow_mut().add_symbol(
                                function_symbol.clone(),
                                Symbol::Function {
                                    is_public,
                                    name: name.clone(),
                                    parameters: parameters.clone(),
                                    return_type: return_type.clone(),
                                    body: fn_body_table,
                                },
                            );
                        }
                        None => {
                            Err(format!("error: package `{}` symbol table not found", current_package))?;
                        }
                    }
                } else {
                    Err(format!("error: function `{}` already declared", name))?;
                }
            }
        }
        Ok(())
    }

    // forward references pass to check if all symbols are defined
    pub fn forward_references(&mut self) -> Result<(), Vec<String>> {
        let result: Vec<String> = self
            .forward_references_symbol_table
            .get_symbols()
            .iter()
            .map(|(symbol_name, symbol)| {
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

    pub fn type_check(&mut self) -> Result<(), Vec<String>> {
        Ok(())
    }
}

impl fmt::Display for SemanticAnalyzer {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.global_symbol_table.debug_print_table())
    }
}
