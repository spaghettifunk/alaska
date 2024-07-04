mod sym;

use std::{
    cell::RefCell,
    fmt::{self, format, Formatter},
    rc::Rc,
};
use uuid::Uuid;

use sym::{GlobalSymbolTable, Symbol, SymbolTable};

use crate::parser::ast::{Stmt, AST};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    global_symbol_table: GlobalSymbolTable,
    current_package: String,
}

impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer {
            global_symbol_table: GlobalSymbolTable::new(),
            current_package: String::new(),
        }
    }

    pub fn symbols_collection(&mut self, ast: AST) -> Result<(), String> {
        for sourcefile in ast.files {
            println!("Symbols collection for file `{:?}`....", sourcefile.name);
            for stmt in sourcefile.statements {
                let s = &**stmt;
                // if the symbol_table is None it means we are using the global scope
                let result = self.build_symbol_table(s.clone(), &None);
                match result {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("Error: {}", e);
                        std::process::exit(1);
                    }
                }
            }
        }
        Ok(())
    }

    fn build_symbol_table(
        &mut self,
        stmt: Stmt,
        symbol_table: &Option<Rc<RefCell<SymbolTable>>>,
    ) -> Result<(), String> {
        match stmt {
            Stmt::Literal(_) => Ok(()),
            Stmt::Comment { text } => Ok(()),
            Stmt::BlockComment { text } => Ok(()),
            Stmt::Identifier(name) => {
                match symbol_table {
                    Some(table) => {
                        if table.as_ref().borrow().lookup(&name).is_some() {
                            Err(format!("error: identifier already declared"))?;
                        }
                        table
                            .as_ref()
                            .borrow_mut()
                            .add_symbol(name.clone(), Symbol::Identifier(name));
                    }
                    None => {
                        Err(format!("error: global scope not found"))?;
                    }
                }
                Ok(())
            }
            Stmt::Interface { name, type_, methods } => {
                let interface_symbol = format!("interface.{}", name);
                if self.global_symbol_table.lookup_symbol(&interface_symbol).is_none() {
                    let package_symbol_table = self
                        .global_symbol_table
                        .get_symbol_table_by_name(self.current_package.as_str());
                    match package_symbol_table {
                        Some(table) => {
                            let interface_functions_signatures_symbol_table = Rc::new(RefCell::new(SymbolTable::new(
                                interface_symbol.clone(),
                                Some(table.clone()),
                            )));

                            for method in methods {
                                let m = &**method;
                                self.build_symbol_table(
                                    m.clone(),
                                    &Some(interface_functions_signatures_symbol_table.clone()),
                                )?;
                            }

                            table.borrow_mut().add_symbol(
                                interface_symbol.clone(),
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
                    Err(format!("error: interface already declared"))?;
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
                if table.borrow_mut().lookup(&name).is_some() {
                    Err(format!("error: method in interface already declared"))?;
                }
                table.borrow_mut().add_symbol(
                    name.clone(),
                    Symbol::InterfaceMethod {
                        name: name.clone(),
                        parameters: parameters.clone(),
                        return_type: return_type.clone(),
                    },
                );

                Ok(())
            }
            Stmt::FunctionCall { name, args } => todo!(),
            Stmt::Array { elements } => todo!(),
            Stmt::ArrayAccess { name, index } => todo!(),
            Stmt::StructAccess { name, field } => todo!(),
            Stmt::PrefixOp { op, expr } => todo!(),
            Stmt::InfixOp { op, lhs, rhs } => todo!(),
            Stmt::PostfixOp { op, expr } => todo!(),
            Stmt::Defer { stmt } => {
                let s = &**stmt;
                self.build_symbol_table(s.clone(), symbol_table)
            }
            Stmt::Let { identifier, statement } => {
                // TODO: need to validate that this is the correct name for the symbol
                let let_symbol = format!("let.{}", identifier);
                match symbol_table {
                    Some(table) => {
                        if table.as_ref().borrow().lookup(&let_symbol).is_some() {
                            Err(format!("error: identifier `{}`  already declared", identifier))?;
                        }
                        table
                            .borrow_mut()
                            .add_symbol(let_symbol.clone(), Symbol::Identifier(identifier));
                    }
                    None => {
                        Err(format!("error: missing symbol table",))?;
                    }
                }

                let s = &**statement;
                self.build_symbol_table(s.clone(), symbol_table)
            }
            Stmt::Assignment { name, value } => {
                match symbol_table {
                    Some(table) => {
                        if table.as_ref().borrow().lookup(&name).is_none() {
                            Err(format!("error: identifier not declared"))?;
                        }
                        // TODO: add value in the table
                    }
                    None => {
                        Err(format!("error: symbol table not found"))?;
                    }
                }
                let s = &**value;
                self.build_symbol_table(s.clone(), symbol_table)
            }
            Stmt::IfStmt {
                condition,
                body,
                else_stmt,
            } => {
                let c = &**condition;
                self.build_symbol_table(c.clone(), symbol_table)?;

                for stmt in body {
                    let s = &**stmt;
                    self.build_symbol_table(s.clone(), symbol_table)?;
                }

                match else_stmt {
                    Some(else_stmt) => {
                        let e = &**else_stmt;
                        self.build_symbol_table(e.clone(), symbol_table)?;
                    }
                    None => {}
                }
                Ok(())
            }
            Stmt::RangeStmt { iterator, range, body } => {
                // TODO: does this need to go to the parent symbol table or
                // the one that is being created?
                let r = &**range;
                self.build_symbol_table(r.clone(), symbol_table)?;

                let range_id = Uuid::new_v4().to_string();
                let range_symbol_table = Rc::new(RefCell::new(SymbolTable::new(
                    format!("range.{}", range_id),
                    symbol_table.clone(),
                )));

                range_symbol_table
                    .borrow_mut()
                    .add_symbol(iterator.clone(), Symbol::Identifier(iterator.clone()));

                for stmt in body {
                    let s = &**stmt;
                    self.build_symbol_table(s.clone(), &Some(range_symbol_table.clone()))?;
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
                self.build_symbol_table(c.clone(), symbol_table)?;

                let while_id = Uuid::new_v4().to_string();
                let while_symbol_table = Rc::new(RefCell::new(SymbolTable::new(
                    format!("while.{}", while_id),
                    symbol_table.clone(),
                )));

                for stmt in body {
                    let s = &**stmt;
                    self.build_symbol_table(s.clone(), &Some(while_symbol_table.clone()))?;
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
            Stmt::MatchStmt { value, arms } => todo!(),
            Stmt::Block { stmts } => todo!(),
            Stmt::Return { value } => {
                for val in value {
                    let v = &**val;
                    self.build_symbol_table(v.clone(), symbol_table)?;
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
                        .get_symbol_table_by_name(self.current_package.as_str());
                    match package_symbol_table {
                        Some(table) => {
                            let enum_member_symbol_table =
                                Rc::new(RefCell::new(SymbolTable::new(enum_symbol.clone(), Some(table.clone()))));

                            for member in members {
                                if enum_member_symbol_table.as_ref().borrow().lookup(&member).is_some() {
                                    Err(format!("error: enum member already declared"))?;
                                }

                                enum_member_symbol_table.borrow_mut().add_symbol(
                                    member.clone(),
                                    Symbol::EnumMember {
                                        name: member.clone(),
                                        value: String::new(), // TODO: increase a sort of counter for the enums
                                    },
                                );
                            }

                            table.borrow_mut().add_symbol(
                                enum_symbol.clone(),
                                Symbol::Enum {
                                    is_public,
                                    name: name.clone(),
                                    table: enum_member_symbol_table,
                                },
                            );
                        }
                        None => {
                            Err(format!("error: package symbol table not found"))?;
                        }
                    }
                } else {
                    Err(format!("error: enum already declared"))?;
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
                        .get_symbol_table_by_name(self.current_package.as_str());
                    match package_symbol_table {
                        Some(table) => {
                            let struct_members_table = Rc::new(RefCell::new(SymbolTable::new(
                                struct_symbol.clone(),
                                Some(table.clone()),
                            )));

                            for member in members {
                                let m = &**member;
                                self.build_symbol_table(m.clone(), &Some(struct_members_table.clone()))?;
                            }

                            table.borrow_mut().add_symbol(
                                struct_symbol.clone(),
                                Symbol::Struct {
                                    is_public,
                                    name: name.clone(),
                                    table: struct_members_table,
                                },
                            );
                        }
                        None => {
                            Err(format!("error: package symbol table not found"))?;
                        }
                    }
                } else {
                    Err(format!("error: struct already declared"))?;
                }

                Ok(())
            }
            Stmt::StructMember { is_public, name, type_ } => {
                if symbol_table.is_none() {
                    Err(format!("error: missing struct symbol table"))?;
                }

                let table = symbol_table.as_ref().unwrap();
                if table.borrow_mut().lookup(&name).is_some() {
                    Err(format!("error: struct member already declared"))?;
                }
                table.borrow_mut().add_symbol(
                    name.clone(),
                    Symbol::StructMember {
                        is_public,
                        name: name.clone(),
                        type_: String::new(), // TODO: how do I save the type???
                    },
                );

                Ok(())
            }
            Stmt::StructInstantiation { name, members } => todo!(),
            Stmt::FunctionDeclaration {
                is_public,
                name,
                generics,
                parameters,
                body,
                return_type,
            } => {
                let function_symbol = format!("func.{}", name);
                if self.global_symbol_table.lookup_symbol(&function_symbol).is_none() {
                    let package_symbol_table = self
                        .global_symbol_table
                        .get_symbol_table_by_name(self.current_package.as_str());
                    match package_symbol_table {
                        Some(table) => {
                            let fn_body_table = Rc::new(RefCell::new(SymbolTable::new(
                                function_symbol.clone(),
                                Some(table.clone()),
                            )));

                            for stmt in body {
                                let s = &**stmt;
                                self.build_symbol_table(s.clone(), &Some(fn_body_table.clone()))?;
                            }

                            table.borrow_mut().add_symbol(
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
                            Err(format!("error: package symbol table not found"))?;
                        }
                    }
                } else {
                    Err(format!("error: function already declared"))?;
                }

                Ok(())
            }
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
                        .get_symbol_table_by_name(self.current_package.as_str());
                    match package_symbol_table {
                        Some(table) => {
                            let impl_methods_table =
                                Rc::new(RefCell::new(SymbolTable::new(impl_symbol.clone(), Some(table.clone()))));

                            for method in methods {
                                let m = &**method;
                                self.build_symbol_table(m.clone(), &Some(impl_methods_table.clone()))?;
                            }

                            table.borrow_mut().add_symbol(
                                impl_symbol.clone(),
                                Symbol::Impl {
                                    name: name.clone(),
                                    interfaces: interfaces.clone(),
                                    table: impl_methods_table,
                                },
                            );
                        }
                        None => {
                            Err(format!("error: package symbol table not found"))?;
                        }
                    }
                } else {
                    Err(format!("error: impl already declared"))?;
                }

                Ok(())
            }
            Stmt::Constant { name, type_, value } => {
                let constant_symbol = format!("const.{}", name);
                if self.global_symbol_table.lookup_symbol(&constant_symbol).is_none() {
                    let current_scope = self
                        .global_symbol_table
                        .get_symbol_table_by_name(self.current_package.as_str());
                    match current_scope {
                        Some(scope) => {
                            scope.borrow_mut().add_symbol(
                                constant_symbol.clone(),
                                Symbol::Constant {
                                    name: name.clone(),
                                    type_: type_.clone(),
                                    value: String::new(), // TODO: how do I save the value???
                                },
                            );
                        }
                        None => {
                            Err(format!("error: global scope not found"))?;
                        }
                    }
                } else {
                    Err(format!("error: constant already declared"))?;
                }
                Ok(())
            }
            Stmt::ConstantGroup { constants } => {
                for constant in constants {
                    let s = &**constant;
                    self.build_symbol_table(s.clone(), &None)?;
                }
                Ok(())
            }
            Stmt::PackageDeclaration { name } => {
                let pkg_symbol = format!("pkg.{}", name);
                if self.global_symbol_table.lookup_symbol(&pkg_symbol).is_none() {
                    self.global_symbol_table.new_package(name.clone(), pkg_symbol.clone());
                    self.current_package = pkg_symbol;
                }
                Ok(())
            }
            Stmt::UseDeclaration { name } => {
                let use_symbol = format!("use.{}", name);
                if self.global_symbol_table.lookup_symbol(&use_symbol).is_none() {
                    let current_scope = self
                        .global_symbol_table
                        .get_symbol_table_by_name(self.current_package.as_str());
                    match current_scope {
                        Some(scope) => {
                            scope
                                .borrow_mut()
                                .add_symbol(use_symbol.clone(), Symbol::Use(name.clone()));
                        }
                        None => {
                            Err(format!("error: global scope not found"))?;
                        }
                    }
                }
                Ok(())
            }
            Stmt::Empty => Ok(()),
        }
    }
}

impl fmt::Display for SemanticAnalyzer {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.global_symbol_table)
    }
}
