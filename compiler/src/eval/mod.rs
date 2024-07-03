mod sym;

use std::{cell::RefCell, fmt::format, rc::Rc};

use sym::{GlobalSymbolTable, Symbol, SymbolTable};

use crate::parser::ast::{Stmt, AST};

pub struct SemanticAnalyzer {
    global_scope: GlobalSymbolTable,
    current_package: String,
}

impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer {
            global_scope: GlobalSymbolTable::new(),
            current_package: String::new(),
        }
    }

    pub fn symbols_collection(&mut self, ast: AST) -> Result<(), String> {
        for sourcefile in ast.files {
            println!("symbols collection for file `{:?}`....", sourcefile.name);
            for stmt in sourcefile.statements {
                match stmt {
                    Ok(stmt) => {
                        self.build_symbol_table(stmt)?;
                    }
                    Err(_) => continue,
                }
            }
        }
        Ok(())
    }

    fn build_symbol_table(&mut self, stmt: Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Literal(_) => Ok(()),
            Stmt::Identifier(name) => todo!(),
            Stmt::Interface { name, type_, methods } => todo!(),
            Stmt::FunctionCall { name, args } => todo!(),
            Stmt::Array { elements } => todo!(),
            Stmt::ArrayAccess { name, index } => todo!(),
            Stmt::StructAccess { name, field } => todo!(),
            Stmt::PrefixOp { op, expr } => todo!(),
            Stmt::InfixOp { op, lhs, rhs } => todo!(),
            Stmt::PostfixOp { op, expr } => todo!(),
            Stmt::Comment { text } => Ok(()),
            Stmt::BlockComment { text } => Ok(()),
            Stmt::Defer { stmt } => todo!(),
            Stmt::Let { identifier, statement } => todo!(),
            Stmt::Assignment { name, value } => todo!(),
            Stmt::IfStmt {
                condition,
                body,
                else_stmt,
            } => todo!(),
            Stmt::RangeStmt { iterator, range, body } => todo!(),
            Stmt::WhileStmt { condition, body } => todo!(),
            Stmt::MatchStmt { value, arms } => todo!(),
            Stmt::Block { stmts } => todo!(),
            Stmt::Return { value } => todo!(),
            Stmt::StructMember { is_public, name, type_ } => todo!(),
            Stmt::Enum {
                is_public,
                name,
                members,
            } => {
                let enum_symbol = format!("enum.{}", name);
                if self.global_scope.lookup_symbol(&enum_symbol).is_none() {
                    let current_scope = self.global_scope.current_scope();
                    match current_scope {
                        Some(scope) => {
                            scope.borrow_mut().add_symbol(
                                enum_symbol.clone(),
                                Symbol::Enum {
                                    is_public,
                                    name: name.clone(),
                                    table: Rc::new(RefCell::new(SymbolTable::new(
                                        enum_symbol.clone(),
                                        Some(scope.clone()),
                                    ))),
                                },
                            );
                        }
                        None => {
                            Err(format!("error: global scope not found"))?;
                        }
                    }
                } else {
                    Err(format!("error: enum already declared"))?;
                }

                let enum_scope = self.global_scope.get_symbol_table_by_name(&enum_symbol);
                match enum_scope {
                    Some(scope) => {
                        for member in members {
                            if scope.borrow().lookup(&member).is_some() {
                                Err(format!("error: enum member already declared"))?;
                            }

                            scope.borrow_mut().add_symbol(
                                member.clone(),
                                Symbol::EnumMember {
                                    name: member.clone(),
                                    value: String::new(), // TODO: how do I save the value???
                                },
                            );
                        }
                    }
                    None => {
                        Err(format!("error: enum scope not found"))?;
                    }
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
                if self.global_scope.lookup_symbol(&struct_symbol).is_none() {
                    let current_scope = self.global_scope.current_scope();
                    match current_scope {
                        Some(scope) => {
                            scope.borrow_mut().add_symbol(
                                struct_symbol.clone(),
                                Symbol::Struct {
                                    is_public,
                                    name: name.clone(),
                                    table: Rc::new(RefCell::new(SymbolTable::new(
                                        struct_symbol.clone(),
                                        Some(scope.clone()),
                                    ))),
                                },
                            );
                        }
                        None => {
                            Err(format!("error: global scope not found"))?;
                        }
                    }
                } else {
                    Err(format!("error: struct already declared"))?;
                }

                let struct_scope = self.global_scope.get_symbol_table_by_name(&struct_symbol);
                match struct_scope {
                    Some(scope) => {
                        for member in members {
                            match member {
                                Stmt::StructMember { is_public, name, type_ } => {
                                    scope.borrow_mut().add_symbol(
                                        name.clone(),
                                        Symbol::StructMember {
                                            is_public,
                                            name,
                                            type_: String::new(), // TODO: how do I save the type???
                                        },
                                    );
                                }
                                _ => continue,
                            }
                        }
                    }
                    None => {
                        Err(format!("error: struct scope not found"))?;
                    }
                }
                Ok(())
            }
            Stmt::StructInstantiation { name, members } => todo!(),
            Stmt::InterfaceFunctionSignature {
                name,
                generics,
                parameters,
                return_type,
            } => todo!(),
            Stmt::FunctionDeclaration {
                is_public,
                name,
                generics,
                parameters,
                body,
                return_type,
            } => todo!(),
            Stmt::ImplDeclaration {
                name,
                generics,
                interfaces,
                methods,
            } => todo!(),
            Stmt::Constant { name, type_, value } => {
                let constant_symbol = format!("const.{}", name);
                if self.global_scope.lookup_symbol(&constant_symbol).is_none() {
                    let current_scope = self.global_scope.current_scope();
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
                }
                Ok(())
            }
            Stmt::ConstantGroup { constants } => {
                for constant in constants {
                    self.build_symbol_table(*constant)?;
                }
                Ok(())
            }
            Stmt::Package { name } => {
                let pkg_symbol = format!("pkg.{}", name);
                if self.global_scope.lookup_symbol(&pkg_symbol).is_none() {
                    let current_scope = self.global_scope.current_scope();
                    match current_scope {
                        Some(scope) => {
                            scope
                                .borrow_mut()
                                .add_symbol(pkg_symbol.clone(), Symbol::Package(name.clone()));
                            self.current_package = name;
                        }
                        None => {
                            Err(format!("error: global scope not found"))?;
                        }
                    }
                }
                Ok(())
            }
            Stmt::Use { name } => {
                let use_symbol = format!("use.{}", name);
                if self.global_scope.lookup_symbol(&use_symbol).is_none() {
                    let current_scope = self.global_scope.current_scope();
                    match current_scope {
                        Some(scope) => {
                            scope
                                .borrow_mut()
                                .add_symbol(use_symbol.clone(), Symbol::Use(name.clone()));
                            self.current_package = name;
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
