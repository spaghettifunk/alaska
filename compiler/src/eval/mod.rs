mod sym;

use std::borrow::{Borrow, BorrowMut};

use sym::{GlobalSymbolTable, NestedSymbolTable, Symbol, SymbolKind};

use crate::parser::ast::{Stmt, AST};

pub struct SemanticAnalyzer {
    global_symbol_table: GlobalSymbolTable,
}

impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer {
            global_symbol_table: GlobalSymbolTable::new(),
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
            Stmt::Package { name } => {
                // the word `package `name` can be used in several files
                // it's important that we use the correct `PackageSymbolTable`
                // object for the rest of the source file
                if !self.global_symbol_table.get_package(&name).is_none() {
                    self.global_symbol_table.set_current_package_name(name.clone());
                    return Ok(());
                }

                // create a new `PackageSymbolTable` object
                self.global_symbol_table.add_package_symbol_table(&name);
                self.global_symbol_table.set_current_package_name(name.clone());

                Ok(())
            }
            Stmt::Literal(_) => {
                // nothing to do here
                Ok(())
            }
            Stmt::Identifier(name) => {
                let pkg_table = self.global_symbol_table.get_current_package();
                pkg_table.insert(Symbol::new(name.clone(), SymbolKind::Identifier, Box::new(()), false));

                Ok(())
            }
            Stmt::Interface { name, type_, methods } => {
                // TODO: this is ugly! the borrow checker is not happy with me
                {
                    let pkg_table = self.global_symbol_table.get_current_package();

                    pkg_table.insert(Symbol::new(name.clone(), SymbolKind::Interface, Box::new(()), true));

                    // create new scope for the function signatures
                    pkg_table.enter_scope(name.clone());

                    let nested_symbol_table = pkg_table.get_current_nested_scope();

                    for method in methods {
                        self.build_symbol_table(*method)?;
                    }
                }

                // exit scope
                let pkg_table = self.global_symbol_table.get_current_package();
                pkg_table.exit_scope();

                Ok(())
            }
            Stmt::FunctionCall { name, args } => {
                // validate that the function exists
                let pkg_table = self.global_symbol_table.get_current_package();
                let nested_symbol_table = pkg_table.get_current_nested_scope();

                if nested_symbol_table.get_identifier(&name).is_none() {
                    return Err(format!("function `{}` not found", name));
                }

                for arg in args {
                    self.build_symbol_table(arg)?;
                }

                Ok(())
            }
            Stmt::Array { elements } => todo!(),
            Stmt::ArrayAccess { name, index } => todo!(),
            Stmt::StructAccess { name, field } => todo!(),
            Stmt::PrefixOp { op, expr } => todo!(),
            Stmt::InfixOp { op, lhs, rhs } => todo!(),
            Stmt::PostfixOp { op, expr } => todo!(),
            Stmt::Assignment { name, value } => todo!(),
            Stmt::IfStmt {
                condition,
                body,
                else_stmt,
            } => todo!(),
            Stmt::RangeStmt { iterator, range, body } => todo!(),
            Stmt::WhileStmt { condition, body } => todo!(),
            Stmt::MatchStmt { value, arms } => todo!(),
            Stmt::Defer { stmt } => {
                self.build_symbol_table(*stmt)?;
                Ok(())
            }
            Stmt::Let { identifier, statement } => {
                let pkg_table = self.global_symbol_table.get_current_package();
                let nested_symbol_table = pkg_table.get_current_nested_scope();

                if nested_symbol_table.get_identifier(&identifier).is_some() {
                    return Err(format!("variable `{}` already declared", identifier));
                }

                nested_symbol_table.insert(Symbol::new(
                    identifier.clone(),
                    SymbolKind::Identifier,
                    Box::new(()),
                    false,
                ));

                return self.build_symbol_table(*statement);
            }
            Stmt::Block { stmts } => {
                for stmt in stmts {
                    self.build_symbol_table(stmt)?;
                }
                Ok(())
            }
            Stmt::Return { value } => {
                for ret in value {
                    self.build_symbol_table(*ret.clone())?;
                }
                Ok(())
            }
            Stmt::StructMember { is_public, name, type_ } => {
                let pkg_table = self.global_symbol_table.get_current_package();
                let nested_symbol_table = pkg_table.get_current_nested_scope();
                nested_symbol_table.insert(Symbol::new(
                    name.clone(),
                    SymbolKind::StructMember,
                    Box::new(()),
                    is_public,
                ));

                Ok(())
            }
            Stmt::Enum {
                is_public,
                name,
                members,
            } => {
                let pkg_table = self.global_symbol_table.get_current_package();
                pkg_table.insert(Symbol::new(name.clone(), SymbolKind::Enum, Box::new(()), is_public));

                // create new scope for the enum members
                pkg_table.enter_scope(name.clone());

                let nested_symbol_table = pkg_table.get_current_nested_scope();

                for member in members {
                    nested_symbol_table.insert(Symbol::new(
                        member.clone(),
                        SymbolKind::EnumMember,
                        Box::new(()),
                        is_public, // we use the enum visibility for the enum members
                    ));
                }

                // exit scope
                pkg_table.exit_scope();

                Ok(())
            }
            Stmt::StructDeclaration {
                is_public,
                name,
                type_,
                members,
            } => {
                // TODO: this is ugly! the borrow checker is not happy with me
                {
                    let pkg_table = self.global_symbol_table.get_current_package();
                    pkg_table.insert(Symbol::new(name.clone(), SymbolKind::Struct, Box::new(()), is_public));

                    // create new scope for the struct members
                    pkg_table.enter_scope(name.clone());

                    for member in members {
                        self.build_symbol_table(member)?;
                    }
                }

                // exit scope
                let pkg_table = self.global_symbol_table.get_current_package();
                pkg_table.exit_scope();

                Ok(())
            }
            Stmt::StructInstantiation { name, members } => todo!(),
            Stmt::InterfaceFunctionSignature {
                name,
                generics,
                parameters,
                return_type,
            } => {
                let pkg_table = self.global_symbol_table.get_current_package();
                let nested_symbol_table = pkg_table.get_current_nested_scope();
                nested_symbol_table.insert(Symbol::new(
                    name.clone(),
                    SymbolKind::InterfaceFunction,
                    Box::new(()),
                    true,
                ));

                for (name, tp) in parameters {
                    nested_symbol_table.insert(Symbol::new(name.clone(), SymbolKind::Parameter, Box::new(()), false));
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
            } => {
                // TODO: this is ugly! the borrow checker is not happy with me
                {
                    let pkg_table = &mut self.global_symbol_table.get_current_package();

                    pkg_table.insert(Symbol::new(name.clone(), SymbolKind::Function, Box::new(()), is_public));
                    // create new scope for the function parameters
                    pkg_table.enter_scope(name.clone());

                    let nested_symbol_table = &mut pkg_table.get_current_nested_scope();

                    for (name, tp) in parameters {
                        nested_symbol_table.insert(Symbol::new(
                            name.clone(),
                            SymbolKind::Parameter,
                            Box::new(()),
                            false,
                        ));
                    }

                    for stmt in body {
                        self.build_symbol_table(stmt)?;
                    }
                }

                // exit scope
                let pkg_table = &mut self.global_symbol_table.get_current_package();
                pkg_table.exit_scope();

                Ok(())
            }
            Stmt::ImplDeclaration {
                name,
                generics,
                interfaces,
                methods,
            } => {
                {
                    let pkg_table = self.global_symbol_table.get_current_package();
                    pkg_table.insert(Symbol::new(name.clone(), SymbolKind::Impl, Box::new(()), true));

                    // create new scope for the function signatures
                    pkg_table.enter_scope(name.clone());

                    for method in methods {
                        self.build_symbol_table(*method)?;
                    }
                }

                // exit scope
                let pkg_table = self.global_symbol_table.get_current_package();
                pkg_table.exit_scope();

                Ok(())
            }
            Stmt::Constant { name, type_, value } => {
                let pkg_table = self.global_symbol_table.get_current_package();
                // TODO: is the value correctly assigned?
                pkg_table.insert(Symbol::new(name.clone(), SymbolKind::Constant, Box::new(value), false));

                Ok(())
            }
            Stmt::ConstantGroup { constants } => {
                for constant in constants {
                    self.build_symbol_table(*constant)?;
                }
                Ok(())
            }
            // TODO: I don't have a `module` system yet. For now, it continues
            Stmt::Use { name } => Ok(()),
            // ignore the comments -- maybe one day we could use comments for special things like go does
            // for now, we skip it
            Stmt::Comment { text } => Ok(()),
            Stmt::BlockComment { text } => Ok(()),
            Stmt::Empty => Ok(()),
        }
    }
}
