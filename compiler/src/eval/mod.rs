mod sym;

use std::{borrow::Borrow, collections::HashMap, hash::Hash};

use sym::{PackageSymbolTable, Symbol, SymbolKind};

use crate::parser::ast::{Stmt, Type, AST};

pub struct SemanticAnalyzer {
    global_symbol_table: HashMap<String, PackageSymbolTable>,
    current_package_name: String,
}

impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer {
            global_symbol_table: HashMap::new(),
            current_package_name: String::new(),
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
                if self.global_symbol_table.contains_key(&name) {
                    self.current_package_name = name;
                    return Ok(());
                }

                // create a new `PackageSymbolTable` object
                self.global_symbol_table
                    .insert(name.clone(), PackageSymbolTable::new(name.clone()));
                self.current_package_name = name;

                Ok(())
            }
            Stmt::Literal(_) => todo!(),
            Stmt::Identifier(name) => {
                self.global_symbol_table
                    .get_mut(self.current_package_name.as_str())
                    .unwrap()
                    .insert(Symbol::new(name, SymbolKind::Identifier, Box::new(()), false));
                Ok(())
            }

            Stmt::Interface { name, type_, methods } => todo!(),
            Stmt::FunctionCall { name, args } => todo!(),
            Stmt::Array { elements } => todo!(),
            Stmt::ArrayAccess { name, index } => todo!(),
            Stmt::StructAccess { name, field } => todo!(),
            Stmt::PrefixOp { op, expr } => todo!(),
            Stmt::InfixOp { op, lhs, rhs } => todo!(),
            Stmt::PostfixOp { op, expr } => todo!(),
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
            } => todo!(),
            Stmt::StructDeclaration {
                is_public,
                name,
                type_,
                members,
            } => todo!(),
            Stmt::StructInstantiation { name, members } => todo!(),
            Stmt::FunctionSignature {
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
            Stmt::Constant { name, type_, value } => todo!(),
            Stmt::ConstantGroup { constants } => todo!(),
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
