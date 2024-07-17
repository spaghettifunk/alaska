use std::collections::HashMap;

use super::sym::{Context, SymbolInfo};
use crate::{
    parser::ast::{CustomType, Expr, Stmt, Type, AST},
    T,
};

pub struct TypeChecker {
    // String -> name of the symbol, CustomType -> the type of the symbol
    custom_types: HashMap<String, CustomType>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            custom_types: HashMap::new(),
        }
    }

    // type checking pass
    pub fn type_check_pass(&mut self, ast: AST, context: &Context) -> Result<(), Vec<String>> {
        let mut errors: Vec<String> = Vec::new();
        for sourcefile in ast.files {
            for stmt in sourcefile.statements {
                let result = self.type_check_stmt(context, *stmt.clone());
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

    fn type_check_stmt(&mut self, context: &Context, stmt: Stmt) -> Result<Type, String> {
        match stmt {
            Stmt::PackageDeclaration(_) => todo!(),
            Stmt::UseDeclaration(_) => todo!(),
            Stmt::Expression(_) => todo!(),
            Stmt::ConstantGroup { constants } => todo!(),
            Stmt::Constant {
                is_public,
                name,
                type_,
                value,
            } => todo!(),
            Stmt::Let { name, type_, expr } => todo!(),
            Stmt::Interface { name, type_, methods } => todo!(),
            Stmt::Defer { expr } => todo!(),
            Stmt::IfStmt {
                condition,
                body,
                else_stmt,
            } => todo!(),
            Stmt::RangeStmt { iterator, range, body } => todo!(),
            Stmt::WhileStmt { condition, body } => todo!(),
            Stmt::Block { stmts } => todo!(),
            Stmt::Return { exprs } => todo!(),
            Stmt::Enum {
                is_public,
                name,
                type_,
                members,
            } => todo!(),
            Stmt::StructDeclaration {
                is_public,
                name,
                type_,
                members,
            } => todo!(),
            Stmt::StructMember { is_public, name, type_ } => todo!(),
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
            _ => Err("Unimplemented expression type".to_string()),
        }
    }

    fn type_check_expr(&mut self, context: &Context, expr: Expr) -> Result<Type, String> {
        match expr {
            Expr::IntegerLiteral(_) => Ok(Type::Int),
            Expr::FloatLiteral(_) => Ok(Type::Float),
            Expr::StringLiteral(_) => Ok(Type::String),
            Expr::BoolLiteral(_) => Ok(Type::Bool),
            Expr::CharLiteral(_) => Ok(Type::Char),
            Expr::Nil => Ok(Type::Optional(Box::new(Type::Nil))),
            Expr::Variable(name) => match context.lookup(name.clone().as_str()) {
                Some(var) => {
                    let opt_name = var.name().expect("Variable name not found");
                    let typ = var.type_().expect("Variable type not found");
                    if *typ == Type::Unknown {
                        return Err(format!("Variable {} has no type", opt_name));
                    }
                    Ok(typ.clone())
                }
                None => {
                    return Err(format!("Variable {} not found", name));
                }
            },
            Expr::PrefixOp { op, expr } => {
                let typ = self.type_check_expr(context, *expr.clone())?;
                match op {
                    T![!] => {
                        if typ == Type::Bool {
                            Ok(Type::Bool)
                        } else {
                            Err(format!("Expected bool, got {}", typ))
                        }
                    }
                    T![-] => {
                        if typ == Type::Int || typ == Type::Float {
                            Ok(typ)
                        } else {
                            Err(format!("Expected int or float, got {}", typ))
                        }
                    }
                    _ => Err(format!("Unknown prefix operator {}", op)),
                }
            }
            Expr::BinaryOp { op, lhs, rhs } => {
                let lhs_typ = self.type_check_expr(context, *lhs.clone())?;
                let rhs_typ = self.type_check_expr(context, *rhs.clone())?;
                match op {
                    T![+] | T![-] | T![*] | T![/] | T![%] => {
                        if lhs_typ == Type::Int && rhs_typ == Type::Int {
                            Ok(Type::Int)
                        } else if lhs_typ == Type::Float && rhs_typ == Type::Float {
                            Ok(Type::Float)
                        } else {
                            Err(format!("Expected int or float, got {} and {}", lhs_typ, rhs_typ))
                        }
                    }
                    T![==] | T![!=] | T![<] | T![>] | T![<=] | T![>=] => {
                        if lhs_typ == rhs_typ {
                            Ok(Type::Bool)
                        } else {
                            Err(format!("Expected same types, got {} and {}", lhs_typ, rhs_typ))
                        }
                    }
                    T![&&] | T![||] => {
                        if lhs_typ == Type::Bool && rhs_typ == Type::Bool {
                            Ok(Type::Bool)
                        } else {
                            Err(format!("Expected bool, got {} and {}", lhs_typ, rhs_typ))
                        }
                    }
                    _ => Err(format!("Unknown binary operator {}", op)),
                }
            }
            Expr::PostfixOp { op, expr } => {
                let typ = self.type_check_expr(context, *expr.clone())?;
                match op {
                    T![++] | T![--] => {
                        if typ == Type::Int {
                            Ok(Type::Int)
                        } else {
                            Err(format!("Expected int, got {}", typ))
                        }
                    }
                    _ => Err(format!("Unknown postfix operator {}", op)),
                }
            }
            Expr::Assignment { name, type_, value } => todo!(),
            Expr::FunctionCall { name, args } => {
                let mut arg_types: Vec<Type> = Vec::new();
                for arg in args {
                    let typ = self.type_check_expr(context, *arg.clone())?;
                    arg_types.push(typ);
                }
                match context.lookup(name.clone().as_str()) {
                    Some(func) => {
                        let opt_name = func.name().expect("Function name not found");
                        if let Some(params) = func.parameters() {
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

                        if let Some(ret_type) = func.return_type() {
                            if ret_type.len() > 1 {
                                // TODO: support multiple return types
                                return Err("Multiple return types not yet supported".to_string());
                            }
                            Ok(ret_type[0].clone())
                        } else {
                            Ok(Type::Unknown)
                        }
                    }
                    None => {
                        return Err(format!("Function {} not found", name));
                    }
                }
            }
            Expr::ArrayInitialization { elements } => {
                let mut elem_types: Vec<Type> = Vec::new();
                for elem in elements {
                    let typ = self.type_check_expr(context, *elem.clone())?;
                    elem_types.push(typ);
                }
                if elem_types.len() == 0 {
                    return Ok(Type::Array(Box::new(Type::Unknown)));
                }
                // the first element determines the type of the array
                let typ = elem_types[0].clone();
                for elem in elem_types {
                    if elem != typ {
                        return Err(format!("Expected array of type {}, got {}", typ, elem));
                    }
                }
                Ok(Type::Array(Box::new(typ)))
            }
            Expr::ArrayAccess { name, index } => {
                let index_typ = self.type_check_expr(context, *index.clone())?;
                if index_typ != Type::Int {
                    return Err(format!("Expected type int, got {}", index_typ));
                }

                match context.lookup(name.clone().as_str()) {
                    Some(var) => {
                        let typ = var.type_().expect("Variable type not found");
                        if let Type::Array(inner) = typ {
                            Ok(*inner.clone())
                        } else {
                            return Err(format!("Expected array, got {}", typ));
                        }
                    }
                    None => {
                        return Err(format!("Variable {} not found", name));
                    }
                }
            }
            Expr::StructAccess { name, field } => match context.lookup(name.clone().as_str()) {
                // TODO: issue here is that we could access a struct that is not yet defined
                Some(struct_symbol) => {
                    match self.type_check_expr(context, *field.clone()) {
                        Ok(typ) => {
                            let struct_type = struct_symbol.type_().expect("Struct type not found");

                            // if let Type::Struct { members } = struct_type {
                            //     for member in members {
                            //         if member.name() == field {
                            //             return Ok(member.type_().expect("Member type not found").clone());
                            //         }
                            //     }
                            //     return Err(format!("Member {} not found in struct {}", field, name));
                            // } else {
                            //     return Err(format!("Expected struct, got {}", struct_type));
                            // }
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }

                    Ok(Type::Nil)
                }
                None => {
                    return Err(format!("Variable {} not found", name));
                }
            },
            Expr::StructInstantiation { name, members } => {
                // TODO: issue here is that we could instantiate a struct that is not yet defined

                println!("StructInstantiation");
                Ok(Type::Nil)
            }
            _ => Err("Unimplemented expression type".to_string()),
        }
    }
}
