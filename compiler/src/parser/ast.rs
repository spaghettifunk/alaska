use std::fmt;

use crate::lexer::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Lit),
    Ident(String),
    FnCall {
        fn_name: String,
        args: Vec<Expr>,
    },
    Array {
        elements: Vec<Expr>,
    },
    ArrayAccess {
        array: String,
        index: Box<Expr>,
    },
    StructAccess {
        struct_name: String,
        field: String,
    },
    PrefixOp {
        op: TokenKind,
        expr: Box<Expr>,
    },
    InfixOp {
        op: TokenKind,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    PostfixOp {
        op: TokenKind,
        expr: Box<Expr>,
    },
    Comment {
        text: String,
    },
    BlockComment {
        text: String,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        var_name: String,
        value: Box<Expr>,
    },
    Assignment {
        var_name: String,
        value: Box<Expr>,
    },
    IfStmt {
        condition: Box<Expr>,
        body: Vec<Stmt>,
        else_stmt: Option<Box<Stmt>>,
    },
    Block {
        stmts: Vec<Stmt>,
    },
    FnDef {
        name: String,
        parameters: Vec<(String, Type)>,
        body: Vec<Stmt>,
        return_type: Option<Box<Type>>,
    },
    Return {
        value: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Struct {
        name: Type,
        members: Vec<(String, Type)>,
    },
    Function {
        name: String,
        parameters: Vec<(String, Type)>,
        body: Vec<Stmt>,
        return_type: Option<Box<Type>>,
        return_stmt: Option<Box<Expr>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub name: String,
    pub generics: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(usize),
    Float(f64),
    Str(String),
    Bool(bool),
    Char(char),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, generic) in self.generics.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", generic)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Ident(name) => write!(f, "{}", name),
            Expr::FnCall { fn_name, args } => {
                write!(f, "{}(", fn_name)?;
                for arg in args {
                    write!(f, "{},", arg)?;
                }
                write!(f, ")")
            }
            Expr::StructAccess { struct_name, field } => write!(f, "{}.{}", struct_name, field),
            Expr::Array { elements } => {
                write!(f, "[")?;
                for element in elements {
                    write!(f, "{},", element)?;
                }
                write!(f, "]")
            }
            Expr::ArrayAccess { array, index } => write!(f, "{}[{}]", array, index),
            Expr::PrefixOp { op, expr } => write!(f, "({} {})", op, expr),
            Expr::InfixOp { op, lhs, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::PostfixOp { op, expr } => write!(f, "({} {})", expr, op),
            Expr::Comment { text } => write!(f, "//{}", text),
            Expr::BlockComment { text } => write!(f, "/*{}*/", text),
        }
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Float(fl) => write!(f, "{}", fl),
            Lit::Str(s) => write!(f, r#""{}""#, s),
            Lit::Bool(b) => write!(f, "{}", b),
            Lit::Char(c) => write!(f, "'{}'", c),
        }
    }
}
