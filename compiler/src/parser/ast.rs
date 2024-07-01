use super::Result;
use crate::lexer::TokenKind;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Literal(Lit),
    Identifier(String),
    Interface {
        name: String,
        type_: Option<Box<Type>>,
        methods: Vec<Box<Stmt>>,
    },
    FunctionCall {
        name: String,
        args: Vec<Stmt>,
    },
    Array {
        elements: Vec<Stmt>,
    },
    ArrayAccess {
        name: String,
        index: Box<Stmt>,
    },
    StructAccess {
        name: String,
        field: Box<Stmt>,
    },
    PrefixOp {
        op: TokenKind,
        expr: Box<Stmt>,
    },
    InfixOp {
        op: TokenKind,
        lhs: Box<Stmt>,
        rhs: Box<Stmt>,
    },
    PostfixOp {
        op: TokenKind,
        expr: Box<Stmt>,
    },
    Comment {
        text: String,
    },
    BlockComment {
        text: String,
    },
    Defer {
        stmt: Box<Stmt>,
    },
    Let {
        identifier: String,
        statement: Box<Stmt>,
    },
    Assignment {
        name: String,
        value: Box<Stmt>,
    },
    IfStmt {
        condition: Box<Stmt>,
        body: Vec<Stmt>,
        else_stmt: Option<Box<Stmt>>,
    },
    RangeStmt {
        iterator: String,
        range: Box<Stmt>,
        body: Vec<Stmt>,
    },
    WhileStmt {
        condition: Box<Stmt>,
        body: Vec<Stmt>,
    },
    MatchStmt {
        value: Box<Stmt>,
        arms: Vec<(Stmt, Vec<Stmt>)>,
    },
    Block {
        stmts: Vec<Stmt>,
    },
    Return {
        value: Vec<Box<Stmt>>,
    },
    StructMember {
        is_public: bool,
        name: String,
        type_: Type,
    },
    Enum {
        is_public: bool,
        name: String,
        members: Vec<String>,
    },
    StructDeclaration {
        is_public: bool,
        name: String,
        type_: Type,
        members: Vec<Stmt>,
    },
    StructInstantiation {
        name: String,
        members: Vec<(String, Box<Stmt>)>,
    },
    FunctionSignature {
        name: String,
        generics: Option<Vec<Box<Type>>>,
        parameters: Vec<(String, Type)>,
        return_type: Option<Vec<Box<Type>>>,
    },
    FunctionDeclaration {
        is_public: bool,
        name: String,
        generics: Option<Vec<Box<Type>>>,
        parameters: Vec<(String, Type)>,
        body: Vec<Stmt>,
        return_type: Option<Vec<Box<Type>>>,
    },
    ImplDeclaration {
        name: String,
        generics: Option<Vec<Box<Type>>>,
        interfaces: Option<Vec<Box<Type>>>,
        methods: Vec<Box<Stmt>>,
    },
    Constant {
        name: String,
        type_: Type,
        value: Box<Stmt>,
    },
    ConstantGroup {
        constants: Vec<Box<Stmt>>,
    },
    Package {
        name: String,
    },
    Use {
        name: String,
    },
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub name: String,
    pub statements: Vec<Result<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub name: String,
    pub is_array: bool,
    pub generics: Option<Vec<Box<Type>>>,
}

impl Type {
    // used to initialize a type with default values
    // this is used in the samentic analysis step where
    // we need to collect all the symbols in the fist pass
    pub fn default() -> Self {
        Type {
            name: String::new(),
            is_array: false,
            generics: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(usize),
    Float(f64),
    Str(String),
    Bool(bool),
    Char(char),
    Nil(),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AST {
    pub files: Vec<SourceFile>,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(generics) = &self.generics {
            write!(f, "<")?;
            for (i, generic) in generics.iter().enumerate() {
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

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Float(fl) => write!(f, "{}", fl),
            Lit::Str(s) => write!(f, r#""{}""#, s),
            Lit::Bool(b) => write!(f, "{}", b),
            Lit::Char(c) => write!(f, "'{}'", c),
            Lit::Nil() => write!(f, "nil"),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Literal(lit) => write!(f, "{}", lit),
            Stmt::Identifier(name) => write!(f, "{}", name),
            Stmt::FunctionCall { name, args } => {
                write!(f, "{}(", name)?;
                for arg in args {
                    write!(f, "{},", arg)?;
                }
                write!(f, ")")
            }
            Stmt::StructAccess {
                name: struct_name,
                field,
            } => write!(f, "{}.{}", struct_name, field),
            Stmt::Array { elements } => {
                write!(f, "[")?;
                for element in elements {
                    write!(f, "{},", element)?;
                }
                write!(f, "]")
            }
            Stmt::ArrayAccess { name: array, index } => write!(f, "{}[{}]", array, index),
            Stmt::PrefixOp { op, expr } => write!(f, "({} {})", op, expr),
            Stmt::InfixOp { op, lhs, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Stmt::PostfixOp { op, expr } => write!(f, "({} {})", expr, op),
            Stmt::Comment { text } => write!(f, "//{}", text),
            Stmt::BlockComment { text } => write!(f, "/*{}*/", text),
            Stmt::Let {
                identifier: var_name,
                statement: value,
            } => write!(f, "let {} = {};", var_name, value),
            Stmt::Assignment { name: var_name, value } => write!(f, "{} = {};", var_name, value),
            Stmt::IfStmt {
                condition,
                body,
                else_stmt,
            } => {
                write!(f, "if {} {{\n", condition)?;
                for stmt in body {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")?;
                if let Some(else_stmt) = else_stmt {
                    write!(f, " else {{\n{}\n}}", else_stmt)?;
                }
                Ok(())
            }
            Stmt::RangeStmt { iterator, range, body } => {
                write!(f, "for {} in {} {{\n", iterator, range)?;
                for stmt in body {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")
            }
            Stmt::FunctionCall { args, name } => {
                write!(f, "{}(", name)?;
                for arg in args {
                    write!(f, "{},", arg)?;
                }
                write!(f, ")")
            }
            Stmt::WhileStmt { condition, body } => {
                write!(f, "while {} {{\n", condition)?;
                for stmt in body {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")
            }
            Stmt::MatchStmt { value, arms } => {
                write!(f, "match {} {{\n", value)?;
                for (pattern, stmts) in arms {
                    write!(f, "{} => {{\n", pattern)?;
                    for stmt in stmts {
                        write!(f, "{}\n", stmt)?;
                    }
                    write!(f, "}}\n")?;
                }
                write!(f, "}}")
            }
            Stmt::Block { stmts } => {
                write!(f, "{{\n")?;
                for stmt in stmts {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")
            }
            Stmt::Return { value } => {
                write!(f, "return ")?;
                for val in value {
                    write!(f, "{},", val)?;
                }
                Ok(())
            }
            Stmt::StructDeclaration {
                is_public,
                name,
                type_,
                members,
            } => {
                if *is_public {
                    write!(f, "pub ")?;
                }
                write!(f, "struct {}<{}> {{\n", name, type_)?;
                for m in members {
                    write!(f, "{}\n", m)?;
                }
                write!(f, "}}")
            }
            Stmt::FunctionDeclaration {
                is_public,
                name,
                generics,
                parameters,
                body,
                return_type,
            } => {
                if *is_public {
                    write!(f, "pub ")?;
                }
                write!(f, "fn {}(", name)?;
                if let Some(generics) = generics {
                    write!(f, "<")?;
                    for (i, generic) in generics.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", generic)?;
                    }
                    write!(f, ">")?;
                }
                for (i, (name, type_)) in parameters.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, type_)?;
                }
                write!(f, ")")?;
                if let Some(return_type) = return_type {
                    write!(f, " -> {:?}", return_type)?;
                }
                write!(f, " {{\n")?;
                for stmt in body {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")
            }
            Stmt::Package { name: path } => write!(f, "package {};", path),
            Stmt::Use { name } => write!(f, "use {};", name),
            Stmt::Defer { stmt } => write!(f, "defer {{\n{}\n}}", stmt),
            Stmt::Empty => write!(f, ""),
            Stmt::Interface { name, type_, methods } => {
                write!(f, "interface {} {{\n", name)?;
                if let Some(type_) = type_ {
                    write!(f, "{}\n", type_)?;
                }
                for method in methods {
                    write!(f, "{}\n", method)?;
                }
                write!(f, "}}")
            }
            Stmt::FunctionSignature {
                name,
                generics,
                parameters,
                return_type,
            } => {
                write!(f, "fn {}(", name)?;
                if let Some(generics) = generics {
                    write!(f, "<")?;
                    for (i, generic) in generics.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", generic)?;
                    }
                    write!(f, ">")?;
                }
                for (i, (name, type_)) in parameters.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, type_)?;
                }
                write!(f, ")")?;
                if let Some(return_type) = return_type {
                    write!(f, " -> {:?}", return_type)?;
                }
                Ok(())
            }
            Stmt::StructMember { is_public, name, type_ } => {
                if *is_public {
                    write!(f, "pub ")?;
                }
                write!(f, "{}: {}", name, type_)
            }
            Stmt::ImplDeclaration {
                name,
                generics,
                interfaces,
                methods,
            } => {
                write!(f, "impl")?;
                if let Some(generics) = generics {
                    write!(f, "<")?;
                    for (i, generic) in generics.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", generic)?;
                    }
                    write!(f, ">")?;
                }
                if let Some(interface) = interfaces {
                    write!(f, " for {:?}", interface)?;
                }
                write!(f, " {{\n")?;
                for method in methods {
                    write!(f, "{}\n", method)?;
                }
                write!(f, "}}")
            }
            Stmt::ConstantGroup { constants } => {
                for constant in constants {
                    write!(f, "{}\n", constant)?;
                }
                Ok(())
            }
            Stmt::Constant { name, type_, value } => write!(f, "const {}:{} {};", name, type_, value),
            Stmt::StructInstantiation { name, members } => {
                write!(f, "{}", name)?;
                write!(f, "{{")?;
                for (name, value) in members {
                    write!(f, "{}: {},", name, value)?;
                }
                write!(f, "}}")
            }
            Stmt::Enum {
                is_public,
                name,
                members,
            } => {
                if *is_public {
                    write!(f, "pub ")?;
                }
                write!(f, "enum {} {{\n", name)?;
                for member in members {
                    write!(f, "{}\n", member)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl fmt::Display for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "File: {}\n", self.name)?;
        for stmt in &self.statements {
            write!(f, "{}\n", stmt.clone().unwrap())?;
        }
        Ok(())
    }
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for file in &self.files {
            write!(f, "{}\n", file)?;
        }
        Ok(())
    }
}
