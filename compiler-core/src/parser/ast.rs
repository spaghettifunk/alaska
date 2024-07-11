use crate::lexer::TokenKind;
use std::{fmt, rc::Rc, sync::Arc};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Literal(Lit),
    Identifier {
        name: String,
        type_: Type,
    },
    Interface {
        name: String,
        type_: Type,
        methods: Vec<Rc<Arc<Stmt>>>,
    },
    FunctionCall {
        name: String,
        args: Vec<Rc<Arc<Stmt>>>,
    },
    ArrayInitialization {
        elements: Vec<Rc<Arc<Stmt>>>,
    },
    ArrayAccess {
        name: String,
        index: Rc<Arc<Stmt>>,
    },
    StructAccess {
        name: String,
        field: Rc<Arc<Stmt>>,
    },
    PrefixOp {
        op: TokenKind,
        expr: Rc<Arc<Stmt>>,
    },
    InfixOp {
        op: TokenKind,
        lhs: Rc<Arc<Stmt>>,
        rhs: Rc<Arc<Stmt>>,
    },
    PostfixOp {
        op: TokenKind,
        expr: Rc<Arc<Stmt>>,
    },
    Comment(String),
    BlockComment(String),
    Defer {
        stmt: Rc<Arc<Stmt>>,
    },
    Let {
        name: String,
        type_: Type,
        statement: Rc<Arc<Stmt>>,
    },
    Assignment {
        name: String,
        type_: Type,
        value: Rc<Arc<Stmt>>,
    },
    IfStmt {
        condition: Rc<Arc<Stmt>>,
        body: Vec<Rc<Arc<Stmt>>>,
        else_stmt: Option<Rc<Arc<Stmt>>>,
    },
    RangeStmt {
        iterator: String,
        range: Rc<Arc<Stmt>>,
        body: Vec<Rc<Arc<Stmt>>>,
    },
    WhileStmt {
        condition: Rc<Arc<Stmt>>,
        body: Vec<Rc<Arc<Stmt>>>,
    },
    MatchStmt {
        value: Rc<Arc<Stmt>>,
        arms: Vec<(Rc<Arc<Stmt>>, Vec<Rc<Arc<Stmt>>>)>,
    },
    Block {
        stmts: Vec<Rc<Arc<Stmt>>>,
    },
    Return {
        value: Vec<Rc<Arc<Stmt>>>,
    },
    StructMember {
        is_public: bool,
        name: String,
        type_: Type,
    },
    Enum {
        is_public: bool,
        name: String,
        type_: Type,
        members: Vec<String>,
    },
    StructDeclaration {
        is_public: bool,
        name: String,
        type_: Type,
        members: Vec<Rc<Arc<Stmt>>>,
    },
    StructInstantiation {
        name: String,
        members: Vec<(String, Rc<Arc<Stmt>>)>,
    },
    InterfaceFunctionSignature {
        name: String,
        generics: Option<Vec<Type>>,
        parameters: Option<Vec<(String, Type)>>,
        return_type: Option<Vec<Type>>,
    },
    FunctionDeclaration {
        is_public: bool,
        name: String,
        generics: Option<Vec<Type>>,
        parameters: Option<Vec<(String, Type)>>,
        body: Vec<Rc<Arc<Stmt>>>,
        return_type: Option<Vec<Type>>,
    },
    ImplDeclaration {
        name: String,
        generics: Option<Vec<Type>>,
        interfaces: Option<Vec<Type>>,
        methods: Vec<Rc<Arc<Stmt>>>,
    },
    Constant {
        name: String,
        type_: Type,
        value: Rc<Arc<Stmt>>,
    },
    ConstantGroup {
        constants: Vec<Rc<Arc<Stmt>>>,
    },
    PackageDeclaration {
        name: String,
    },
    UseDeclaration {
        name: String,
    },
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub name: String,
    pub statements: Vec<Rc<Arc<Stmt>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    name: String,
    literal: Lit,
    generics: Option<Vec<Type>>,
}

impl Type {
    // used to initialize a type with default values
    // this is used in the samentic analysis step where
    // we need to collect all the symbols in the fist pass
    pub fn default() -> Self {
        Type {
            name: String::new(),
            literal: Lit::Unknown,
            generics: None,
        }
    }

    pub fn new(name: String, literal: Lit, generics: Option<Vec<Type>>) -> Self {
        Type {
            name,
            literal,
            generics,
        }
    }

    pub fn compare(&self, other: &Type) -> bool {
        self.name == other.name && self.literal == other.literal && self.generics == other.generics
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
    Struct(String),    // Struct type with the name of the struct
    Interface(String), // Interface type with the name of the interface
    Enum(String),      // Enum type with the name of the enum
    Unknown,
}

impl Lit {
    pub fn to_string(&self) -> String {
        match self {
            Lit::Int(i) => i.to_string(),
            Lit::Float(fl) => fl.to_string(),
            Lit::Str(s) => s.clone(),
            Lit::Bool(b) => b.to_string(),
            Lit::Char(c) => c.to_string(),
            Lit::Nil() => "nil".to_string(),
            Lit::Struct(s) => format!("struct {}", s),
            Lit::Interface(i) => format!("interface {}", i),
            Lit::Enum(e) => format!("enum {}", e),
            Lit::Unknown => "unknown".to_string(),
        }
    }

    pub fn from_string(s: String) -> Lit {
        if let Ok(i) = s.parse::<usize>() {
            return Lit::Int(i);
        }
        if let Ok(fl) = s.parse::<f64>() {
            return Lit::Float(fl);
        }
        if s == "nil" {
            return Lit::Nil();
        }
        if s == "true" {
            return Lit::Bool(true);
        }
        if s == "false" {
            return Lit::Bool(false);
        }
        if s.starts_with('\'') && s.ends_with('\'') {
            return Lit::Char(s.chars().nth(1).unwrap());
        }
        if s.starts_with('"') && s.ends_with('"') {
            return Lit::Str(s[1..s.len() - 1].to_string());
        }
        Lit::Unknown
    }
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
            Lit::Struct(s) => write!(f, "struct {}", s),
            Lit::Interface(i) => write!(f, "interface {}", i),
            Lit::Enum(e) => write!(f, "enum {}", e),
            Lit::Unknown => write!(f, "unknown"),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Literal(lit) => write!(f, "{}", lit),
            Stmt::Identifier { name, type_ } => write!(f, "{}", name),
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
            Stmt::ArrayInitialization { elements } => {
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
            Stmt::Comment(text) => write!(f, "//{}", text),
            Stmt::BlockComment(text) => write!(f, "/*{}*/", text),
            Stmt::Let {
                name,
                type_,
                statement: value,
            } => write!(f, "let {}: {} = {};", name, type_, value),
            Stmt::Assignment { name, type_, value } => write!(f, "{}: {} = {};", name, type_, value),
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
                write!(f, "struct {}: {} {{\n", name, type_)?;
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
                if parameters.is_some() {
                    for (i, (name, type_)) in parameters.clone().unwrap().iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}: {}", name, type_)?;
                    }
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
            Stmt::PackageDeclaration { name: path } => write!(f, "package {};", path),
            Stmt::UseDeclaration { name } => write!(f, "use {};", name),
            Stmt::Defer { stmt } => write!(f, "defer {{\n{}\n}}", stmt),
            Stmt::Empty => write!(f, ""),
            Stmt::Interface { name, type_, methods } => {
                write!(f, "interface {} {{\n", name)?;
                write!(f, "{}\n", type_)?;
                for method in methods {
                    write!(f, "{}\n", method)?;
                }
                write!(f, "}}")
            }
            Stmt::InterfaceFunctionSignature {
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
                if parameters.is_some() {
                    for (i, (name, type_)) in parameters.as_ref().unwrap().iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}: {}", name, type_)?;
                    }
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
                write!(f, "impl {}", name)?;
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
                type_,
                members,
            } => {
                if *is_public {
                    write!(f, "pub ")?;
                }
                write!(f, "enum {}: {} {{\n", name, type_)?;
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
            write!(f, "{}\n", stmt.clone())?;
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
