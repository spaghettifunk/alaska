use crate::lexer::TokenKind;
use std::{collections::HashMap, fmt};

pub type Identifier = String;
pub type TypeBox = Box<Type>;
pub type ExprBox = Box<Expr>;
pub type StmtBox = Box<Stmt>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Char,
    Bool,
    Enum,
    String,
    Array(TypeBox),
    Custom { name: String, generics: Option<Vec<Type>> },
    Optional(TypeBox), // Represents an optional type
    Unknown,
}

#[derive(Debug, Clone)]
pub struct CustomType {
    pub name: String,
    pub fields: Option<HashMap<String, Type>>,  // For structs
    pub methods: Option<HashMap<String, Type>>, // For interfaces
    pub variants: Option<Vec<String>>,          // For enums
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    CharLiteral(char),
    Nil,
    Variable(Identifier),
    PrefixOp {
        op: TokenKind,
        expr: ExprBox,
    },
    BinaryOp {
        op: TokenKind,
        lhs: ExprBox,
        rhs: ExprBox,
    },
    PostfixOp {
        op: TokenKind,
        expr: ExprBox,
    },
    Assignment {
        name: String,
        type_: TypeBox,
        value: ExprBox,
    },
    FunctionCall {
        name: String,
        args: Vec<ExprBox>,
    },
    ArrayInitialization {
        elements: Vec<ExprBox>,
    },
    ArrayAccess {
        name: String,
        index: ExprBox,
    },
    StructAccess {
        name: String,
        field: ExprBox,
    },
    StructInstantiation {
        name: String,
        members: Vec<(String, ExprBox)>,
    },
    Comment(String),
    BlockComment(String),
}

impl Expr {
    pub fn type_to_string(&self) -> String {
        match self {
            Expr::IntegerLiteral(i) => i.to_string(),
            Expr::FloatLiteral(fl) => fl.to_string(),
            Expr::StringLiteral(s) => s.clone(),
            Expr::BoolLiteral(b) => b.to_string(),
            Expr::CharLiteral(c) => c.to_string(),
            Expr::Nil => "nil".to_string(),
            _ => "unknown".to_string(),
        }
    }

    pub fn from_string_to_type(s: String) -> Expr {
        if let Ok(i) = s.parse::<i64>() {
            return Expr::IntegerLiteral(i);
        }
        if let Ok(fl) = s.parse::<f64>() {
            return Expr::FloatLiteral(fl);
        }
        if s == "nil" {
            return Expr::Nil;
        }
        if s == "true" {
            return Expr::BoolLiteral(true);
        }
        if s == "false" {
            return Expr::BoolLiteral(false);
        }
        if s.starts_with('\'') && s.ends_with('\'') {
            return Expr::CharLiteral(s.chars().nth(1).unwrap());
        }
        if s.starts_with('"') && s.ends_with('"') {
            return Expr::StringLiteral(s[1..s.len() - 1].to_string());
        }
        Expr::Nil
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    PackageDeclaration(Identifier),
    UseDeclaration(Identifier),
    Expression(ExprBox),
    ConstantGroup {
        constants: Vec<StmtBox>,
    },
    Constant {
        is_public: bool,
        name: Identifier,
        type_: Type,
        value: ExprBox,
    },
    Let {
        name: Identifier,
        type_: Type,
        expr: ExprBox,
    },
    Interface {
        name: Identifier,
        type_: Type,
        methods: Vec<StmtBox>,
    },
    Defer {
        expr: ExprBox,
    },
    IfStmt {
        condition: ExprBox,
        body: Vec<StmtBox>,
        else_stmt: Option<StmtBox>,
    },
    RangeStmt {
        iterator: Identifier,
        range: ExprBox,
        body: Vec<StmtBox>,
    },
    WhileStmt {
        condition: ExprBox,
        body: Vec<StmtBox>,
    },
    Block {
        stmts: Vec<StmtBox>,
    },
    Return {
        exprs: Vec<ExprBox>,
    },
    Enum {
        is_public: bool,
        name: Identifier,
        type_: Type,
        members: Vec<Identifier>,
    },
    StructDeclaration {
        is_public: bool,
        name: Identifier,
        type_: Type,
        members: Vec<StmtBox>,
    },
    StructMember {
        is_public: bool,
        name: Identifier,
        type_: Type,
    },
    InterfaceFunctionSignature {
        name: Identifier,
        generics: Option<Vec<Type>>,
        parameters: Option<Vec<(Identifier, Type)>>,
        return_type: Option<Vec<Type>>,
    },
    FunctionDeclaration {
        is_public: bool,
        name: Identifier,
        generics: Option<Vec<Type>>,
        parameters: Option<Vec<(Identifier, Type)>>,
        body: Vec<StmtBox>,
        return_type: Option<Vec<Type>>,
    },
    ImplDeclaration {
        name: Identifier,
        generics: Option<Vec<Type>>,
        interfaces: Option<Vec<Type>>,
        methods: Vec<StmtBox>,
    },
    Empty,
}

impl Stmt {
    pub fn is_package_declaration(&self) -> (bool, String) {
        match self {
            Stmt::PackageDeclaration(name) => (true, name.clone()),
            _ => (false, "".to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub name: String,
    pub statements: Vec<StmtBox>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AST {
    pub files: Vec<SourceFile>,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Char => write!(f, "char"),
            Type::Bool => write!(f, "bool"),
            Type::Enum => write!(f, "enum"),
            Type::String => write!(f, "string"),
            Type::Optional(expr) => {
                write!(f, "{:?}", expr)?;
                write!(f, "?")
            }
            Type::Custom { name, generics } => {
                write!(f, "{}", name)?;
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
                Ok(())
            }
            Type::Array(lit) => write!(f, "{}[]", lit),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}

impl fmt::Display for CustomType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(fields) = &self.fields {
            write!(f, "{{\n")?;
            for (name, type_) in fields {
                write!(f, "{}: {},\n", name, type_)?;
            }
            write!(f, "}}")
        } else if let Some(methods) = &self.methods {
            write!(f, "{{\n")?;
            for (name, type_) in methods {
                write!(f, "{}: {},\n", name, type_)?;
            }
            write!(f, "}}")
        } else if let Some(variants) = &self.variants {
            write!(f, "{{\n")?;
            for variant in variants {
                write!(f, "{},\n", variant)?;
            }
            write!(f, "}}")
        } else {
            Ok(())
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::IntegerLiteral(i) => write!(f, "{}", i),
            Expr::FloatLiteral(fl) => write!(f, "{}", fl),
            Expr::StringLiteral(s) => write!(f, r#""{}""#, s),
            Expr::BoolLiteral(b) => write!(f, "{}", b),
            Expr::CharLiteral(c) => write!(f, "'{}'", c),
            Expr::Nil => write!(f, "nil"),
            Expr::PrefixOp { op, expr } => write!(f, "({} {})", op, expr),
            Expr::BinaryOp { op, lhs, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::PostfixOp { op, expr } => write!(f, "({} {})", expr, op),
            Expr::Assignment { name, type_, value } => write!(f, "{}: {} = {}", name, type_, value),
            Expr::FunctionCall { name, args } => {
                write!(f, "{}(", name)?;
                for arg in args {
                    write!(f, "{},", arg)?;
                }
                write!(f, ")")
            }
            Expr::ArrayInitialization { elements } => {
                write!(f, "[")?;
                for element in elements {
                    write!(f, "{},", element)?;
                }
                write!(f, "]")
            }
            Expr::ArrayAccess { name: array, index } => write!(f, "{}[{}]", array, index),
            Expr::StructAccess {
                name: struct_name,
                field,
            } => write!(f, "{}.{}", struct_name, field),
            Expr::StructInstantiation { name, members } => {
                write!(f, "{}", name)?;
                write!(f, "{{")?;
                for (name, value) in members {
                    write!(f, "{}: {},", name, value)?;
                }
                write!(f, "}}")
            }
            Expr::Comment(text) => write!(f, "//{}", text),
            Expr::BlockComment(text) => write!(f, "/*{}*/", text),
            Expr::Variable(identifier) => write!(f, "{}", identifier),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Let {
                name,
                type_,
                expr: value,
            } => write!(f, "let {}: {} = {};", name, type_, value),
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
            Stmt::Block { stmts } => {
                write!(f, "{{\n")?;
                for stmt in stmts {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")
            }
            Stmt::Return { exprs: value } => {
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
            Stmt::PackageDeclaration(name) => write!(f, "package {};", name),
            Stmt::UseDeclaration(name) => write!(f, "use {};", name),
            Stmt::Defer { expr: stmt } => write!(f, "defer {{\n{}\n}}", stmt),
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
            Stmt::Constant {
                is_public,
                name,
                type_,
                value,
            } => {
                if *is_public {
                    write!(f, "pub ")?;
                }
                write!(f, "const {}: {} = {};", name, type_, value)
            }
            Stmt::Expression(expr) => {
                write!(f, "{}", expr)
            }
            Stmt::StructMember { is_public, name, type_ } => {
                if *is_public {
                    write!(f, "pub ")?;
                }
                write!(f, "{}: {}", name, type_)
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
