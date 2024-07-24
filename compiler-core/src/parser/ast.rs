use crate::lexer::TokenKind;
use std::{collections::HashMap, fmt};

pub type Identifier = String;
pub type TypeBox = Box<Type>;
pub type ExprBox = Box<Expr>;
pub type StmtBox = Box<Stmt>;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Int,
    Float,
    Char,
    Bool,
    Enum,
    String,
    Array(TypeBox, usize),
    Struct { name: String },
    Interface { name: String },
    Custom { name: String },
    Optional(TypeBox), // Represents an optional type
    Nil,
    Unknown,
}

impl Type {
    pub fn is_equal(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) => true,
            (Type::Float, Type::Float) => true,
            (Type::Char, Type::Char) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Enum, Type::Enum) => true,
            (Type::String, Type::String) => true,
            (Type::Array(lit1, _), Type::Array(lit2, _)) => lit1.is_equal(lit2),
            (Type::Struct { name: name1 }, Type::Struct { name: name2 }) => name1 == name2,
            (Type::Interface { name: name1 }, Type::Interface { name: name2 }) => name1 == name2,
            (Type::Optional(lit1), Type::Optional(lit2)) => lit1.is_equal(lit2),
            (Type::Nil, Type::Nil) => true,
            _ => false,
        }
    }

    pub fn variant_eq(&self, other: &Type) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Type::Nil => true,
            _ => false,
        }
    }

    pub fn from_string_to_type(s: String) -> Type {
        match s.as_str() {
            "int" => Type::Int,
            "float" => Type::Float,
            "char" => Type::Char,
            "bool" => Type::Bool,
            "string" => Type::String,
            _ => Type::Unknown,
        }
    }

    pub fn get_name(&self) -> Option<String> {
        match self {
            Type::Struct { name } => Some(name.clone()),
            Type::Interface { name } => Some(name.clone()),
            Type::Custom { name } => Some(name.clone()),
            _ => None,
        }
    }
}

pub trait ExprInfo {
    fn name(&self) -> Option<&str>;
    fn type_(&self) -> Option<&Type>;
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
    /// Represents a function call in the AST.
    ///
    /// # Fields
    ///
    /// * `name`: The name of the function being called.
    /// * `args`: The arguments passed to the function call.
    ///
    /// # Example
    ///
    /// ```
    /// let result = my_fn(10, 20);
    /// };
    /// ```
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
    /// Represents a struct access expression.
    ///
    /// This struct is used to access a field of a struct by providing the struct's name and the field expression.
    /// The `name` field is a `String` that represents the name of the struct being accessed.
    /// The `field` field is an `ExprBox` that represents the expression used to access the field.
    ///
    /// # Example
    ///
    /// ```
    /// let x = MyStruct{name: "davide", age: 20};
    /// let a = x.age;
    /// let n = x.name;
    /// };
    /// ```
    StructAccess {
        name: String,
        field: ExprBox,
    },
    /// Expression representing a struct instantiation
    ///
    /// # Example
    ///
    /// ```
    /// struct Person {
    ///     name: string,
    ///     age: int,
    /// }
    /// ```
    StructInstantiation {
        name: String,
        members: Vec<(String, ExprBox)>,
    },
    StructMember {
        is_public: bool,
        name: Identifier,
        type_: Type,
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

impl ExprInfo for Expr {
    fn name(&self) -> Option<&str> {
        match self {
            Expr::Assignment { name, .. } => Some(name),
            Expr::FunctionCall { name, .. } => Some(name),
            Expr::ArrayAccess { name, .. } => Some(name),
            Expr::StructAccess { name, .. } => Some(name),
            Expr::StructInstantiation { name, .. } => Some(name),
            Expr::StructMember { name, .. } => Some(name),
            Expr::Variable(name) => Some(name),
            _ => None,
        }
    }

    fn type_(&self) -> Option<&Type> {
        match self {
            Expr::Assignment { type_, .. } => Some(type_.as_ref()),
            Expr::StructMember { type_, .. } => Some(type_),
            Expr::Variable(_) => Some(&Type::Unknown),
            _ => None,
        }
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
        else_stmt: Option<Vec<StmtBox>>,
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
        members: Vec<ExprBox>,
    },
    InterfaceFunctionSignature {
        name: Identifier,
        generics: Option<Vec<Type>>,
        parameters: Option<HashMap<Identifier, Type>>,
        return_type: Option<Type>,
    },
    FunctionDeclaration {
        is_public: bool,
        name: Identifier,
        generics: Option<Vec<Type>>,
        parameters: Option<Vec<(Identifier, Type)>>,
        body: Vec<StmtBox>,
        return_type: Option<Type>,
    },
    ImplDeclaration {
        name: Identifier,
        generics: Option<Vec<Type>>,
        interfaces: Option<Vec<Type>>,
        methods: Vec<StmtBox>,
    },
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub name: String,
    pub package: String,
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
            Type::Struct { name } => {
                write!(f, "struct {}", name)?;
                Ok(())
            }
            Type::Interface { name } => {
                write!(f, "interface {}", name)?;
                Ok(())
            }
            Type::Custom { name } => {
                write!(f, "custom {}", name)?;
                Ok(())
            }
            Type::Array(lit, size) => write!(f, "{}[{}]", lit, size),
            Type::Unknown => write!(f, "unknown"),
            Type::Nil => write!(f, "nil"),
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
            Expr::StructMember { is_public, name, type_ } => {
                if *is_public {
                    write!(f, "pub ")?;
                }
                write!(f, "{}: {}", name, type_)
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
                    for stmt in else_stmt {
                        write!(f, " else {{\n{}\n}}", stmt)?;
                    }
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
