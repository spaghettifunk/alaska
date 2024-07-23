use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;

use crate::parser::ast::Type;

#[derive(Debug, Clone)]
pub enum Symbol {
    Identifier {
        name: String,
        type_: Type,   // Identifier type
        value: String, // Identifier value as a String
    },
    Constant {
        name: String,
        type_: Type,   // Constant type
        value: String, // Constant with its value as a String
    },
    Struct {
        is_public: bool,
        name: String,
        type_: Type, // Struct type
        table: SymbolTable,
    }, // Struct with its own symbol table
    StructMember {
        is_public: bool,
        name: String,
        type_: Type, // Struct member type
    },
    Impl {
        name: String,
        type_: Type,
        interfaces: Option<Vec<Type>>, // Vector of interface names
        table: SymbolTable,            // Impl with its own symbol table
    },
    Interface {
        name: String,
        type_: Type,
        table: SymbolTable, // Interface with its own symbol table
    },
    InterfaceMethod {
        name: String,
        parameters: Option<Vec<Symbol>>, // Vector of (parameter name, parameter type)
        return_type: Option<Type>,       // Optional vector of return types
    },
    Block(Option<SymbolTable>), // Block with its own symbol table
    Enum {
        is_public: bool,
        name: String,
        type_: Type,
        table: SymbolTable,
    },
    EnumMember {
        name: String,
        type_: Type,
        value: String, // Enum member value as a String
    },
    Function {
        is_public: bool,
        name: String,
        parameters: Option<Vec<Symbol>>, // Vector of Symbol::FunctionParameter
        return_type: Option<Type>,       // Optional return type
        body: SymbolTable,               // Function body with its own symbol table
    },
    FunctionCall {
        name: String,
        arguments: Option<Vec<Symbol>>, // Vector of argument names
    },
    RangeLoop {
        iterator: String,
        iterable: String,
        body: SymbolTable, // Range loop body with its own symbol table
    },
    WhileLoop {
        condition: String,
        body: SymbolTable, // While loop body with its own symbol table
    },
    IfStatement {
        condition: String,
        then_body: SymbolTable,         // Then body with its own symbol table
        else_body: Option<SymbolTable>, // Optional else body with its own symbol table
    },
    Use(String), // Use statement
}

pub trait SymbolInfo {
    fn name(&self) -> Option<&str>;
    fn type_(&self) -> Option<&Type>;
    fn value(&self) -> Option<&str>;
    fn table(&self) -> Option<&SymbolTable>;
    fn is_public(&self) -> Option<bool>;
    fn parameters(&self) -> Option<Vec<Symbol>>;
    fn return_type(&self) -> Option<Type>;
    fn update_arguments(&mut self, arguments: Vec<Symbol>);
}

impl SymbolInfo for Symbol {
    fn name(&self) -> Option<&str> {
        match self {
            Symbol::Identifier { name, .. } => Some(name),
            Symbol::Constant { name, .. } => Some(name),
            Symbol::Struct { name, .. } => Some(name),
            Symbol::StructMember { name, .. } => Some(name),
            Symbol::Impl { name, .. } => Some(name),
            Symbol::Interface { name, .. } => Some(name),
            Symbol::InterfaceMethod { name, .. } => Some(name),
            Symbol::Enum { name, .. } => Some(name),
            Symbol::EnumMember { name, .. } => Some(name),
            Symbol::Function { name, .. } => Some(name),
            Symbol::FunctionCall { name, .. } => Some(name),
            Symbol::Use(name) => Some(name),
            _ => None,
        }
    }

    fn type_(&self) -> Option<&Type> {
        match self {
            Symbol::Identifier { type_, .. } => Some(type_),
            Symbol::Constant { type_, .. } => Some(type_),
            Symbol::Struct { type_, .. } => Some(type_),
            Symbol::StructMember { type_, .. } => Some(type_),
            Symbol::Impl { type_, .. } => Some(type_),
            Symbol::Interface { type_, .. } => Some(type_),
            Symbol::Enum { type_, .. } => Some(type_),
            Symbol::EnumMember { type_, .. } => Some(type_),
            _ => None,
        }
    }

    fn value(&self) -> Option<&str> {
        match self {
            Symbol::Identifier { value, .. } => Some(value),
            Symbol::Constant { value, .. } => Some(value),
            Symbol::EnumMember { value, .. } => Some(value),
            _ => None,
        }
    }

    fn table(&self) -> Option<&SymbolTable> {
        match self {
            Symbol::Struct { table, .. } => Some(table),
            Symbol::Impl { table, .. } => Some(table),
            Symbol::Interface { table, .. } => Some(table),
            Symbol::Enum { table, .. } => Some(table),
            Symbol::Block(table) => table.as_ref(),
            _ => None,
        }
    }

    fn is_public(&self) -> Option<bool> {
        match self {
            Symbol::Struct { is_public, .. } => Some(*is_public),
            Symbol::StructMember { is_public, .. } => Some(*is_public),
            Symbol::Enum { is_public, .. } => Some(*is_public),
            Symbol::Function { is_public, .. } => Some(*is_public),
            _ => None,
        }
    }

    fn parameters(&self) -> Option<Vec<Symbol>> {
        match self {
            Symbol::InterfaceMethod { parameters, .. } => parameters.clone(),
            Symbol::Function { parameters, .. } => parameters.clone(),
            _ => None,
        }
    }

    fn return_type(&self) -> Option<Type> {
        match self {
            Symbol::InterfaceMethod { return_type, .. } => return_type.clone(),
            Symbol::Function { return_type, .. } => return_type.clone(),
            _ => None,
        }
    }

    fn update_arguments(&mut self, arguments: Vec<Symbol>) {
        match self {
            Symbol::FunctionCall { arguments: args, .. } => {
                *args = Some(arguments);
            }
            _ => {}
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Identifier { name, type_, value } => {
                write!(f, "Identifier(name: {}, type: {}, value: {})", name, type_, value)
            }
            Symbol::Constant { name, type_, value } => {
                write!(f, "Constant(name: {}, type: {}, value: {})", name, type_, value)
            }
            Symbol::Struct { is_public, name, .. } => {
                write!(f, "Struct(name: {}, is_public: {})", name, is_public,)
            }
            Symbol::StructMember { is_public, name, type_ } => {
                write!(
                    f,
                    "StructMember(name: {}, is_public: {}, type: {})",
                    name, is_public, type_
                )
            }
            Symbol::Impl { name, interfaces, .. } => {
                if let Some(interfaces) = interfaces {
                    let interfaces_str: Vec<String> = interfaces.iter().map(|i| i.to_string()).collect();
                    write!(f, "Impl(name: {}, interfaces: [{}])\n", name, interfaces_str.join(", "))
                } else {
                    write!(f, "Impl(name: {}, interfaces: None)\n", name)
                }
            }
            Symbol::Interface { name, .. } => {
                write!(f, "Interface(name: {})\n", name)
            }
            Symbol::InterfaceMethod {
                name,
                parameters,
                return_type,
            } => {
                write!(f, "InterfaceMethod(name: {}, parameters: [", name)?;
                if parameters.is_some() {
                    for param in parameters.as_ref().unwrap().iter() {
                        write!(f, "{}, ", param)?;
                    }
                }
                write!(f, "], return_type: [")?;
                if let Some(return_type) = return_type {
                    write!(f, "{}, ", return_type)?;
                }
                write!(f, "])")
            }
            Symbol::Enum { is_public, name, .. } => {
                write!(f, "Enum(name: {}, is_public: {})\n", name, is_public)
            }
            Symbol::EnumMember { name, value, type_ } => {
                write!(f, "EnumMember(name: {}, value: {}, type: {})", name, value, type_)
            }
            Symbol::Function {
                is_public,
                name,
                parameters,
                return_type,
                ..
            } => {
                write!(f, "Function(name: {}, is_public: {}, parameters: [", name, is_public)?;
                if let Some(parameters) = parameters {
                    for symbol in parameters.iter() {
                        write!(f, "({}), ", symbol)?;
                    }
                }
                write!(f, "], return_type: [")?;
                if let Some(return_type) = return_type {
                    write!(f, "{}, ", return_type)?;
                }
                write!(f, "])\n")
            }
            Symbol::RangeLoop { iterator, iterable, .. } => {
                write!(f, "RangeLoop {{\n",)?;
                write!(f, "  iterator: {}\n", iterator)?;
                write!(f, "  iterable: {}\n", iterable)?;
                write!(f, "}}\n")
            }
            Symbol::WhileLoop { condition, .. } => {
                write!(f, "WhileLoop {{\n",)?;
                write!(f, "   condition: {}\n", condition)?;
                write!(f, "}}\n")
            }
            Symbol::IfStatement { condition, .. } => {
                write!(f, "IfStatement: {}", condition)
            }
            Symbol::Use(name) => {
                write!(f, "Use: {}\n", name)
            }
            Symbol::Block(..) => {
                write!(f, "Block\n")
            }
            Symbol::FunctionCall { name, arguments } => {
                write!(f, "FunctionCall(name: {}, arguments: [", name)?;
                if let Some(arguments) = arguments {
                    for arg in arguments.iter() {
                        write!(f, "{}, ", arg)?;
                    }
                }
                write!(f, "])")
            }
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct SymbolTable {
    pub name: String,
    symbols: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new(name: String) -> Self {
        SymbolTable {
            name,
            symbols: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, name: String, symbol: Symbol) {
        self.symbols.insert(name, symbol);
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    pub fn get_symbols(&self) -> &HashMap<String, Symbol> {
        &self.symbols
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "SymbolTable: {}", self.name)?;
        writeln!(f, "{}", "-".repeat(40))?;
        writeln!(f, "{:<15} | {:<20}", "Symbol", "Details")?;
        writeln!(f, "{}", "-".repeat(40))?;
        for (key, symbol) in &self.symbols {
            writeln!(f, "{:<15} | {}", key, symbol)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Context {
    scopes: Vec<SymbolTable>,
}

impl Context {
    pub fn new() -> Self {
        Context { scopes: Vec::new() }
    }

    pub fn enter_scope(&mut self, name: String) {
        let new_table = SymbolTable::new(name.clone());
        self.scopes.push(new_table);
    }

    pub fn exit_scope(&mut self) -> Option<SymbolTable> {
        self.scopes.pop()
    }

    pub fn current_scope(&self) -> Option<&SymbolTable> {
        self.scopes.last()
    }

    pub fn current_scope_mut(&mut self) -> Option<&mut SymbolTable> {
        self.scopes.last_mut()
    }

    pub fn lookup_current_scope(&self, symbol: &str) -> Option<&Symbol> {
        if let Some(table) = self.current_scope() {
            return table.lookup(symbol);
        }
        None
    }

    pub fn lookup(&self, symbol: &str) -> Option<&Symbol> {
        for table in self.scopes.iter().rev() {
            if let Some(sym) = table.symbols.get(symbol) {
                return Some(sym);
            }
        }
        None
    }

    pub fn lookup_mut(&mut self, symbol: &str) -> Option<&mut Symbol> {
        for table in self.scopes.iter_mut().rev() {
            if let Some(sym) = table.symbols.get_mut(symbol) {
                return Some(sym);
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct GlobalSymbolTable {
    // Global symbol table in which all the symbols are stored
    // here we store mostly at package level
    pub packages: HashMap<String, SymbolTable>,
}

impl GlobalSymbolTable {
    pub fn new() -> Self {
        GlobalSymbolTable {
            packages: HashMap::new(),
        }
    }

    pub fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        for symbol_table in self.packages.values() {
            match symbol_table.lookup(name) {
                Some(symbol) => return Some(symbol),
                None => continue,
            }
        }
        None
    }

    pub fn get_package_symbol_table(&mut self, name: &str) -> &mut SymbolTable {
        return self
            .packages
            .entry(name.to_string().clone())
            .or_insert(SymbolTable::new(name.to_string().clone()));
    }
}

impl fmt::Display for GlobalSymbolTable {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn print_nested_tables(table: SymbolTable, depth: usize) {
            for symbol in table.symbols.values() {
                match symbol {
                    Symbol::Struct { table, .. }
                    | Symbol::Impl { table, .. }
                    | Symbol::Interface { table, .. }
                    | Symbol::Enum { table, .. }
                    | Symbol::Block(Some(table))
                    | Symbol::Function { body: table, .. }
                    | Symbol::RangeLoop { body: table, .. }
                    | Symbol::WhileLoop { body: table, .. } => {
                        println!("Nested Table: {}", table.borrow().name);
                        println!("{}", table);
                        print_nested_tables(table.clone(), depth + 1);
                    }
                    Symbol::IfStatement {
                        then_body, else_body, ..
                    } => {
                        println!("Then Table: {}", then_body.borrow().name);
                        println!("{}", then_body);
                        print_nested_tables(then_body.clone(), depth + 1);
                        if let Some(else_body) = else_body {
                            println!("Else Table: {}", else_body.borrow().name);
                            println!("{}", else_body);
                            print_nested_tables(else_body.clone(), depth + 1);
                        }
                    }
                    _ => {}
                }
            }
        }

        Ok(for (package_name, symbol_table) in self.packages.clone() {
            println!("Package: {}", package_name);
            println!("{}", symbol_table.borrow());
            print_nested_tables(symbol_table, 1);
        })
    }
}
