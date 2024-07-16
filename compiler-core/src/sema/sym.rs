use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

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
        parameters: Option<Vec<(String, Type)>>, // Vector of (parameter name, parameter type)
        return_type: Option<Vec<Type>>,          // Optional vector of return types
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
        return_type: Option<Vec<Type>>,  // Optional vector of return types
        body: SymbolTable,               // Function body with its own symbol table
    },
    FunctionCall {
        name: String,
        arguments: Option<Vec<Symbol>>, // Vector of argument names
    },
    ForLoop {
        iterator: String,
        start: String,
        end: String,
        body: SymbolTable, // For loop body with its own symbol table
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
                    for (param_name, param_type) in parameters.as_ref().unwrap().iter() {
                        write!(f, "({}, {}), ", param_name, param_type)?;
                    }
                }
                write!(f, "], return_type: [")?;
                if let Some(return_type) = return_type {
                    for r in return_type.iter() {
                        write!(f, "{}, ", r)?;
                    }
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
                    for r in return_type.iter() {
                        write!(f, "{}, ", r)?;
                    }
                }
                write!(f, "])\n")
            }
            Symbol::ForLoop {
                iterator, start, end, ..
            } => {
                write!(f, "ForLoop {{\n",)?;
                write!(f, "  iterator: {}\n", iterator)?;
                write!(f, "  start: {}\n", start)?;
                write!(f, "  end: {}\n", end)?;
                write!(f, "}}\n")
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
    name: String,
    symbols: HashMap<String, Symbol>,
    parent: Option<Rc<SymbolTable>>,
}

impl SymbolTable {
    pub fn new(name: String, parent: Option<Rc<SymbolTable>>) -> Self {
        SymbolTable {
            name,
            symbols: HashMap::new(),
            parent,
        }
    }

    pub fn add_symbol(&mut self, name: String, symbol: Symbol) {
        self.symbols.insert(name, symbol);
    }

    pub fn lookup(&self, name: &str) -> Option<Symbol> {
        if let Some(symbol) = self.symbols.get(name) {
            return Some(symbol.clone());
        }
        if let Some(parent_table) = self.parent.clone() {
            return parent_table.lookup(name);
        }
        None
    }

    pub fn lookup_local_scope(&self, name: &str) -> Option<&Symbol> {
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

pub struct Context {
    stack: Vec<SymbolTable>,
}

impl Context {
    pub fn new() -> Self {
        Context { stack: Vec::new() }
    }

    pub fn enter_scope(&mut self, name: String) {
        let new_table = SymbolTable::new() {
            name,
            symbols: HashMap::new(),
        };
        self.stack.push(new_table);
    }

    pub fn exit_scope(&mut self) {
        self.stack.pop();
    }

    pub fn current_scope(&self) -> Option<&SymbolTable> {
        self.stack.last()
    }

    pub fn current_scope_mut(&mut self) -> Option<&mut SymbolTable> {
        self.stack.last_mut()
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

    pub fn lookup_symbol(&self, name: &str) -> Option<Symbol> {
        for symbol_table in self.packages.values() {
            match symbol_table.lookup(name) {
                Some(symbol) => return Some(symbol),
                None => continue,
            }
        }
        None
    }

    pub fn get_symbol_table_by_name(&self, name: &str) -> Option<SymbolTable> {
        for symbol_table in self.packages.values() {
            if symbol_table.borrow().name == name {
                return Some(symbol_table.clone());
            }
        }
        None
    }

    pub fn add_package_symbol_table(&mut self, name: &str, symbol_table: SymbolTable) {
        self.packages.insert(name.to_string().clone(), symbol_table);
    }

    pub fn get_package_symbol_table(&mut self, name: &str) -> &mut SymbolTable {
        return self
            .packages
            .entry(name.to_string().clone())
            .or_insert(SymbolTable::new(name.to_string().clone(), None));
    }

    pub fn debug_print_table(&self) {
        for (package_name, symbol_table) in self.packages.clone() {
            println!("Package: {}", package_name);
            println!("{}", symbol_table.borrow());
            self.print_nested_tables(symbol_table, 1);
        }
    }

    fn print_nested_tables(&self, table: SymbolTable, depth: usize) {
        for symbol in table.symbols.values() {
            match symbol {
                Symbol::Struct { table, .. }
                | Symbol::Impl { table, .. }
                | Symbol::Interface { table, .. }
                | Symbol::Enum { table, .. }
                | Symbol::Block(Some(table))
                | Symbol::Function { body: table, .. }
                | Symbol::ForLoop { body: table, .. }
                | Symbol::RangeLoop { body: table, .. }
                | Symbol::WhileLoop { body: table, .. } => {
                    println!("Nested Table: {}", table.borrow().name);
                    println!("{}", table);
                    self.print_nested_tables(table.clone(), depth + 1);
                }
                Symbol::IfStatement {
                    then_body, else_body, ..
                } => {
                    println!("Then Table: {}", then_body.borrow().name);
                    println!("{}", then_body);
                    self.print_nested_tables(then_body.clone(), depth + 1);
                    if let Some(else_body) = else_body {
                        println!("Else Table: {}", else_body.borrow().name);
                        println!("{}", else_body);
                        self.print_nested_tables(else_body.clone(), depth + 1);
                    }
                }
                _ => {}
            }
        }
    }
}
