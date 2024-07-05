use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::parser::ast::Type;

#[derive(Debug, Clone)]
pub enum Symbol {
    Identifier(String), // Identifier with its type as a string
    Constant {
        name: String,
        type_: Type,   // Constant type
        value: String, // Constant with its value as a String
    },
    Struct {
        is_public: bool,
        name: String,
        table: Rc<RefCell<SymbolTable>>,
    }, // Struct with its own symbol table
    StructMember {
        is_public: bool,
        name: String,
        type_: String, // Struct member type
    },
    Impl {
        name: String,
        interfaces: Option<Vec<Type>>,   // Vector of interface names
        table: Rc<RefCell<SymbolTable>>, // Impl with its own symbol table
    },
    Interface {
        name: String,
        table: Rc<RefCell<SymbolTable>>, // Interface with its own symbol table
    },
    InterfaceMethod {
        name: String,
        parameters: Vec<(String, Type)>, // Vector of (parameter name, parameter type)
        return_type: Option<Vec<Type>>,  // Optional vector of return types
    },
    Block(Option<Rc<RefCell<SymbolTable>>>), // Block with its own symbol table
    Enum {
        is_public: bool,
        name: String,
        table: Rc<RefCell<SymbolTable>>,
    },
    EnumMember {
        name: String,
        value: String, // Enum member value as a String
    },
    Function {
        is_public: bool,
        name: String,
        parameters: Vec<(String, Type)>, // Vector of (parameter name, parameter type)
        return_type: Option<Vec<Type>>,  // Optional vector of return types
        body: Rc<RefCell<SymbolTable>>,  // Function body with its own symbol table
    },
    ForLoop {
        iterator: String,
        start: String,
        end: String,
        body: Rc<RefCell<SymbolTable>>, // For loop body with its own symbol table
    },
    RangeLoop {
        iterator: String,
        iterable: String,
        body: Rc<RefCell<SymbolTable>>, // Range loop body with its own symbol table
    },
    WhileLoop {
        condition: String,
        body: Rc<RefCell<SymbolTable>>, // While loop body with its own symbol table
    },
    IfStatement {
        condition: String,
        then_body: Rc<RefCell<SymbolTable>>, // Then body with its own symbol table
        else_body: Option<Rc<RefCell<SymbolTable>>>, // Optional else body with its own symbol table
    },
    Use(String), // Use statement
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Identifier(name) => {
                write!(f, "    {}: Identifier\n", name)
            }
            Symbol::Constant { name, type_, value } => {
                write!(f, "    Constant {{\n")?;
                write!(f, "      name: {}\n", name)?;
                write!(f, "      type: {:?}\n", type_)?;
                write!(f, "      value: {}\n", value)?;
                write!(f, "    }}\n")
            }
            Symbol::Struct { is_public, name, table } => {
                write!(f, "    Struct {{\n")?;
                write!(f, "      is_public: {}\n", is_public)?;
                write!(f, "      name: {}\n", name)?;
                write!(f, "      table: {:?}\n", table)?;
                write!(f, "    }}\n")
            }
            Symbol::StructMember { is_public, name, type_ } => {
                write!(f, "    StructMember {{\n")?;
                write!(f, "      is_public: {}\n", is_public)?;
                write!(f, "      name: {}\n", name)?;
                write!(f, "      type: {}\n", type_)?;
                write!(f, "    }}\n")
            }
            Symbol::Impl {
                name,
                interfaces,
                table,
            } => {
                write!(f, "    Impl {{\n",)?;
                write!(f, "      name: {}\n", name)?;
                write!(f, "      interfaces: {:?}\n", interfaces)?;
                write!(f, "      table: {:?}\n", table)?;
                write!(f, "    }}\n")
            }
            Symbol::Interface { name, table } => {
                write!(f, "    Interface {{\n",)?;
                write!(f, "      name: {}\n", name)?;
                write!(f, "      table: {:?}\n", table)?;
                write!(f, "    }}\n")
            }
            Symbol::InterfaceMethod {
                name,
                parameters,
                return_type,
            } => {
                write!(f, "    InterfaceMethod {{\n",)?;
                write!(f, "      name: {}\n", name)?;
                write!(f, "      parameters: {:?}\n", parameters)?;
                write!(f, "      return_type: {:?}\n", return_type)?;
                write!(f, "    }}\n")
            }
            Symbol::Enum { is_public, name, table } => {
                write!(f, "    Enum {{\n",)?;
                write!(f, "      is_public: {}\n", is_public)?;
                write!(f, "      name: {}\n", name)?;
                write!(f, "      table: {:?}\n", table)?;
                write!(f, "    }}\n")
            }
            Symbol::EnumMember { name, value } => {
                write!(f, "    EnumMember {{\n",)?;
                write!(f, "      name: {}\n", name)?;
                write!(f, "      value: {}\n", value)?;
                write!(f, "    }}\n")
            }
            Symbol::Function {
                is_public,
                name,
                parameters,
                return_type,
                body,
            } => {
                write!(f, "    Function {{\n",)?;
                write!(f, "      is_public: {}\n", is_public)?;
                write!(f, "      name: {}\n", name)?;
                write!(f, "      parameters: {:?}\n", parameters)?;
                write!(f, "      return_type: {:?}\n", return_type)?;
                write!(f, "      body: {}\n", *body.borrow_mut())?;
                write!(f, "    }}\n")
            }
            Symbol::ForLoop {
                iterator,
                start,
                end,
                body,
            } => {
                write!(f, "    ForLoop {{\n",)?;
                write!(f, "      iterator: {}\n", iterator)?;
                write!(f, "      start: {}\n", start)?;
                write!(f, "      end: {}\n", end)?;
                write!(f, "      body: {}\n", *body.borrow_mut())?;
                write!(f, "    }}\n")
            }
            Symbol::RangeLoop {
                iterator,
                iterable,
                body,
            } => {
                write!(f, "    ForLoop {{\n",)?;
                write!(f, "      iterator: {}\n", iterator)?;
                write!(f, "      iterable: {}\n", iterable)?;
                write!(f, "      body: {}\n", *body.borrow_mut())?;
                write!(f, "    }}\n")
            }
            Symbol::WhileLoop { condition, body } => {
                write!(f, "    WhileLoop {{\n",)?;
                write!(f, "      condition: {}\n", condition)?;
                write!(f, "      body: {}\n", *body.borrow_mut())?;
                write!(f, "    }}\n")
            }
            Symbol::IfStatement {
                condition,
                then_body,
                else_body,
            } => {
                write!(f, "    IfStatement {{\n",)?;
                write!(f, "      condition: {}\n", condition)?;
                write!(f, "      then_body: {}\n", *then_body.borrow_mut())?;
                if else_body.is_some() {
                    write!(
                        f,
                        "      else_body: {}\n",
                        <Option<Rc<RefCell<SymbolTable>>> as Clone>::clone(&else_body)
                            .unwrap()
                            .borrow_mut()
                    )?;
                }
                write!(f, "    }}\n")
            }
            Symbol::Use(name) => {
                write!(f, "    {}: Use\n", name)
            }
            Symbol::Block(table) => {
                write!(f, "    Block {{\n",)?;
                write!(f, "      table: {}\n", table.as_ref().unwrap().borrow_mut())?;
                write!(f, "    }}\n")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    name: String,
    symbols: HashMap<String, Symbol>,
    parent: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    pub fn new(name: String, parent: Option<Rc<RefCell<SymbolTable>>>) -> Self {
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
        match self.symbols.get(name) {
            Some(symbol) => Some(symbol.clone()),
            None => match &self.parent {
                Some(parent_scope) => parent_scope.borrow_mut().lookup(name),
                None => None,
            },
        }
    }

    pub fn get_symbols(&self) -> &HashMap<String, Symbol> {
        &self.symbols
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} SymbolTable {{\n", self.name)
            .and_then(|_| {
                for (symbol_name, symbol) in self.symbols.iter() {
                    write!(f, "         {}: {:?}\n", symbol_name, symbol)?;
                }
                Ok(())
            })
            .and_then(|_| write!(f, "}}"))
    }
}

#[derive(Debug, Clone)]
pub struct GlobalSymbolTable {
    // Global symbol table in which all the symbols are stored
    // here we store mostly at package level
    packages: HashMap<String, Rc<RefCell<SymbolTable>>>,
}

impl GlobalSymbolTable {
    pub fn new() -> Self {
        GlobalSymbolTable {
            packages: HashMap::new(),
        }
    }

    pub fn lookup_symbol(&self, name: &str) -> Option<Symbol> {
        for symbol_table in self.packages.values() {
            match symbol_table.borrow_mut().lookup(name) {
                Some(symbol) => return Some(symbol),
                None => continue,
            }
        }
        None
    }

    pub fn get_symbol_table_by_name(&self, name: &str) -> Option<Rc<RefCell<SymbolTable>>> {
        for symbol_table in self.packages.values() {
            if symbol_table.borrow_mut().name == name {
                return Some(symbol_table.clone());
            }
        }
        None
    }

    pub fn new_package(&mut self, name: String, sym_name: String) {
        self.packages.insert(
            name.clone(),
            Rc::new(RefCell::new(SymbolTable::new(sym_name.clone(), None))),
        );
    }
}

impl fmt::Display for GlobalSymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "GlobalSymbolTable {{\n")?;
        for (package_name, package) in self.packages.iter() {
            write!(f, "  Package: {}\n", package_name)?;
            write!(f, "  Symbols: {{\n")?;
            for (symbol_name, symbol) in package.borrow_mut().symbols.iter() {
                write!(f, "    {}: {}\n", symbol_name, symbol)?;
            }
            write!(f, "  }}\n")?;
        }
        write!(f, "}}")
    }
}
