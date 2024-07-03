use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::Type;

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable(String), // Variable with its type as a string
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
    Interface(Rc<RefCell<SymbolTable>>), // Interface with its own symbol table
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
        name: String,
        parameters: Vec<(String, Type)>, // Vector of (parameter name, parameter type)
        return_type: Option<Vec<Type>>,  // Optional vector of return types
        body: Rc<RefCell<SymbolTable>>,  // Function body with its own symbol table
    },
    MainFunction {
        body: Rc<RefCell<SymbolTable>>, // Main function body with its own symbol table
    },
    ForLoop {
        iterator: String,
        iterable: String,
        body: Rc<RefCell<SymbolTable>>, // For loop body with its own symbol table
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
    Package(String), // Package name
    Use(String),     // Use statement
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
                Some(parent_scope) => parent_scope.borrow().lookup(name),
                None => None,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct GlobalSymbolTable {
    // Global symbol table in which all the symbols are stored
    // here we store mostly at package level
    scopes: Vec<Rc<RefCell<SymbolTable>>>,
    current_scope_level: usize,
}

impl GlobalSymbolTable {
    pub fn new() -> Self {
        GlobalSymbolTable {
            scopes: Vec::new(),
            current_scope_level: 0,
        }
    }

    pub fn lookup_symbol(&self, name: &str) -> Option<Symbol> {
        for symbol_table in self.scopes.iter().rev() {
            match symbol_table.borrow().lookup(name) {
                Some(symbol) => return Some(symbol),
                None => continue,
            }
        }
        None
    }

    pub fn get_symbol_table_by_name(&self, name: &str) -> Option<Rc<RefCell<SymbolTable>>> {
        for symbol_table in self.scopes.iter().rev() {
            if symbol_table.borrow().name == name {
                return Some(symbol_table.clone());
            }
        }
        None
    }

    pub fn enter_scope(&mut self, name: String) {
        let parent = self.scopes.last().cloned();
        let symbol_table = Rc::new(RefCell::new(SymbolTable::new(name, parent)));

        self.scopes.push(symbol_table);
        self.current_scope_level += 1;
    }

    pub fn exit_scope(&mut self) {
        if self.current_scope_level == 0 {
            panic!("Cannot exit the global scope");
        }
        self.current_scope_level -= 1;
    }

    pub fn current_scope(&mut self) -> Option<&mut Rc<RefCell<SymbolTable>>> {
        self.scopes.get_mut(self.current_scope_level)
    }
}
