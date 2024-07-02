use std::any::Any;
use std::collections::HashMap;

pub enum SymbolKind {
    Identifier,
    Function,
    Parameter,
    Constant,
    Enum,
    EnumMember,
    Interface,
    InterfaceFunction,
    Struct,
    StructMember,
    Return,
    Impl,
    Use,
    Package,
}

pub struct Symbol {
    name: String,        // Name of the symbol or Lexeme token
    kind: SymbolKind,    // Kind of symbol
    value: Box<dyn Any>, // Value of the symbol
    is_public: bool,     // Is the symbol public
}

impl Symbol {
    pub fn new(name: String, kind: SymbolKind, value: Box<dyn Any>, is_public: bool) -> Symbol {
        Symbol {
            name,
            kind,
            value,
            is_public,
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_kind(&self) -> &SymbolKind {
        &self.kind
    }

    pub fn get_value(&self) -> &Box<dyn Any> {
        &self.value
    }

    pub fn is_public(&self) -> bool {
        self.is_public
    }
}

pub struct GlobalSymbolTable {
    packages: HashMap<String, PackageSymbolTable>,
    current_package_name: String,
}

impl GlobalSymbolTable {
    pub fn new() -> GlobalSymbolTable {
        GlobalSymbolTable {
            packages: HashMap::new(),
            current_package_name: String::new(),
        }
    }

    pub fn set_current_package_name(&mut self, name: String) {
        self.current_package_name = name;
    }

    pub fn add_package_symbol_table(&mut self, package_name: &str) {
        if !self.packages.contains_key(package_name) {
            self.packages.insert(
                package_name.to_string(),
                PackageSymbolTable::new(package_name.to_string()),
            );
        }
    }

    // TODO:: this should return a Result since we don't want to brutally panic
    pub fn insert_symbol_package(&mut self, package_name: &str, symbol: Symbol) {
        if self.packages.contains_key(package_name) {
            self.packages.get_mut(package_name).unwrap().insert(symbol);
        } else {
            panic!("Package `{}` not found in the global symbol table", package_name);
        }
    }

    // TODO:: this should return a Result since we don't want to brutally panic
    pub fn get_package(&mut self, package_name: &str) -> Option<&mut PackageSymbolTable> {
        if self.packages.contains_key(package_name) {
            return Some(self.packages.get_mut(package_name).unwrap());
        } else {
            None
        }
    }

    pub fn get_current_package(&mut self) -> &mut PackageSymbolTable {
        self.packages.get_mut(&self.current_package_name).unwrap()
    }
}

// TODO: the values don't yet contain generics types
pub struct PackageSymbolTable {
    name: String, // package name
    // identifiers: (Name, Symbol) --> this can contain variables, constants, uses, parameters, return values, etc
    identifiers: HashMap<String, Symbol>,
    enums: HashMap<String, Symbol>,
    functions: HashMap<String, Symbol>,
    interfaces: HashMap<String, Symbol>,
    structs: HashMap<String, Symbol>,
    impls: HashMap<String, Symbol>,

    current_scope_level: usize,

    nested_symbol_tables: Vec<NestedSymbolTable>,
}

impl PackageSymbolTable {
    pub fn new(name: String) -> PackageSymbolTable {
        PackageSymbolTable {
            name,
            identifiers: HashMap::new(),
            enums: HashMap::new(),
            functions: HashMap::new(),
            interfaces: HashMap::new(),
            structs: HashMap::new(),
            impls: HashMap::new(),
            nested_symbol_tables: Vec::new(),
            current_scope_level: 0,
        }
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn insert(&mut self, symbol: Symbol) {
        match symbol.kind {
            SymbolKind::Identifier => {
                self.identifiers.insert(symbol.name.clone(), symbol);
            }
            SymbolKind::Enum => {
                self.enums.insert(symbol.name.clone(), symbol);
            }
            SymbolKind::Function => {
                self.functions.insert(symbol.name.clone(), symbol);
            }
            SymbolKind::Interface => {
                self.interfaces.insert(symbol.name.clone(), symbol);
            }
            SymbolKind::Struct => {
                self.structs.insert(symbol.name.clone(), symbol);
            }
            SymbolKind::Impl => {
                self.impls.insert(symbol.name.clone(), symbol);
            }
            SymbolKind::Parameter => todo!(),
            SymbolKind::Constant => todo!(),
            SymbolKind::EnumMember => todo!(),
            SymbolKind::InterfaceFunction => todo!(),
            SymbolKind::StructMember => todo!(),
            SymbolKind::Return => todo!(),
            SymbolKind::Use => todo!(),
            SymbolKind::Package => todo!(),
        }
    }

    pub fn get_identifier(&self, name: &str) -> Option<&Symbol> {
        self.identifiers.get(name)
    }

    pub fn get_enum(&self, name: &str) -> Option<&Symbol> {
        self.enums.get(name)
    }

    pub fn get_function(&self, name: &str) -> Option<&Symbol> {
        self.functions.get(name)
    }

    pub fn get_interface(&self, name: &str) -> Option<&Symbol> {
        self.interfaces.get(name)
    }

    pub fn get_struct(&self, name: &str) -> Option<&Symbol> {
        self.structs.get(name)
    }

    pub fn get_impl(&self, name: &str) -> Option<&Symbol> {
        self.impls.get(name)
    }

    pub fn enter_scope(&mut self, name: String) {
        self.current_scope_level += 1;
        if self.nested_symbol_tables.len() == self.current_scope_level {
            self.nested_symbol_tables.push(NestedSymbolTable::new(name));
        }
    }

    pub fn exit_scope(&mut self) {
        if self.current_scope_level == 0 {
            return;
        }
        self.current_scope_level -= 1;
    }

    pub fn get_current_nested_scope(&mut self) -> &mut NestedSymbolTable {
        self.nested_symbol_tables.get_mut(self.current_scope_level).unwrap()
    }
}

pub struct NestedSymbolTable {
    name: String, // this is the same name of the function, impl, etc
    identifiers: HashMap<String, Symbol>,
}

impl NestedSymbolTable {
    pub fn new(name: String) -> NestedSymbolTable {
        NestedSymbolTable {
            name,
            identifiers: HashMap::new(),
        }
    }

    pub fn insert(&mut self, symbol: Symbol) {
        self.identifiers.insert(symbol.name.clone(), symbol);
    }

    pub fn get_identifier(&self, name: &str) -> Option<&Symbol> {
        self.identifiers.get(name)
    }
}
