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
    pub packages: HashMap<String, PackageSymbolTable>,
}

impl GlobalSymbolTable {
    pub fn new() -> GlobalSymbolTable {
        GlobalSymbolTable {
            packages: HashMap::new(),
        }
    }

    pub fn insert(&mut self, package_name: &str, symbol: Symbol) {
        if !self.packages.contains_key(package_name) {
            self.packages.insert(
                package_name.to_string(),
                PackageSymbolTable::new(package_name.to_string()),
            );
        }

        let package_symbol_table = self.packages.get_mut(package_name).unwrap();
        package_symbol_table.insert(symbol);
    }

    pub fn get_package(&self, package_name: &str) -> Option<&PackageSymbolTable> {
        self.packages.get(package_name)
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
            _ => {}
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

    pub fn add_nested_symbol_table(&mut self, nested_symbol_table: NestedSymbolTable) {
        self.nested_symbol_tables.push(nested_symbol_table);
    }
}

pub struct NestedSymbolTable {
    name: String, // this is the same name of the function, impl, etc
    identifiers: HashMap<String, Symbol>,

    nested_symbol_table: Vec<NestedSymbolTable>,
}

impl NestedSymbolTable {
    pub fn new(name: String) -> NestedSymbolTable {
        NestedSymbolTable {
            name,
            identifiers: HashMap::new(),
            nested_symbol_table: Vec::new(),
        }
    }

    pub fn insert(&mut self, symbol: Symbol) {
        self.identifiers.insert(symbol.name.clone(), symbol);
    }

    pub fn get_identifier(&self, name: &str) -> Option<&Symbol> {
        self.identifiers.get(name)
    }

    pub fn add_nested_symbol_table(&mut self, nested_symbol_table: NestedSymbolTable) {
        self.nested_symbol_table.push(nested_symbol_table);
    }
}
