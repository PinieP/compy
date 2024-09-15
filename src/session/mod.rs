use std::{cell::RefCell, collections::HashMap};

use crate::source::SourceMap;
use crate::util::{Arena, IndexWrapper, IntegerWrapper};

pub struct Session {
    pub source_map: SourceMap,
}

pub struct SymbolTable<'a> {
    str_to_symbol: HashMap<&'a str, Symbol>,
    symbol_to_str: Vec<&'a str>,
}

impl<'a> SymbolTable<'a> {
    fn new() -> Self {
        SymbolTable {
            str_to_symbol: HashMap::new(),
            symbol_to_str: Vec::new(),
        }
    }

    fn new_symbol_from_str(&mut self, text: &'a str) -> Symbol {
        let index = self.symbol_to_str.len();
        //let string = self.arena.alloc_str(text);
        // lying to the compiler
        //let string: &'static str = unsafe { &*(string as *const str) };
        let string = text;

        self.symbol_to_str.push(string);
        let symbol = Symbol::from_usize(index);
        if let Some(_) = self.str_to_symbol.insert(string, symbol) {
            panic!("symbol already exists")
        }
        symbol
    }
    fn get(&self, symbol: Symbol) -> &'a str {
        self.symbol_to_str[symbol.as_usize()]
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(u32);

impl IntegerWrapper for Symbol {
    type Wrapped = u32;

    fn index(self) -> Self::Wrapped {
        self.0
    }

    fn from_index(index: Self::Wrapped) -> Self {
        Symbol(index)
    }
}
impl IndexWrapper for Symbol {}

impl Symbol {
    fn as_str<'a>(self) -> &'a str {
        with_globals(|globals| globals.symbol_table.get(self))
    }
}

pub struct Globals {
    pub symbol_table: SymbolTable<'static>,
    pub source_map: RefCell<SourceMap>,
}

scoped_thread_local!(static GLOBALS: Globals);
pub fn with_globals<R>(closure: impl FnOnce(&Globals) -> R) -> R {
    GLOBALS.with(closure)
}

#[cfg(test)]
mod test {
    use crate::source::SourceFile;

    use super::*;
    fn test() {
        let file = SourceFile::new("file.lang", "file file file file\nfile file file");
        GLOBALS.set(
            &Globals {
                symbol_table: SymbolTable::new(),
                source_map: RefCell::new(SourceMap::new()),
            },
            || {
                with_globals(|globals| globals.source_map.borrow_mut().add_source_file(file));
                eprint!("s");
            },
        );
    }
}
