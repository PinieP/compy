use std::collections::HashMap;

struct SymbolTable {
    map: HashMap<String, (u64, usize)>,
    current_scope_index: u64,
    top_scope_index: u64,
}
