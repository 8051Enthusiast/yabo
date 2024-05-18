use std::collections::HashMap;

use anyhow::Result;
fn main() -> Result<()> {
    treereduce::cli::main(
        tree_sitter_yabo::language(),
        tree_sitter_yabo::NODE_TYPES,
        HashMap::default(),
    )
}
