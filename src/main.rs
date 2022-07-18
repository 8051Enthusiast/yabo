pub mod absint;
pub mod ast;
pub mod context;
pub mod databased_display;
pub mod error;
pub mod expr;
pub mod hir;
pub mod hir_types;
pub mod interner;
pub mod layout;
pub mod low_effort_interner;
pub mod mir;
pub mod order;
pub mod source;
pub mod types;
use context::Context;
use std::{env::args_os, error::Error, path::Path};

fn main() -> Result<(), Box<dyn Error>> {
    let infile = args_os()
        .nth(1)
        .unwrap_or_else(|| exit_with_message("No filename provided"));
    let mut context = Context::default();
    let path = Path::new(&infile);
    context.fc.add(path).expect("Could not read file");
    context.update_db();
    if context.print_diagnostics() {
        exit_with_message("Errors occured during compilation");
    }
    mir::print_all_mir(&context.db, &mut std::io::stdout().lock())?;
    Ok(())
}

fn exit_with_message(msg: &str) -> ! {
    eprintln!("{}", msg);
    std::process::exit(1);
}
