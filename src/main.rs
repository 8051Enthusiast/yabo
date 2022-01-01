pub mod ast;
pub mod context;
pub mod expr;
pub mod hir;
pub mod interner;
pub mod parse;
pub mod source;
pub mod types;
use ast::Asts;
use context::Context;
use hir::Hirs;
use std::{env::args_os, error::Error, path::Path};

fn main() -> Result<(), Box<dyn Error>> {
    let infile = args_os()
        .nth(1)
        .unwrap_or_else(|| exit_with_message("No filename provided"));
    let mut context = Context::default();
    let path = Path::new(&infile);
    let fid = context.fc.add(path).expect("Could not read file");
    context.update_db();
    let _ast = match context.db.ast(fid) {
        Ok(s) => s,
        Err(e) => {
            let handler = miette::GraphicalReportHandler::new();
            let mut s = String::new();
            for error in e {
                handler.render_report(&mut s, &error)?;
            }
            eprintln!("{}", s);
            return Ok(());
        }
    };
    let graph = hir::represent::HirGraph(&context.db as &dyn Hirs);
    dot::render(&graph, &mut std::io::stdout()).expect("Could not render graph");
    Ok(())
}

fn exit_with_message(msg: &str) -> ! {
    eprintln!("{}", msg);
    std::process::exit(1);
}
