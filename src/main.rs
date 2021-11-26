pub mod ast;
pub mod context;
pub mod interner;
pub mod parse;
pub mod source;
pub mod types;
pub mod hir;
pub mod expr;
use ast::Asts;
use hir::Hirs;
use context::Context;
use std::{env::args_os, error::Error, path::Path};

fn main() -> Result<(), Box<dyn Error>> {
    let infile = args_os()
        .nth(1)
        .unwrap_or_else(|| exit_with_message("No filename provided"));
    let mut context = Context::default();
    let path = Path::new(&infile);
    let fid = context.fc.add(path).expect("Could not read file");
    context.update_db();
    let ast = match context.db.ast(fid) {
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

    let expr1 = context.id("complex");
    let collection = context.db.hir_parser_collection(fid, expr1).unwrap().unwrap();
    let hirctx = hir::HirConversionCtx::new(collection, &context.db);
    dot::render(&hirctx, &mut std::io::stdout());
    Ok(())
}

fn exit_with_message(msg: &str) -> ! {
    eprintln!("{}", msg);
    std::process::exit(1);
}
