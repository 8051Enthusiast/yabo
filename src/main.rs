pub mod absint;
pub mod ast;
pub mod cg_llvm;
pub mod config;
pub mod context;
pub mod databased_display;
pub mod error;
pub mod expr;
pub mod hash;
pub mod hir;
pub mod hir_types;
pub mod interner;
pub mod layout;
pub mod low_effort_interner;
pub mod mir;
pub mod order;
pub mod source;
pub mod types;
use bumpalo::Bump;
use config::Config;
use context::Context;
use hir_types::TyHirs;
use layout::{LayoutContext, InternerLayout, instantiate};
use low_effort_interner::Interner;
use std::{env::args, error::Error};

fn main() -> Result<(), Box<dyn Error>> {
    let infile = args()
        .nth(1)
        .unwrap_or_else(|| exit_with_message("No filename provided"));
    let mut context = Context::default();
    context.fc.add(&infile).expect("Could not read file");
    context.update_db();
    context.set_config(Config {
        target_triple: String::from("x86_64-pc-linux-gnu"),
        // if x86-64 was so great,,,
        target_cpu: String::from("x86-64-v2"),
        target_features: String::from(""),
    });
    if context.print_diagnostics() {
        exit_with_message("Errors occured during compilation");
    }
    //mir::print_all_mir(&context.db, &mut std::io::stdout().lock())?;
    let llvm = inkwell::context::Context::create();
    let mut bump = Bump::new();
    let intern = Interner::<InternerLayout>::new(&mut bump);
    let layout_ctx = LayoutContext::new(intern);
    let main = context.parser("main");
    let main_ty = context.db.parser_args(main).unwrap().thunk;
    let mut layouts = layout::AbsLayoutCtx::new(&context.db, layout_ctx);
    instantiate(&mut layouts, &[main_ty]).unwrap();
    let mut codegen = cg_llvm::CodeGenCtx::new(&llvm, &context, &mut layouts).unwrap();
    codegen.create_all_vtables();
    let object_file = codegen.object_file();
    eprintln!("{:?}", object_file);
    Ok(())
}

fn exit_with_message(msg: &str) -> ! {
    eprintln!("{}", msg);
    std::process::exit(1);
}
