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
pub mod resolve;
pub mod source;
pub mod types;

use clap::Parser;
use config::Config;
use context::Context;

use std::ffi::OsString;

#[derive(clap::ValueEnum, Clone, Copy)]
enum EmitKind {
    Hir,
    Mir,
    Llvm,
    Object,
    SharedLib,
}

#[derive(Parser)]
#[clap(author, version, about)]
struct Args {
    #[clap(short, long)]
    output_json: bool,
    #[clap(short, long, arg_enum, default_value = "shared-lib")]
    emit: EmitKind,
    infile: String,
    outfile: OsString,
}

fn main() {
    let args = Args::parse();
    let infile = &args.infile;
    let mut context = Context::default();
    context.fc.add(&infile).expect("Could not read file");
    context.update_db();
    context.set_config(Config {
        target_triple: String::from("x86_64-pc-linux-gnu"),
        // if x86-64 was so great,,,
        target_cpu: String::from("x86-64-v2"),
        target_features: String::from(""),
        output_json: args.output_json,
    });
    if context.print_diagnostics() {
        if !args.output_json {
            exit_with_message("Errors occured during compilation");
        } else {
            std::process::exit(1);
        }
    }
    let outfile = &args.outfile;
    match match args.emit {
        EmitKind::Hir => context.write_hir(outfile).map_err(|x| x.to_string()),
        EmitKind::Mir => context.write_mir(outfile).map_err(|x| x.to_string()),
        EmitKind::Llvm => context.write_llvm(outfile),
        EmitKind::Object => context.write_object(outfile),
        EmitKind::SharedLib => context.write_shared_lib(outfile),
    } {
        Ok(()) => {}
        Err(x) => exit_with_message(&x),
    };
}

fn exit_with_message(msg: &str) -> ! {
    eprintln!("{}", msg);
    std::process::exit(1);
}
