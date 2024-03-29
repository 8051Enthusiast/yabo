pub mod context;
use clap::Parser;
use context::Driver;
use yaboc_ast::import::Import;
use yaboc_base::config::Config;

use std::{ffi::OsString, path::PathBuf};

#[derive(clap::ValueEnum, Clone, Copy)]
enum EmitKind {
    Hir,
    Deps,
    Lens,
    Mir,
    MirGraph,
    Llvm,
    Object,
    SharedLib,
}

#[derive(Parser)]
#[clap(author, version, about)]
struct Args {
    #[clap(short = 'j', long)]
    output_json: bool,
    #[clap(short, long, value_enum, default_value = "shared-lib")]
    emit: EmitKind,
    #[clap(long)]
    mir_emit_path: Option<String>,
    #[clap(long)]
    target: Option<String>,
    #[clap(long)]
    cc: Option<String>,
    #[clap(long)]
    sysroot: Option<PathBuf>,
    #[clap(short, long)]
    module: Vec<String>,
    infile: String,
    outfile: OsString,
}

fn main() {
    let args = Args::parse();
    let infile = &args.infile;
    let modules: Vec<_> = args
        .module
        .iter()
        .map(|x| {
            let a = x.splitn(2, '=').collect::<Vec<_>>();
            match a.as_slice() {
                [a, b] => (*a, *b),
                [a] => {
                    exit_with_message(&format!("Missing '=' in module specifier {a}"));
                }
                _ => unreachable!(),
            }
        })
        .collect();
    let target_triple = args
        .target
        .unwrap_or_else(|| "x86_64-unknown-linux-gnu".to_string());
    let mut context = match Driver::new(Config {
        target_triple,
        output_json: args.output_json,
        target_cpu: None,
        target_features: None,
        sysroot: args.sysroot,
        cc: args.cc,
    }) {
        Ok(ctx) => ctx,
        Err(e) => exit_with_message(&format!("Could not create context: {e}")),
    };
    let main = context.fc.add(infile).expect("Could not read file");
    context.update_db(&[main], &modules);
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
        EmitKind::MirGraph => context
            .write_mir_graphs(main, args.mir_emit_path.as_deref(), outfile)
            .map_err(|x| x.to_string()),
        EmitKind::Deps => context.write_deps(outfile).map_err(|x| x.to_string()),
        EmitKind::Lens => context.write_lens(outfile).map_err(|x| x.to_string()),
        EmitKind::Llvm => context.write_llvm(outfile),
        EmitKind::Object => context.write_object(outfile),
        EmitKind::SharedLib => context.write_shared_lib(outfile),
    } {
        Ok(()) => {}
        Err(x) => exit_with_message(&x),
    };
}

fn exit_with_message(msg: &str) -> ! {
    eprintln!("{msg}");
    std::process::exit(1);
}
