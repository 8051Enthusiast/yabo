pub mod context;
use clap::Parser;
use context::Driver;
use yaboc_ast::import::Import;
use yaboc_base::config::Config;
use yaboc_target::HOST_TARGET_TRIPLE;

use std::{ffi::OsString, path::PathBuf};

#[derive(clap::ValueEnum, Clone, Copy)]
enum EmitKind {
    Hir,
    Deps,
    Lens,
    Backtrack,
    Mir,
    MirGraph,
    Llvm,
    Object,
    SharedLib,
}

#[derive(Clone, PartialEq, Eq, clap::ValueEnum)]
pub enum Sanitizer {
    Address,
    Memory,
}

#[derive(Parser)]
#[clap(author, version, about)]
struct Args {
    #[clap(short = 'j', long)]
    /// Output diagnostics as json
    output_json: bool,
    #[clap(short, long, value_enum, default_value = "shared-lib")]
    /// The kind of output to emit
    emit: EmitKind,
    #[clap(long)]
    mir_emit_path: Option<String>,
    #[clap(long)]
    /// The target triplet to compile for
    target: Option<String>,
    #[clap(long)]
    /// llvm target cpu
    target_cpu: Option<String>,
    #[clap(long)]
    /// llvm target features
    target_features: Option<String>,
    #[clap(long)]
    /// sanitize (memory or address), mostly for debugging the compiler itself
    sanitize: Option<Sanitizer>,
    #[clap(long)]
    /// The C compiler to use as linker
    cc: Option<String>,
    #[clap(long)]
    /// Set the dynamic linker (PT_INTERP) for dynamic libraries
    dynamic_linker: Option<String>,
    #[clap(long)]
    /// The sysroot to use (for WASI)
    sysroot: Option<PathBuf>,
    #[clap(short, long)]
    /// List of module paths, formatted as comma-separated list of `name=path`
    module: Vec<String>,
    /// The source file
    infile: String,
    /// The output file
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
    let Some(target_triple) = args
        .target
        .or_else(|| HOST_TARGET_TRIPLE.map(|x| x.to_string()))
    else {
        exit_with_message("unsupported host triple and no --target given")
    };
    let mut context = match Driver::new(Config {
        target_triple,
        output_json: args.output_json,
        target_cpu: args.target_cpu,
        target_features: args.target_features,
        sysroot: args.sysroot,
        cc: args.cc,
        asan: args.sanitize == Some(Sanitizer::Address),
        msan: args.sanitize == Some(Sanitizer::Memory),
        dynamic_linker: args.dynamic_linker,
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
        EmitKind::Backtrack => context.write_backtrack(outfile).map_err(|x| x.to_string()),
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
