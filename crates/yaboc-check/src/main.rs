use clap::Parser;
use yaboc_ast::import::Import;
use yaboc_base::{source::AriadneCache, Context};
use yaboc_database::{YabocDatabase, ERROR_FNS};

#[derive(Parser)]
#[clap(author, version, about)]
struct Args {
    #[clap(short = 'j', long)]
    output_json: bool,
    #[clap(short, long)]
    module: Vec<String>,
    infile: String,
}

fn main() {
    let args = Args::parse();
    let mut ctx = Context::<YabocDatabase>::default();
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
    let main = ctx.fc.add(&args.infile).expect("Could not read file");
    ctx.update_db(&[main], &modules);
    let reports = ERROR_FNS
        .iter()
        .flat_map(|f| f(&ctx.db))
        .collect::<Vec<_>>();
    for report in reports {
        report
            .into_ariadne()
            .eprint(AriadneCache::new(&ctx.db))
            .expect("Error while printing errors")
    }
}

fn exit_with_message(msg: &str) -> ! {
    eprintln!("{msg}");
    std::process::exit(1);
}
