use clap::ArgGroup;
use clap::Parser;

mod compile_target;
mod frontend;

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[clap(group(
            ArgGroup::new("vers")
                .args(&["lex", "parse"]),
        ))]
struct Args {
    files: Vec<std::path::PathBuf>,
    // mode
    #[clap(long, short, action)]
    lex: bool,
    #[clap(long, short, action)]
    parse: bool,
}

#[derive(Debug)]
enum Mode {
    Lex,
    Parse,
    Compile,
}
impl Mode {
    fn from_args(args: &Args) -> Self {
        if args.lex {
            Mode::Lex
        } else if args.parse {
            Mode::Parse
        } else {
            Mode::Compile
        }
    }
}

fn main() {
    let args = Args::parse();
    let mode = Mode::from_args(&args);
    println!("files: {:?}, mode: {:?}", args.files, mode);
}
