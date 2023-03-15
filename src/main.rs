use std::env;
use std::io;

mod rlox;

fn main() {
  let args = env::args().skip(1).collect::<Vec<_>>();
  if args.len() > 1 {
    eprintln!("Usage: rlox [script]");
    std::process::exit(64);
  } else if args.len() == 1 {
    _ = rlox::run_file(&args[0]);
  } else {
    rlox::start_repl(io::stdin(), io::stdout());
  }
}
