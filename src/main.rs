use std::env;

mod err;
mod rlox;

fn main() {
  let args = env::args().skip(1).collect::<Vec<_>>();
  if let Err(err) = rlox::run(args) {
    eprintln!("{}", err);
    err.exit();
  }
}
