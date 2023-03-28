#![allow(dead_code)]

use std::env;

mod err;
mod expr;
mod parse;
mod peektwo;
mod rlox;
mod scan;
mod tok;

fn main() {
  let args = env::args().skip(1).collect::<Vec<_>>();
  if let Err(err) = rlox::run(args) {
    err.print();
    err.exit();
  }
}
