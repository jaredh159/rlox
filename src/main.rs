#![allow(dead_code)]

mod env;
mod err;
mod eval;
mod expr;
mod obj;
mod oop;
mod parse;
mod peektwo;
mod resolver;
mod rlox;
mod scan;
mod stmt;
mod tok;
mod visit;

fn main() {
  let args = std::env::args().skip(1).collect::<Vec<_>>();
  if let Err(err) = rlox::run(&args) {
    err.print();
    err.exit();
  }
}
