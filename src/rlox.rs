use crate::err::Result;
use crate::parse::Parser;
use std::io::{self, BufRead, Write};

pub fn run(args: Vec<String>) -> Result<()> {
  if args.len() > 1 {
    eprintln!("Usage: rlox [script]");
    std::process::exit(64);
  } else if args.len() == 1 {
    eval_file(&args[0])
  } else {
    start_repl(io::stdin(), io::stdout());
    Ok(())
  }
}

pub fn eval_file(path: &str) -> Result<()> {
  let source = std::fs::read_to_string(path)?;
  eval(source)
}

pub fn start_repl(stdin: io::Stdin, mut stdout: io::Stdout) {
  print!("> ");
  stdout.flush().unwrap();
  for line in stdin.lock().lines() {
    let line = line.unwrap();
    match eval(line) {
      Ok(_) => {}
      Err(err) => err.print(),
    }
    print!("> ");
    stdout.flush().unwrap();
  }
}

fn eval(source: String) -> Result<()> {
  let mut parser = Parser::new(&source);
  println!("{:#?}", parser.parse());
  Ok(())
}
