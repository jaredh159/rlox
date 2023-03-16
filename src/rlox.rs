use crate::err::LoxErr;
use std::io::{self, BufRead, Write};

pub fn run(args: Vec<String>) -> Result<(), LoxErr> {
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

pub fn eval_file(path: &str) -> Result<(), LoxErr> {
  let source = std::fs::read_to_string(path)?;
  // todo: handle error
  _ = eval(source);
  Ok(())
}

pub fn start_repl(stdin: io::Stdin, mut stdout: io::Stdout) {
  print!("> ");
  stdout.flush().unwrap();
  for line in stdin.lock().lines() {
    let line = line.unwrap();
    println!("your line was: {}", line);
    match eval(line) {
      Ok(_) => {}
      Err(err) => eprintln!("{}", err),
    }
    print!("> ");
    stdout.flush().unwrap();
  }
}

fn eval(source: String) -> Result<(), LoxErr> {
  let scanner = Scanner::new(source);
  for token in scanner {
    println!("{:?}", token);
  }
  Ok(())
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Token {}

#[derive(Debug)]
struct Scanner {
  source: String,
}

impl Scanner {
  fn new(source: String) -> Self {
    Scanner { source }
  }
}

impl Iterator for Scanner {
  type Item = Token;
  fn next(&mut self) -> Option<Token> {
    None
  }
}
