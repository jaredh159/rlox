use crate::parse::Parser;
use crate::{err::Result, eval::Interpreter};
use colored::Colorize;
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
  eval(source).map(|obj| println!("{:?}", obj))
}

pub fn start_repl(stdin: io::Stdin, mut stdout: io::Stdout) {
  print!("{} ", ">".to_string().bright_cyan());
  stdout.flush().unwrap();
  for line in stdin.lock().lines() {
    let line = line.unwrap();
    _ = eval(line);
    print!("{} ", ">".to_string().bright_cyan());
    stdout.flush().unwrap();
  }
}

fn eval(source: String) -> Result<()> {
  let mut program = Parser::new(&source).parse()?;
  let mut interpreter = Interpreter::new();
  interpreter.interpret(&mut program).map_err(|err| {
    err.print();
    err
  })
}
