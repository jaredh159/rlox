use std::io::{self, BufRead, Write};
use std::{error::Error, fs};

pub fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
  let source = fs::read_to_string(path)?;
  run(source);
  Ok(())
}

pub fn start_repl(stdin: io::Stdin, mut stdout: io::Stdout) {
  print!("> ");
  stdout.flush().unwrap();
  for line in stdin.lock().lines() {
    let line = line.unwrap();
    println!("your line was: {}", line);
    run(line);
    print!("> ");
    stdout.flush().unwrap();
  }
}

fn run(source: String) {
  let scanner = Scanner::new(source);
  for token in scanner {
    println!("{:?}", token);
  }
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
