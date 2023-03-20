use std::io;
use std::{error::Error, fmt::Display};

#[derive(Debug, PartialEq, Eq)]
pub enum LoxErr {
  Io(String),
  Scan { line: usize, message: String },
}

impl LoxErr {
  pub fn print(&self) {
    eprintln!("{}", self);
  }

  pub fn exit(self) -> ! {
    match self {
      LoxErr::Io(_) => std::process::exit(74),
      LoxErr::Scan { .. } => std::process::exit(65),
    }
  }
}

impl Display for LoxErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LoxErr::Io(msg) => write!(f, "IO Error: {}", msg),
      LoxErr::Scan { line, message } => write!(f, "[line {}] Error: {}", line, message),
    }
  }
}

impl From<io::Error> for LoxErr {
  fn from(io_err: io::Error) -> Self {
    LoxErr::Io(io_err.to_string())
  }
}

impl Error for LoxErr {}
