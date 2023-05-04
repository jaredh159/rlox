use colored::Colorize;
use std::io;
use std::{error::Error, fmt::Display};

pub type Result<T> = std::result::Result<T, LoxErr>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LoxErr {
  Io(String),
  Scan { line: usize, message: String },
  Parse { line: usize, message: String },
  Runtime { line: usize, message: String },
  Resolve { line: usize, message: String },
  Many(Vec<Box<LoxErr>>),
}

impl LoxErr {
  pub fn print(&self) {
    eprintln!("{}", self.to_string().red());
  }

  pub fn exit(&self) -> ! {
    match self {
      LoxErr::Io(_) => std::process::exit(74),
      LoxErr::Scan { .. } => std::process::exit(65),
      LoxErr::Parse { .. } => std::process::exit(65),
      LoxErr::Runtime { .. } => std::process::exit(70),
      LoxErr::Resolve { .. } => std::process::exit(70),
      LoxErr::Many(errs) => errs
        .first()
        .map_or_else(|| std::process::exit(1), |err| err.exit()),
    }
  }
}

impl Display for LoxErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LoxErr::Io(msg) => write!(f, "IO Error: {}", msg),
      LoxErr::Scan { line, message } => write!(f, "Scan Error: [line {}] {}", line, message),
      LoxErr::Parse { line, message } => write!(f, "Parse Error: [line {}] {}", line, message),
      LoxErr::Runtime { line, message } => write!(f, "Runtime Error: [line {}] {}", line, message),
      LoxErr::Resolve { line, message } => write!(f, "Resolve Error: [line {}] {}", line, message),
      LoxErr::Many(errs) => {
        for err in errs {
          err.fmt(f)?;
        }
        Ok(())
      }
    }
  }
}

impl From<io::Error> for LoxErr {
  fn from(io_err: io::Error) -> Self {
    LoxErr::Io(io_err.to_string())
  }
}

impl Error for LoxErr {}
