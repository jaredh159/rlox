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
  Many(Vec<LoxErr>),
}

impl LoxErr {
  pub fn print(&self) {
    eprintln!("{}", self.to_string().red());
  }

  pub fn exit(&self) -> ! {
    match self {
      Self::Io(_) => std::process::exit(74),
      Self::Scan { .. } | Self::Parse { .. } => std::process::exit(65),
      Self::Runtime { .. } | Self::Resolve { .. } => std::process::exit(70),
      Self::Many(errs) => errs
        .first()
        .map_or_else(|| std::process::exit(1), |err| err.exit()),
    }
  }
}

impl Display for LoxErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Io(msg) => write!(f, "IO Error: {msg}"),
      Self::Scan { line, message } => {
        write!(f, "Scan Error: [line {line}] {message}")
      }
      Self::Parse { line, message } => {
        write!(f, "Parse Error: [line {line}] {message}")
      }
      Self::Runtime { line, message } => {
        write!(f, "Runtime Error: [line {line}] {message}")
      }
      Self::Resolve { line, message } => {
        write!(f, "Resolve Error: [line {line}] {message}")
      }
      Self::Many(errs) => {
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
    Self::Io(io_err.to_string())
  }
}

impl Error for LoxErr {}
