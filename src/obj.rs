use crate::err::Result;
use colored::*;
use std::fmt::Debug;
use std::fmt::Display;
use std::time;

#[derive(Debug, PartialEq, Clone)]
pub enum Obj {
  Nil,
  Bool(bool),
  Num(f64),
  Str(String),
  NativeFunc(NativeFunc),
}

#[derive(Debug, PartialEq, Clone)]
pub enum NativeFunc {
  Clock,
}

impl Display for NativeFunc {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      NativeFunc::Clock => write!(f, "<native fun: clock>"),
    }
  }
}

impl Callable for NativeFunc {
  fn call(&self, _: Vec<Obj>) -> Result<Obj> {
    match self {
      NativeFunc::Clock => {
        let duration_since_epoch = time::SystemTime::now()
          .duration_since(time::UNIX_EPOCH)
          .unwrap();
        let ms_since_epoch = duration_since_epoch.as_secs() * 1000
          + duration_since_epoch.subsec_nanos() as u64 / 1_000_000;
        Ok(Obj::Num(ms_since_epoch as f64))
      }
    }
  }

  fn arity(&self) -> usize {
    match self {
      NativeFunc::Clock => 0,
    }
  }
}

impl Obj {
  pub fn is_truthy(&self) -> bool {
    match self {
      Obj::Nil => false,
      Obj::Bool(boolean) => *boolean,
      _ => true,
    }
  }

  pub fn callable(&self) -> Option<Box<&dyn Callable>> {
    match self {
      Obj::NativeFunc(func) => Some(Box::new(func)),
      _ => None,
    }
  }

  pub fn print(&self) {
    match self {
      Obj::Nil => println!("{}", "nil".blue()),
      Obj::Bool(boolean) => println!("{}", boolean.to_string().green()),
      Obj::Num(number) => println!("{}", number.to_string().magenta()),
      Obj::Str(string) => println!("\"{}\"", string.cyan()),
      Obj::NativeFunc(func) => println!("{}", format!("{func}").dimmed()),
    }
  }
}

pub trait Callable {
  fn call(&self, args: Vec<Obj>) -> Result<Obj>;
  fn arity(&self) -> usize;
}
