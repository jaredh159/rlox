use crate::env::Env;
use crate::err::Result;
use crate::eval::Interpreter;
use crate::stmt::FnStmt;
use crate::tok::Token;
use colored::*;
use std::cell::RefCell;
use std::fmt::Debug;
use std::fmt::Display;
use std::rc::Rc;
use std::time;

#[derive(Debug, PartialEq, Clone)]
pub enum Obj {
  Nil,
  Bool(bool),
  Num(f64),
  Str(String),
  NativeFunc(NativeFunc),
  Func(Func),
  Class(Class),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Func {
  pub decl: FnStmt,
  pub closure: Rc<RefCell<Env>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
  pub name: Token,
}

impl Display for Class {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "<Class: {}>", self.name.lexeme())
  }
}

impl Callable for Func {
  fn call(&mut self, interpreter: &mut Interpreter, args: Vec<Obj>) -> Result<Obj> {
    let mut env = Env::new_enclosing(Rc::clone(&self.closure));
    for (arg, param) in args.into_iter().zip(&self.decl.params) {
      env.define(param.lexeme().to_string(), arg);
    }
    interpreter.interpret_block(&mut self.decl.body, Some(Rc::new(RefCell::new(env))))?;
    Ok(interpreter.return_value.take().unwrap_or(Obj::Nil))
  }

  fn arity(&self) -> usize {
    self.decl.params.len()
  }
}

impl Display for Func {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "<fun: {}>", self.decl.name.lexeme())
  }
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
  fn call(&mut self, _: &mut Interpreter, _: Vec<Obj>) -> Result<Obj> {
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

  pub fn callable(&mut self) -> Option<Box<dyn Callable>> {
    match self {
      Obj::NativeFunc(func) => Some(Box::new(func.clone())),
      Obj::Func(func) => Some(Box::new(func.clone())),
      _ => None,
    }
  }

  pub fn print(&self) {
    match self {
      Obj::Nil => println!("{}", "nil".blue()),
      Obj::Bool(boolean) => println!("{}", boolean.to_string().green()),
      Obj::Num(number) => println!("{}", number.to_string().magenta()),
      Obj::Str(string) => println!("\"{}\"", string.cyan()),
      Obj::NativeFunc(native_fn) => println!("{}", format!("{native_fn}").dimmed()),
      Obj::Func(func) => println!("{}", format!("{func}").dimmed()),
      Obj::Class(class) => println!("{}", format!("{class}").dimmed()),
    }
  }
}

pub trait Callable {
  fn call(&mut self, interpreter: &mut Interpreter, args: Vec<Obj>) -> Result<Obj>;
  fn arity(&self) -> usize;
}
