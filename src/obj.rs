use crate::env::Env;
use crate::err::Result;
use crate::eval::Interpreter;
use crate::oop::{Class, Instance};
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
  Instance(Rc<RefCell<Instance>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Func {
  pub decl: FnStmt,
  pub closure: Rc<RefCell<Env>>,
  pub is_initializer: bool,
}

impl Callable for Func {
  fn call(&mut self, interpreter: &mut Interpreter, args: Vec<Obj>) -> Result<Obj> {
    let mut env = Env::new_enclosing(Rc::clone(&self.closure));
    for (arg, param) in args.into_iter().zip(&self.decl.params) {
      env.define(param.lexeme().to_string(), arg);
    }
    interpreter.interpret_block(&mut self.decl.body, Some(Rc::new(RefCell::new(env))))?;
    let return_value = interpreter.return_value.take();
    if self.is_initializer {
      let decl_line = self.decl.name.line();
      Ok(self.closure.borrow().get_at(0, &Token::This(decl_line))?)
    } else {
      Ok(return_value.unwrap_or(Obj::Nil))
    }
  }

  fn arity(&self) -> usize {
    self.decl.params.len()
  }
}

impl Func {
  pub fn bind(&self, instance: Rc<RefCell<Instance>>) -> Self {
    let mut env = Env::new_enclosing(Rc::clone(&self.closure));
    env.define("this".to_string(), Obj::Instance(instance));
    Self {
      decl: self.decl.clone(),
      closure: Rc::new(RefCell::new(env)),
      is_initializer: self.is_initializer,
    }
  }
}

impl Display for Func {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "<fun: {}>", self.decl.name.lexeme())
  }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NativeFunc {
  Clock,
}

impl Display for NativeFunc {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Clock => write!(f, "<native fun: clock>"),
    }
  }
}

impl Callable for NativeFunc {
  fn call(&mut self, _: &mut Interpreter, _: Vec<Obj>) -> Result<Obj> {
    match self {
      Self::Clock => {
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
      Self::Clock => 0,
    }
  }
}

impl Obj {
  pub const fn is_truthy(&self) -> bool {
    match self {
      Self::Nil => false,
      Self::Bool(boolean) => *boolean,
      _ => true,
    }
  }

  pub fn callable(&mut self) -> Option<Box<dyn Callable>> {
    match self {
      Self::NativeFunc(func) => Some(Box::new(func.clone())),
      Self::Func(func) => Some(Box::new(func.clone())),
      Self::Class(class) => Some(Box::new(class.clone())),
      _ => None,
    }
  }

  pub fn print(&self) {
    match self {
      Self::Nil => println!("{}", "nil".blue()),
      Self::Bool(boolean) => println!("{}", boolean.to_string().green()),
      Self::Num(number) => println!("{}", number.to_string().magenta()),
      Self::Str(string) => println!("\"{}\"", string.cyan()),
      Self::NativeFunc(native_fn) => println!("{}", format!("{native_fn}").dimmed()),
      Self::Func(func) => println!("{}", format!("{func}").dimmed()),
      Self::Class(class) => println!("{}", format!("{class}").dimmed()),
      Self::Instance(instance) => {
        println!("{}", format!("{}", instance.as_ref().borrow()).dimmed());
      }
    }
  }
}

pub trait Callable {
  fn call(&mut self, interpreter: &mut Interpreter, args: Vec<Obj>) -> Result<Obj>;
  fn arity(&self) -> usize;
}
