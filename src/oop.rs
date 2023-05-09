use crate::err::{LoxErr, Result};
use crate::eval::Interpreter;
use crate::obj::Callable;
use crate::obj::Obj;
use crate::tok::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
  pub name: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Instance {
  class: Class,
  fields: HashMap<String, Obj>,
}

impl Instance {
  pub fn get(&self, name: &Token) -> Result<Obj> {
    self.fields.get(name.lexeme()).map_or(
      Err(LoxErr::Runtime {
        line: name.line(),
        message: format!("undefined property `{}`", name.lexeme()),
      }),
      |obj| Ok(obj.clone()),
    )
  }

  pub fn set(&mut self, name: &Token, value: Obj) {
    self.fields.insert(name.lexeme().to_string(), value);
  }
}

impl Callable for Class {
  fn call(&mut self, _: &mut Interpreter, _: Vec<Obj>) -> Result<Obj> {
    Ok(Obj::Instance(Rc::new(RefCell::new(Instance {
      class: self.clone(),
      fields: HashMap::new(),
    }))))
  }

  fn arity(&self) -> usize {
    0
  }
}

impl Display for Class {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "<Class: {}>", self.name.lexeme())
  }
}

impl Display for Instance {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} instance", self.class.name.lexeme())
  }
}
