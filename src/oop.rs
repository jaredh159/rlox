use crate::err::Result;
use crate::eval::Interpreter;
use crate::obj::Callable;
use crate::obj::Obj;
use crate::tok::Token;
use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
  pub name: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Instance {
  class: Class,
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

impl Callable for Class {
  fn call(&mut self, _: &mut Interpreter, _: Vec<Obj>) -> Result<Obj> {
    Ok(Obj::Instance(Instance {
      class: self.clone(),
    }))
  }

  fn arity(&self) -> usize {
    0
  }
}
