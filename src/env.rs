use crate::err::{LoxErr, Result};
use crate::obj::Obj;
use crate::tok::Token;
use std::collections::HashMap;

pub struct Env {
  pub values: HashMap<String, Obj>,
}

impl Env {
  pub fn define(&mut self, name: String, value: Obj) {
    self.values.insert(name, value);
  }

  pub fn get(&mut self, name: &Token) -> Result<Obj> {
    match self.values.get(name.lexeme()) {
      Some(obj) => Ok(obj.clone()),
      None => Err(LoxErr::Runtime {
        line: name.line(),
        message: format!("undefined variable `{}`", name.lexeme()),
      }),
    }
  }

  pub fn assign(&mut self, name: &Token, value: Obj) -> Result<()> {
    if self.values.contains_key(name.lexeme()) {
      self.values.insert(name.lexeme().to_string(), value);
      Ok(())
    } else {
      Err(LoxErr::Runtime {
        line: name.line(),
        message: format!("undefined variable `{}`", name.lexeme()),
      })
    }
  }

  pub fn new() -> Self {
    Env {
      values: HashMap::new(),
    }
  }
}
