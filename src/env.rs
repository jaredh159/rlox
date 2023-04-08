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

  pub fn get(&mut self, token: &Token) -> Result<Obj> {
    match self.values.get(token.lexeme()) {
      Some(obj) => Ok(obj.clone()),
      None => Err(LoxErr::Runtime {
        line: token.line(),
        message: format!("undefined variable `{}`", token.lexeme()),
      }),
    }
  }

  pub fn new() -> Self {
    Env {
      values: HashMap::new(),
    }
  }
}
