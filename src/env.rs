use crate::err::{LoxErr, Result};
use crate::obj::Obj;
use crate::tok::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Env {
  pub enclosing: Option<Rc<RefCell<Env>>>,
  pub values: HashMap<String, Obj>,
}

impl Env {
  pub fn define(&mut self, name: String, value: Obj) {
    self.values.insert(name, value);
  }

  pub fn get(&mut self, name: &Token) -> Result<Obj> {
    match self.values.get(name.lexeme()) {
      Some(obj) => Ok(obj.clone()),
      None => {
        if let Some(enclosing) = &self.enclosing {
          enclosing.borrow_mut().get(name)
        } else {
          Err(LoxErr::Runtime {
            line: name.line(),
            message: format!("undefined variable `{}`", name.lexeme()),
          })
        }
      }
    }
  }

  pub fn assign(&mut self, name: &Token, value: Obj) -> Result<()> {
    if self.values.contains_key(name.lexeme()) {
      self.values.insert(name.lexeme().to_string(), value);
      Ok(())
    } else if let Some(enclosing) = &self.enclosing {
      enclosing.borrow_mut().assign(name, value)
    } else {
      Err(LoxErr::Runtime {
        line: name.line(),
        message: format!("undefined variable `{}`", name.lexeme()),
      })
    }
  }

  pub fn new() -> Self {
    Env {
      enclosing: None,
      values: HashMap::new(),
    }
  }

  pub fn new_enclosing(env: Rc<RefCell<Env>>) -> Self {
    Env {
      enclosing: Some(env),
      values: HashMap::new(),
    }
  }
}
