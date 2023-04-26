use crate::err::{LoxErr, Result};
use crate::obj::{NativeFunc, Obj};
use crate::tok::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

fn resolve_global(name: &str) -> Option<Obj> {
  match name {
    "clock" => Some(Obj::NativeFunc(NativeFunc::Clock)),
    _ => None,
  }
}

#[derive(Debug)]
pub struct Env {
  pub enclosing: Option<Rc<RefCell<Env>>>,
  pub values: HashMap<String, Obj>,
}

impl PartialEq for Env {
  fn eq(&self, other: &Self) -> bool {
    std::ptr::eq(self, other)
  }
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
        } else if let Some(native_func) = resolve_global(name.lexeme()) {
          Ok(native_func.clone())
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
