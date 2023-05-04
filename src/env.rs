use crate::err::{LoxErr, Result};
use crate::obj::{NativeFunc, Obj};
use crate::tok::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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

  pub fn get_at(&mut self, distance: usize, name: &Token) -> Result<Obj> {
    self.with_ancestor_at(distance, |env| env.get(&name))
  }

  pub fn assign_at(&mut self, distance: usize, name: &Token, value: Obj) -> Result<()> {
    self.with_ancestor_at(distance, |env| env.assign(name, value))
  }

  pub fn get(&mut self, name: &Token) -> Result<Obj> {
    match self.values.get(name.lexeme()) {
      Some(obj) => Ok(obj.clone()),
      None => {
        if let Some(enclosing) = &self.enclosing {
          enclosing.borrow_mut().get(name)
        } else if let Some(native_func) = resolve_native_func(name.lexeme()) {
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

  fn with_ancestor_at<F, T>(&mut self, distance: usize, f: F) -> T
  where
    F: FnOnce(&mut Self) -> T,
  {
    if distance == 0 {
      return f(self);
    }
    let mut enclosing = self
      .enclosing
      .as_ref()
      .expect("expected enclosing environment")
      .borrow_mut();
    return enclosing.with_ancestor_at(distance - 1, f);
  }
}

fn resolve_native_func(name: &str) -> Option<Obj> {
  match name {
    "clock" => Some(Obj::NativeFunc(NativeFunc::Clock)),
    _ => None,
  }
}
