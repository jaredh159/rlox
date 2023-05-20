use crate::err::{LoxErr, Result};
use crate::eval::Interpreter;
use crate::obj::Obj;
use crate::obj::{Callable, Func};
use crate::tok::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
  pub name: Token,
  pub methods: HashMap<String, Func>,
  pub superclass: Option<Box<Class>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Instance {
  class: Class,
  fields: HashMap<String, Obj>,
}

impl Instance {
  pub fn get(name: &Token, instance: Rc<RefCell<Self>>) -> Result<Obj> {
    if let Some(field) = instance.borrow().fields.get(name.lexeme()) {
      Ok(field.clone())
    } else if let Some(method) = instance.borrow().class.methods.get(name.lexeme()) {
      Ok(Obj::Func(method.bind(Rc::clone(&instance))))
    } else {
      Err(LoxErr::Runtime {
        line: name.line(),
        message: format!("undefined property `{}`", name.lexeme()),
      })
    }
  }

  pub fn set(&mut self, name: &Token, value: Obj) {
    self.fields.insert(name.lexeme().to_string(), value);
  }
}

impl Callable for Class {
  fn call(&mut self, interpreter: &mut Interpreter, args: Vec<Obj>) -> Result<Obj> {
    let instance = Rc::new(RefCell::new(Instance {
      class: self.clone(),
      fields: HashMap::new(),
    }));
    if let Some(initializer) = self.methods.get_mut("init") {
      initializer.bind(Rc::clone(&instance));
      initializer.call(interpreter, args)?;
    }
    Ok(Obj::Instance(instance))
  }

  fn arity(&self) -> usize {
    self.methods.get("init").map_or(0, |init| init.arity())
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
