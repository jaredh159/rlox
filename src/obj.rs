use colored::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Obj {
  Nil,
  Bool(bool),
  Num(f64),
  Str(String),
}

impl Obj {
  pub fn is_truthy(&self) -> bool {
    match self {
      Obj::Nil => false,
      Obj::Bool(boolean) => *boolean,
      _ => true,
    }
  }

  pub fn print(&self) {
    match self {
      Obj::Nil => println!("{}", "nil".blue()),
      Obj::Bool(boolean) => println!("{}", boolean.to_string().green()),
      Obj::Num(number) => println!("{}", number.to_string().magenta()),
      Obj::Str(string) => println!("\"{}\"", string.cyan()),
    }
  }
}
