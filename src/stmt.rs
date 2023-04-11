use crate::{expr::Expr, tok::Token};

#[derive(Debug, PartialEq)]
pub enum Stmt {
  Block(Vec<Stmt>),
  Expression(Expr),
  Print(Expr),
  Var {
    name: Token,
    initializer: Option<Expr>,
  },
}
