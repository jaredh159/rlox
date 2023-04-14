use crate::{expr::Expr, tok::Token};

#[derive(Debug, PartialEq)]
pub enum Stmt {
  Block(Vec<Stmt>),
  Expression(Expr),
  If(IfStmt),
  Print(Expr),
  Var {
    name: Token,
    initializer: Option<Expr>,
  },
  While(WhileStmt),
}

#[derive(Debug, PartialEq)]
pub struct IfStmt {
  pub condition: Expr,
  pub then_branch: Box<Stmt>,
  pub else_branch: Option<Box<Stmt>>,
}

#[derive(Debug, PartialEq)]
pub struct WhileStmt {
  pub condition: Expr,
  pub body: Box<Stmt>,
}
