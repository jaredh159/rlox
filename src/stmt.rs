use crate::{expr::Expr, tok::Token};

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
  Block(Vec<Stmt>),
  Expression(Expr),
  If(IfStmt),
  Function(FnStmt),
  Print(Expr),
  Var {
    name: Token,
    initializer: Option<Expr>,
  },
  Return {
    keyword: Token,
    value: Option<Expr>,
  },
  While(WhileStmt),
  Class(Class),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
  pub name: Token,
  pub methods: Vec<FnStmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStmt {
  pub condition: Expr,
  pub then_branch: Box<Stmt>,
  pub else_branch: Option<Box<Stmt>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStmt {
  pub condition: Expr,
  pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnStmt {
  pub name: Token,
  pub params: Vec<Token>,
  pub body: Vec<Stmt>,
}
