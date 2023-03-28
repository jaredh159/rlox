use crate::tok::Token;

#[derive(Debug, PartialEq)]
pub enum Expr {
  Binary(Binary),
  Unary(Unary),
  Literal(Literal),
  Grouping(Grouping),
}

#[derive(Debug, PartialEq)]
pub struct Binary {
  pub left: Box<Expr>,
  pub operator: Token,
  pub right: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Unary {
  pub operator: Token,
  pub right: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
  Nil,
  True,
  False,
  Number(f64), // ¯\_(ツ)_/¯
}

#[derive(Debug, PartialEq)]
pub struct Grouping {
  pub expr: Box<Expr>,
}

pub trait Visitor {
  type Result;
  fn visit_binary(&mut self, binary: &Binary) -> Self::Result;
  fn visit_grouping(&mut self, grouping: &Grouping) -> Self::Result;
  fn visit_literal(&mut self, literal: &Literal) -> Self::Result;
  fn visit_unary(&mut self, unary: &Unary) -> Self::Result;
}

pub trait Visitable {
  fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result;
}

impl Visitable for Expr {
  fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
    match self {
      Expr::Binary(binary) => visitor.visit_binary(binary),
      Expr::Unary(unary) => visitor.visit_unary(unary),
      Expr::Literal(literal) => visitor.visit_literal(literal),
      Expr::Grouping(grouping) => visitor.visit_grouping(grouping),
    }
  }
}
