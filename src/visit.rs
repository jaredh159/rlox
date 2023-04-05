use crate::{expr::*, stmt::Stmt};

pub trait ExprVisitor {
  type Result;
  fn visit_binary(&mut self, binary: &mut Binary) -> Self::Result;
  fn visit_grouping(&mut self, grouping: &mut Grouping) -> Self::Result;
  fn visit_literal(&mut self, literal: &mut Literal) -> Self::Result;
  fn visit_unary(&mut self, unary: &mut Unary) -> Self::Result;
}

pub trait StmtVisitor {
  type Result;
  fn visit_expression(&mut self, expr: &mut Expr) -> Self::Result;
  fn visit_print(&mut self, expr: &mut Expr) -> Self::Result;
}

pub trait ExprVisitable {
  fn accept<V: ExprVisitor>(&mut self, visitor: &mut V) -> V::Result;
}

pub trait StmtVisitable {
  fn accept<V: StmtVisitor>(&mut self, visitor: &mut V) -> V::Result;
}

impl ExprVisitable for Expr {
  fn accept<V: ExprVisitor>(&mut self, visitor: &mut V) -> V::Result {
    match self {
      Expr::Binary(binary) => visitor.visit_binary(binary),
      Expr::Unary(unary) => visitor.visit_unary(unary),
      Expr::Literal(literal) => visitor.visit_literal(literal),
      Expr::Grouping(grouping) => visitor.visit_grouping(grouping),
    }
  }
}

impl StmtVisitable for Stmt {
  fn accept<V: StmtVisitor>(&mut self, visitor: &mut V) -> V::Result {
    match self {
      Stmt::Expression(expr) => visitor.visit_expression(expr),
      Stmt::Print(expr) => visitor.visit_print(expr),
    }
  }
}
