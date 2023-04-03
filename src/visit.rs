use crate::expr::*;

pub trait Visitor {
  type Result;
  fn visit_binary(&mut self, binary: &mut Binary) -> Self::Result;
  fn visit_grouping(&mut self, grouping: &mut Grouping) -> Self::Result;
  fn visit_literal(&mut self, literal: &mut Literal) -> Self::Result;
  fn visit_unary(&mut self, unary: &mut Unary) -> Self::Result;
}

pub trait Visitable {
  fn accept<V: Visitor>(&mut self, visitor: &mut V) -> V::Result;
}

impl Visitable for Expr {
  fn accept<V: Visitor>(&mut self, visitor: &mut V) -> V::Result {
    match self {
      Expr::Binary(binary) => visitor.visit_binary(binary),
      Expr::Unary(unary) => visitor.visit_unary(unary),
      Expr::Literal(literal) => visitor.visit_literal(literal),
      Expr::Grouping(grouping) => visitor.visit_grouping(grouping),
    }
  }
}
