use crate::stmt::*;
use crate::{expr::*, tok::Token};

pub trait ExprVisitor {
  type Result;
  fn visit_assign(&mut self, assign: &mut Assign) -> Self::Result;
  fn visit_binary(&mut self, binary: &mut Binary) -> Self::Result;
  fn visit_grouping(&mut self, grouping: &mut Grouping) -> Self::Result;
  fn visit_literal(&mut self, literal: &mut Literal) -> Self::Result;
  fn visit_logical(&mut self, logical: &mut Logical) -> Self::Result;
  fn visit_unary(&mut self, unary: &mut Unary) -> Self::Result;
  fn visit_variable(&mut self, variable: &mut Variable) -> Self::Result;
  fn visit_call(&mut self, call: &mut Call) -> Self::Result;
}

pub trait StmtVisitor {
  type Result;
  fn visit_expression(&mut self, expr: &mut Expr) -> Self::Result;
  fn visit_print(&mut self, expr: &mut Expr) -> Self::Result;
  fn visit_var(&mut self, name: &Token, initializer: Option<&mut Expr>) -> Self::Result;
  fn visit_block(&mut self, stmts: &mut Vec<Stmt>) -> Self::Result;
  fn visit_if(&mut self, if_stmt: &mut IfStmt) -> Self::Result;
  fn visit_while(&mut self, while_stmt: &mut WhileStmt) -> Self::Result;
  fn visit_fn(&mut self, fn_stmt: &mut FnStmt) -> Self::Result;
  fn visit_return(&mut self, keyword: &Token, value: Option<&mut Expr>) -> Self::Result;
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
      Expr::Variable(token) => visitor.visit_variable(token),
      Expr::Assign(assign) => visitor.visit_assign(assign),
      Expr::Logical(logical) => visitor.visit_logical(logical),
      Expr::Call(call) => visitor.visit_call(call),
    }
  }
}

impl StmtVisitable for Stmt {
  fn accept<V: StmtVisitor>(&mut self, visitor: &mut V) -> V::Result {
    match self {
      Stmt::Expression(expr) => visitor.visit_expression(expr),
      Stmt::Print(expr) => visitor.visit_print(expr),
      Stmt::Var { name, initializer } => visitor.visit_var(name, initializer.as_mut()),
      Stmt::Block(stmts) => visitor.visit_block(stmts),
      Stmt::If(if_stmt) => visitor.visit_if(if_stmt),
      Stmt::While(while_stmt) => visitor.visit_while(while_stmt),
      Stmt::Function(fn_stmt) => visitor.visit_fn(fn_stmt),
      Stmt::Return { keyword, value } => visitor.visit_return(keyword, value.as_mut()),
    }
  }
}
