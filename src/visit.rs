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
  fn visit_get(&mut self, get: &mut Get) -> Self::Result;
  fn visit_set(&mut self, set: &mut Set) -> Self::Result;
  fn visit_super(&mut self, super_expr: &mut Super) -> Self::Result;
  fn visit_this(&mut self, this: &mut This) -> Self::Result;
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
  fn visit_class(&mut self, class: &mut Class) -> Self::Result;
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
      Self::Binary(binary) => visitor.visit_binary(binary),
      Self::Unary(unary) => visitor.visit_unary(unary),
      Self::Literal(literal) => visitor.visit_literal(literal),
      Self::Grouping(grouping) => visitor.visit_grouping(grouping),
      Self::Variable(token) => visitor.visit_variable(token),
      Self::Assign(assign) => visitor.visit_assign(assign),
      Self::Logical(logical) => visitor.visit_logical(logical),
      Self::Call(call) => visitor.visit_call(call),
      Self::Get(get) => visitor.visit_get(get),
      Self::Set(set) => visitor.visit_set(set),
      Self::This(this) => visitor.visit_this(this),
      Self::Super(super_expr) => visitor.visit_super(super_expr),
    }
  }
}

impl StmtVisitable for Stmt {
  fn accept<V: StmtVisitor>(&mut self, visitor: &mut V) -> V::Result {
    match self {
      Self::Expression(expr) => visitor.visit_expression(expr),
      Self::Print(expr) => visitor.visit_print(expr),
      Self::Var { name, initializer } => visitor.visit_var(name, initializer.as_mut()),
      Self::Block(stmts) => visitor.visit_block(stmts),
      Self::If(if_stmt) => visitor.visit_if(if_stmt),
      Self::While(while_stmt) => visitor.visit_while(while_stmt),
      Self::Function(fn_stmt) => visitor.visit_fn(fn_stmt),
      Self::Return { keyword, value } => visitor.visit_return(keyword, value.as_mut()),
      Self::Class(class) => visitor.visit_class(class),
    }
  }
}
