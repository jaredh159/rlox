use crate::err::*;
use crate::expr::*;
use crate::obj::{Obj::*, *};
use crate::stmt::Stmt;
use crate::tok::Token;
use crate::visit::*;

pub struct Interpreter {
  last_result: Option<Result<Obj>>,
}

impl Interpreter {
  pub fn interpret(&mut self, statements: &mut Vec<Stmt>) -> Result<()> {
    for statement in statements {
      statement.accept(self)?;
    }
    Ok(())
  }

  pub fn new() -> Self {
    Interpreter { last_result: None }
  }

  fn evaluate(&mut self, expr: &mut Expr) -> Result<Obj> {
    let result = expr.accept(self);
    if cfg!(test) {
      self.last_result = Some(result.clone());
    }
    result
  }
}

impl StmtVisitor for Interpreter {
  type Result = Result<()>;

  fn visit_expression(&mut self, expr: &mut Expr) -> Self::Result {
    self.evaluate(expr).map(|_| ())
  }

  fn visit_print(&mut self, expr: &mut Expr) -> Self::Result {
    let value = self.evaluate(expr)?;
    value.print();
    Ok(())
  }
}

impl ExprVisitor for Interpreter {
  type Result = Result<Obj>;

  fn visit_binary(&mut self, binary: &mut Binary) -> Self::Result {
    let left = self.evaluate(&mut *binary.left)?;
    let right = self.evaluate(&mut *binary.right)?;
    match (left, &binary.operator, right) {
      (Num(lhs), BinaryOp::Minus(_), Num(rhs)) => Ok(Num(lhs - rhs)),
      (Num(lhs), BinaryOp::Slash(_), Num(rhs)) => Ok(Num(lhs / rhs)),
      (Num(lhs), BinaryOp::Star(_), Num(rhs)) => Ok(Num(lhs * rhs)),
      (Num(lhs), BinaryOp::Plus(_), Num(rhs)) => Ok(Num(lhs + rhs)),
      (Str(lhs), BinaryOp::Plus(_), Str(rhs)) => Ok(Str(lhs + &rhs)),
      (Num(lhs), BinaryOp::Greater(_), Num(rhs)) => Ok(Bool(lhs > rhs)),
      (Num(lhs), BinaryOp::GreaterEqual(_), Num(rhs)) => Ok(Bool(lhs >= rhs)),
      (Num(lhs), BinaryOp::Less(_), Num(rhs)) => Ok(Bool(lhs < rhs)),
      (Num(lhs), BinaryOp::LessEqual(_), Num(rhs)) => Ok(Bool(lhs <= rhs)),
      (lhs, BinaryOp::EqualEqual(_), rhs) => Ok(Bool(lhs == rhs)),
      (lhs, BinaryOp::BangEqual(_), rhs) => Ok(Bool(lhs != rhs)),

      // error cases
      (_, BinaryOp::Minus(line), _)
      | (_, BinaryOp::Slash(line), _)
      | (_, BinaryOp::Greater(line), _)
      | (_, BinaryOp::GreaterEqual(line), _)
      | (_, BinaryOp::Less(line), _)
      | (_, BinaryOp::LessEqual(line), _)
      | (_, BinaryOp::Star(line), _) => Err(runtime(
        line,
        format!(
          "operands for binary `{}` must be numbers",
          binary.operator.lexeme()
        ),
      )),
      (_, BinaryOp::Plus(line), _) => Err(runtime(
        line,
        "operands for binary `+` must be two numbers or two strings",
      )),
      _ => Err(runtime(
        binary.operator.line(),
        format!(
          "illegal usage of binary operator `{}`",
          binary.operator.lexeme()
        ),
      )),
    }
  }

  fn visit_grouping(&mut self, grouping: &mut Grouping) -> Self::Result {
    self.evaluate(&mut *grouping.expr)
  }

  fn visit_literal(&mut self, literal: &mut Literal) -> Self::Result {
    match literal {
      Literal::Nil => Ok(Nil),
      Literal::True => Ok(Bool(true)),
      Literal::False => Ok(Bool(false)),
      Literal::Number(number) => Ok(Num(*number)),
      Literal::String(string) => Ok(Str(string.to_string())),
    }
  }

  fn visit_unary(&mut self, unary: &mut Unary) -> Self::Result {
    let right = self.evaluate(&mut *unary.right)?;
    match (&unary.operator, right) {
      (UnaryOp::Bang(_), rhs) => Ok(Bool(rhs.is_truthy())),
      (UnaryOp::Minus(_), Num(number)) => Ok(Num(-number)),
      (UnaryOp::Minus(line), _) => Err(runtime(line, "operand for unary `-` must be number")),
    }
  }

  fn visit_variable(&mut self, variable: &mut Token) -> Self::Result {
    todo!()
  }
}

fn runtime<S>(line: &usize, message: S) -> LoxErr
where
  S: Into<String>,
{
  LoxErr::Runtime {
    line: *line,
    message: message.into(),
  }
}

// tests

#[cfg(test)]
mod tests {
  use crate::parse::Parser;

  use super::*;

  #[test]
  fn test_eval() {
    let cases = vec![
      ("2 + 2;", Obj::Num(4.0)),
      ("(3 + 5) * 2;", Obj::Num(16.0)),
      ("3 + 5 * 2;", Obj::Num(13.0)),
      ("2 < 2;", Obj::Bool(false)),
      ("2 <= 2;", Obj::Bool(true)),
      ("true == false;", Obj::Bool(false)),
      ("2.5 * 5;", Obj::Num(12.5)),
      ("!nil;", Obj::Bool(false)),
    ];
    for (input, expected) in cases {
      assert_eq!(eval(input), expected);
    }
  }

  fn eval(input: &str) -> Obj {
    let mut parser = Parser::from_str(input);
    let mut interpreter = Interpreter::new();
    let mut stmts = parser.parse().unwrap();
    interpreter.interpret(&mut stmts).unwrap();
    interpreter.last_result.unwrap().unwrap()
  }
}
