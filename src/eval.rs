use crate::env::Env;
use crate::err::*;
use crate::expr::*;
use crate::obj::{Obj::*, *};
use crate::stmt::Stmt;
use crate::tok::Token;
use crate::visit::*;

pub struct Interpreter {
  env: Env,
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
    Interpreter {
      env: Env::new(),
      last_result: None,
    }
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

  fn visit_var(&mut self, name: &Token, initializer: Option<&mut Expr>) -> Self::Result {
    let value = initializer.map_or(Ok(Obj::Nil), |expr| self.evaluate(expr))?;
    self.env.define(name.lexeme().to_string(), value);
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

  fn visit_assign(&mut self, assign: &mut Assign) -> Self::Result {
    let value = self.evaluate(&mut *assign.value)?;
    self.env.assign(&assign.name, value.clone())?;
    Ok(value)
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
    self.env.get(variable)
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
      ("2 + 2", Obj::Num(4.0)),
      ("(3 + 5) * 2", Obj::Num(16.0)),
      ("3 + 5 * 2", Obj::Num(13.0)),
      ("2 < 2", Obj::Bool(false)),
      ("2 <= 2", Obj::Bool(true)),
      ("true == false", Obj::Bool(false)),
      ("2.5 * 5", Obj::Num(12.5)),
      ("!nil", Obj::Bool(false)),
    ];
    for (input, expected) in cases {
      assert_eq!(eval(input).unwrap(), expected);
    }
  }

  #[test]
  fn test_interpret() {
    let cases = vec![
      ("var x = 2; x + 2;", Obj::Num(4.0)),
      ("var x = 2; x = 3; x + 2;", Obj::Num(5.0)),
      ("var a = 1; var b = 2; a + b;", Obj::Num(3.0)),
    ];
    for (input, expected) in cases {
      assert_eq!(interpret(input).unwrap(), expected);
    }
  }

  #[test]
  fn test_interpret_errors() {
    let cases = vec![
      (
        "print 2 + x;",
        Err(LoxErr::Runtime {
          line: 1,
          message: "undefined variable `x`".to_string(),
        }),
      ),
      (
        "x = 2; var x;",
        Err(LoxErr::Runtime {
          line: 1,
          message: "undefined variable `x`".to_string(),
        }),
      ),
    ];
    for (input, expected) in cases {
      assert_eq!(interpret(input), expected);
    }
  }

  fn eval(input: &str) -> Result<Obj> {
    let stmt = input.to_string() + ";";
    let mut parser = Parser::new(&stmt);
    let mut interpreter = Interpreter::new();
    let mut stmts = parser.parse().unwrap();
    assert_eq!(stmts.len(), 1);
    let mut expr = match stmts.pop().unwrap() {
      Stmt::Expression(expr) => expr,
      _ => panic!("expected expression statement"),
    };
    interpreter.evaluate(&mut expr)
  }

  fn interpret(input: &str) -> Result<Obj> {
    let mut parser = Parser::from_str(input);
    let mut interpreter = Interpreter::new();
    let mut stmts = parser.parse().unwrap();
    interpreter.interpret(&mut stmts)?;
    interpreter.last_result.unwrap()
  }
}
