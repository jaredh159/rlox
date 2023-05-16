use crate::env::Env;
use crate::err::*;
use crate::expr::*;
use crate::obj::{Func, Obj::*, *};
use crate::oop;
use crate::resolver::Resolvable;
use crate::stmt::{Class, FnStmt, IfStmt, Stmt, WhileStmt};
use crate::tok::Token;
use crate::visit::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Interpreter {
  pub env: Rc<RefCell<Env>>,
  pub globals: Rc<RefCell<Env>>,
  pub return_value: Option<Obj>,
  last_result: Option<Result<Obj>>,
}

impl Interpreter {
  pub fn interpret(&mut self, statements: &mut Vec<Stmt>) -> Result<()> {
    for statement in statements {
      if self.return_value != None {
        return Ok(());
      }
      statement.accept(self)?;
    }
    Ok(())
  }

  pub fn interpret_block(
    &mut self,
    statements: &mut Vec<Stmt>,
    env: Option<Rc<RefCell<Env>>>,
  ) -> Result<()> {
    let mut block = self.scope(env);
    let result = block.interpret(statements);
    if let Some(return_value) = block.return_value.take() {
      self.return_value = Some(return_value);
    }
    if cfg!(test) {
      self.last_result = block.last_result;
    }
    result
  }

  pub fn new() -> Self {
    let globals = Rc::new(RefCell::new(Env::new()));
    return Interpreter::new_with_env(Rc::clone(&globals), globals);
  }

  pub fn new_with_env(env: Rc<RefCell<Env>>, globals: Rc<RefCell<Env>>) -> Self {
    Interpreter {
      env,
      globals,
      return_value: None,
      last_result: None,
    }
  }

  fn lookup_variable(&self, variable: &mut impl Resolvable) -> Result<Obj> {
    variable
      .get_distance()
      .map_or(self.globals.borrow().get(variable.name()), |distance| {
        self.env.borrow().get_at(distance, variable.name())
      })
  }

  fn evaluate(&mut self, expr: &mut Expr) -> Result<Obj> {
    let result = expr.accept(self);
    if cfg!(test) {
      self.last_result = Some(result.clone());
    }
    result
  }

  fn scope(&mut self, env: Option<Rc<RefCell<Env>>>) -> Self {
    Interpreter::new_with_env(
      env.unwrap_or(Rc::new(RefCell::new(Env::new_enclosing(Rc::clone(
        &self.env,
      ))))),
      Rc::clone(&self.globals),
    )
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
    self
      .env
      .borrow_mut()
      .define(name.lexeme().to_string(), value);
    Ok(())
  }

  fn visit_block(&mut self, stmts: &mut Vec<Stmt>) -> Self::Result {
    self.interpret_block(stmts, None)
  }

  fn visit_if(&mut self, if_stmt: &mut IfStmt) -> Self::Result {
    if if_stmt.condition.accept(self)?.is_truthy() {
      if_stmt.then_branch.accept(self)
    } else if let Some(mut else_branch) = if_stmt.else_branch.take() {
      else_branch.accept(self)
    } else {
      Ok(())
    }
  }

  fn visit_while(&mut self, while_stmt: &mut WhileStmt) -> Self::Result {
    while self.return_value == None && self.evaluate(&mut while_stmt.condition)?.is_truthy() {
      while_stmt.body.accept(self)?;
    }
    Ok(())
  }

  fn visit_fn(&mut self, fn_stmt: &mut FnStmt) -> Self::Result {
    self.env.borrow_mut().define(
      fn_stmt.name.lexeme().to_string(),
      Obj::Func(Func {
        decl: fn_stmt.clone(),
        closure: Rc::clone(&self.env),
      }),
    );
    Ok(())
  }

  fn visit_return(&mut self, _keyword: &Token, value: Option<&mut Expr>) -> Self::Result {
    let value = value.map_or(Ok(Obj::Nil), |expr| self.evaluate(expr))?;
    self.return_value = Some(value);
    Ok(())
  }

  fn visit_class(&mut self, class_node: &mut Class) -> Self::Result {
    let name = &class_node.name;
    self
      .env
      .borrow_mut()
      .define(name.lexeme().to_string(), Obj::Nil);

    let mut methods = HashMap::new();
    for method in &class_node.methods {
      methods.insert(
        method.name.lexeme().to_string(),
        Func {
          decl: method.clone(),
          closure: Rc::clone(&self.env),
        },
      );
    }

    let runtime_class = oop::Class {
      name: name.clone(),
      methods,
    };

    self
      .env
      .borrow_mut()
      .assign(name, Obj::Class(runtime_class))?;
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
    assign.distance.map_or(
      self
        .globals
        .borrow_mut()
        .assign(assign.name(), value.clone()),
      |distance| {
        self
          .env
          .borrow_mut()
          .assign_at(distance, assign.name(), value.clone())
      },
    )?;
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

  fn visit_variable(&mut self, variable: &mut Variable) -> Self::Result {
    self.lookup_variable(variable)
  }

  fn visit_logical(&mut self, logical: &mut Logical) -> Self::Result {
    let left = self.evaluate(&mut logical.left)?;
    match (logical.operator, left.is_truthy()) {
      (LogicalOp::Or(_), true) => Ok(left),
      (LogicalOp::And(_), false) => Ok(left),
      _ => self.evaluate(&mut logical.right),
    }
  }

  fn visit_call(&mut self, call: &mut Call) -> Self::Result {
    let mut callee = self.evaluate(&mut *call.callee)?;
    let mut args = Vec::new();
    for mut arg in &mut call.args {
      args.push(self.evaluate(&mut arg)?);
    }
    match callee.callable() {
      None => Err(runtime(
        &call.paren.line(),
        "can only call functions and classes",
      )),
      Some(mut callable) => {
        if args.len() != callable.arity() {
          Err(runtime(
            &call.paren.line(),
            format!(
              "expected {} arguments but got {}",
              callable.arity(),
              args.len()
            ),
          ))
        } else {
          callable.call(self, args)
        }
      }
    }
  }

  fn visit_get(&mut self, get: &mut Get) -> Self::Result {
    let object = self.evaluate(&mut get.object)?;
    if let Obj::Instance(instance) = object {
      oop::Instance::get(&get.name, instance)
    } else {
      Err(LoxErr::Runtime {
        line: get.name.line(),
        message: "only instances have properties".to_string(),
      })
    }
  }

  fn visit_set(&mut self, set: &mut Set) -> Self::Result {
    let object = self.evaluate(&mut set.object)?;
    if let Obj::Instance(instance) = object {
      let value = self.evaluate(&mut set.value)?;
      instance.borrow_mut().set(&set.name, value.clone());
      Ok(value)
    } else {
      Err(LoxErr::Runtime {
        line: set.name.line(),
        message: "only instances have fields".to_string(),
      })
    }
  }

  fn visit_this(&mut self, this: &mut This) -> Self::Result {
    self.lookup_variable(this)
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
  use crate::{parse::Parser, resolver::resolve};

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
      ("\"hi\" or 2", Obj::Str("hi".to_string())),
      ("nil or \"yes\"", Obj::Str("yes".to_string())),
      ("nil or false", Obj::Bool(false)),
      ("true and true", Obj::Bool(true)),
      ("true and false", Obj::Bool(false)),
      ("false and true", Obj::Bool(false)),
    ];
    for (input, expected) in cases {
      assert_eq!(eval(input).unwrap(), expected);
    }
  }

  #[test]
  fn test_bound_methods() {
    let input = r#"
      class Person {
        getName() {
          return this.name;
        }
      }
      var jane = Person();
      jane.name = "Jane";
      var bill = Person();
      bill.name = "Bill";
      bill.getName = jane.getName;
      bill.getName(); // <-- should be "Jane"
      "#;
    assert_eq!(interpret(input).unwrap(), Obj::Str("Jane".to_string()));
  }

  #[test]
  fn test_weird_scope_issue() {
    let input = r#"
      var a = 7;
      {
        fun returnA() {
          return a;
        }

        var b = returnA();
        var a = 0;
        var c = returnA();
        b + c; // should be 14
      }
      "#;
    assert_eq!(interpret(input).unwrap(), Obj::Num(14.0));
  }

  #[test]
  fn test_return_unwind_and_closures() {
    let cases = vec![
      (
        r"
      fun makeCounter() {
        var i = 0;
        fun count() {
          i = i + 1;
          return i;
        }

        return count;
      }

      var counter = makeCounter();
      counter(); // 1
      counter(); // 2
      ",
        Obj::Num(2.0),
      ),
      (
        r"
      fun fib(n) {
        if (n <= 1) return n;
        return fib(n - 2) + fib(n - 1);
      }

      fib(15);
      ",
        Obj::Num(610.0),
      ),
      (
        r"
      fun foobar(n) {
        if (n < 6) {
          return 2;
        } else {
          return 3;
        }
      }

      foobar(1) + foobar(10);
      ",
        Obj::Num(5.0),
      ),
      (
        r"
      fun foobar(n) {
        return 7;
        if (n < 6) {
          return 2;
        }
      }

      foobar(1);
      ",
        Obj::Num(7.0),
      ),
      (
        r"
      fun foobar(n) {
        {
          {
            {
              return n + 5;
            }
          }
        }
      }

      foobar(1);
      ",
        Obj::Num(6.0),
      ),
      (
        r"
      fun count(n) {
        while (n < 100) {
          if (n == 3) return n;
          n = n + 1;
        }
      }

      count(1);
      ",
        Obj::Num(3.0),
      ),
    ];
    for (input, expected) in cases {
      assert_eq!(interpret(input).unwrap(), expected);
    }
  }

  #[test]
  fn test_interpret() {
    let cases = vec![
      ("var x = 2; x + 2;", Obj::Num(4.0)),
      ("var x = 2; x = 3; x + 2;", Obj::Num(5.0)),
      ("var a = 1; var b = 2; a + b;", Obj::Num(3.0)),
      ("var a = 1; { var b = 3; a = b * 2; } a + 1;", Obj::Num(7.0)),
      ("var a = 1; { var a = 3; } a + 1;", Obj::Num(2.0)),
      (
        "var a = 1; while (a < 10) { a = a + 1; } a;",
        Obj::Num(10.0),
      ),
      ("fun add(x) { return x + 1; } add(3);", Obj::Num(4.0)),
      ("fun six() { return 6; } six();", Obj::Num(6.0)),
      (
        "fun sum(x, y, z) { return x + y + z; } sum(1, 2, 3);",
        Obj::Num(6.0),
      ),
      ("class Foo {} var x = Foo(); x.y = 1; x.y;", Obj::Num(1.0)),
      (
        "class Foo { one() { return 1; } } var x = Foo(); x.one();",
        Obj::Num(1.0),
      ),
      (
        "class Foo { incrX() { return this.x + 1; } } var x = Foo(); x.x = 2; x.incrX();",
        Obj::Num(3.0),
      ),
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
      (
        "{ var a = a; }",
        Err(LoxErr::Resolve {
          line: 1,
          message: "can't read local variable `a` in it's own initializer".to_string(),
        }),
      ),
      (
        "{ var a = 3; var a = 4; }",
        Err(LoxErr::Resolve {
          line: 1,
          message: "already a variable with the name `a` in this scope".to_string(),
        }),
      ),
      (
        "return 3;",
        Err(LoxErr::Resolve {
          line: 1,
          message: "can't return from top-level code".to_string(),
        }),
      ),
      (
        "var x = 3; x.foo;",
        Err(LoxErr::Runtime {
          line: 1,
          message: "only instances have properties".to_string(),
        }),
      ),
      (
        "var x = 3; x.foo = 3;",
        Err(LoxErr::Runtime {
          line: 1,
          message: "only instances have fields".to_string(),
        }),
      ),
      (
        "class Foo {} var x = Foo(); x.bar;",
        Err(LoxErr::Runtime {
          line: 1,
          message: "undefined property `bar`".to_string(),
        }),
      ),
      (
        "print this;",
        Err(LoxErr::Resolve {
          line: 1,
          message: "can't use `this` outside of a class".to_string(),
        }),
      ),
      (
        "fun notAMethod() { return this; }",
        Err(LoxErr::Resolve {
          line: 1,
          message: "can't use `this` outside of a class".to_string(),
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
    resolve(&mut stmts)?;
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
    resolve(&mut stmts)?;
    interpreter.interpret(&mut stmts)?;
    interpreter.last_result.unwrap()
  }
}
