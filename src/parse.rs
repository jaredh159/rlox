use crate::err::{LoxErr, Result};
use crate::expr::*;
use crate::scan::Scanner;
use crate::stmt::{self, Stmt, *};
use crate::tok::{Token, TokenType, TokenType::*};
use std::iter::Peekable;

pub struct Parser<'a> {
  tokens: Peekable<Scanner<'a>>,
}

impl<'a> Parser<'a> {
  pub fn parse(&mut self) -> Result<Vec<Stmt>> {
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    while !self.is_at_end() {
      match self.parse_declaration() {
        Ok(stmt) => statements.push(stmt),
        Err(err) => {
          errors.push(err);
          self.synchronize_after_error();
        }
      }
    }
    if errors.is_empty() {
      Ok(statements)
    } else if errors.len() == 1 {
      Err(errors.pop().unwrap())
    } else {
      Err(LoxErr::Many(
        errors.into_iter().map(Box::new).collect::<Vec<_>>(),
      ))
    }
  }

  fn parse_declaration(&mut self) -> Result<Stmt> {
    if self.consume_discarding(Fun) {
      self.parse_fn_declaration(FnKind::Function)
    } else if self.consume_discarding(Class) {
      self.parse_class_declaration()
    } else if self.consume_discarding(Var) {
      self.parse_variable_declaration()
    } else {
      self.parse_statement()
    }
  }

  fn parse_fn_declaration(&mut self, kind: FnKind) -> Result<Stmt> {
    let name = self.consume_expecting(Identifier, kind.ident_error())?;
    self.consume_expecting(LeftParen, kind.lparen_error())?;
    let mut params = Vec::new();
    if !self.peek_is(&RightParen) {
      loop {
        if params.len() >= 255 {
          return Err(LoxErr::Parse {
            line: self.tokens.peek().unwrap_or(&name).line(),
            message: "can't have more than 255 parameters".to_string(),
          });
        }
        params.push(self.consume_expecting(Identifier, "expected parameter name")?);
        if !self.consume_discarding(Comma) {
          break;
        }
      }
    }
    self.consume_expecting(RightParen, "expected `)` after parameters")?;
    self.consume_expecting(LeftBrace, kind.lbrace_error())?;
    let body = self.parse_block()?;
    Ok(Stmt::Function(FnStmt { name, params, body }))
  }

  fn parse_class_declaration(&mut self) -> Result<Stmt> {
    let name = self.consume_expecting(Identifier, "expected class name")?;
    self.consume_expecting(LeftBrace, "expected `{` before class body")?;
    let mut methods = Vec::new();
    while !self.peek_is(&RightBrace) && !self.is_at_end() {
      methods.push(self.parse_fn_declaration(FnKind::Method)?);
    }
    self.consume_expecting(RightBrace, "expected `}` after class body")?;
    Ok(Stmt::Class(stmt::Class { name, methods }))
  }

  fn parse_variable_declaration(&mut self) -> Result<Stmt> {
    let name = self.consume_expecting(Identifier, "expected a variable name")?;
    let initializer = match self.consume_discarding(Equal) {
      true => Some(self.parse_expression()?),
      false => None,
    };
    self.consume_expecting(Semicolon, "expected `;` after variable declaration")?;
    Ok(Stmt::Var { name, initializer })
  }

  fn parse_statement(&mut self) -> Result<Stmt> {
    if self.consume_discarding(Print) {
      self.parse_print_stmt()
    } else if let Some(keyword) = self.consume_if(Return) {
      self.parse_return_stmt(keyword)
    } else if self.consume_discarding(For) {
      self.parse_for_stmt()
    } else if self.consume_discarding(If) {
      self.parse_if_stmt()
    } else if self.consume_discarding(While) {
      self.parse_while_stmt()
    } else if self.consume_discarding(LeftBrace) {
      Ok(Stmt::Block(self.parse_block()?))
    } else {
      self.parse_expression_stmt()
    }
  }

  fn parse_block(&mut self) -> Result<Vec<Stmt>> {
    let mut stmts = Vec::new();
    while !self.peek_is(&RightBrace) && !self.is_at_end() {
      stmts.push(self.parse_declaration()?);
    }
    self.consume_expecting(RightBrace, "expected `}` after block")?;
    Ok(stmts)
  }

  fn parse_print_stmt(&mut self) -> Result<Stmt> {
    let expr = self.parse_expression()?;
    self.consume_expecting(Semicolon, "expected `;` after value")?;
    Ok(Stmt::Print(expr))
  }

  fn parse_return_stmt(&mut self, keyword: Token) -> Result<Stmt> {
    let value = match self.peek_is(&Semicolon) {
      false => Some(self.parse_expression()?),
      true => None,
    };
    self.consume_expecting(Semicolon, "expected `;` after return value")?;
    Ok(Stmt::Return { keyword, value })
  }

  fn parse_while_stmt(&mut self) -> Result<Stmt> {
    self.consume_expecting(LeftParen, "expected `(` after `while`")?;
    let condition = self.parse_expression()?;
    self.consume_expecting(RightParen, "expected `)` after `condition`")?;
    let body = Box::new(self.parse_statement()?);
    Ok(Stmt::While(WhileStmt { condition, body }))
  }

  fn parse_for_stmt(&mut self) -> Result<Stmt> {
    self.consume_expecting(LeftParen, "expected `(` after `for`")?;
    let initializer = match self.consume_one_of(&[Semicolon, Var]) {
      Some(Token::Semicolon(_)) => None,
      Some(Token::Var(_)) => Some(self.parse_variable_declaration()?),
      _ => Some(self.parse_expression_stmt()?),
    };

    let condition = match self.peek_is(&Semicolon) {
      true => None,
      false => Some(self.parse_expression()?),
    };
    self.consume_expecting(Semicolon, "expected `;` after loop condition")?;

    let increment = match self.peek_is(&RightParen) {
      true => None,
      false => Some(self.parse_expression()?),
    };
    self.consume_expecting(RightParen, "expected `)` after for clauses")?;

    let mut body = self.parse_statement()?;
    if let Some(increment) = increment {
      body = Stmt::Block(vec![body, Stmt::Expression(increment)])
    }

    if let Some(condition) = condition {
      body = Stmt::While(WhileStmt {
        condition,
        body: Box::new(body),
      })
    }

    if let Some(initializer) = initializer {
      body = Stmt::Block(vec![initializer, body]);
    }
    Ok(body)
  }

  fn parse_if_stmt(&mut self) -> Result<Stmt> {
    self.consume_expecting(LeftParen, "expected `(` after `if`")?;
    let condition = self.parse_expression()?;
    self.consume_expecting(RightParen, "expected `)` after `condition`")?;
    let then_branch = Box::new(self.parse_statement()?);
    let else_branch = match self.consume_discarding(Else) {
      true => Some(Box::new(self.parse_statement()?)),
      false => None,
    };
    Ok(Stmt::If(IfStmt {
      condition,
      then_branch,
      else_branch,
    }))
  }

  fn parse_expression_stmt(&mut self) -> Result<Stmt> {
    let expr = self.parse_expression()?;
    self.consume_expecting(Semicolon, "expected `;` after expression")?;
    Ok(Stmt::Expression(expr))
  }

  fn parse_expression(&mut self) -> Result<Expr> {
    self.parse_assignment()
  }

  fn parse_assignment(&mut self) -> Result<Expr> {
    let expr = self.parse_or()?;
    if let Some(equals) = self.consume_if(Equal) {
      let value = self.parse_assignment()?;
      match expr {
        Expr::Variable(variable) => Ok(Expr::Assign(Assign {
          name: variable.name,
          value: Box::new(value),
          distance: None,
        })),
        _ => Err(LoxErr::Parse {
          line: equals.line(),
          message: "invalid assignment target".to_string(),
        }),
      }
    } else {
      Ok(expr)
    }
  }

  fn parse_or(&mut self) -> Result<Expr> {
    self.parse_logical(Or, Parser::parse_and)
  }

  fn parse_and(&mut self) -> Result<Expr> {
    self.parse_logical(And, Parser::parse_equality)
  }

  fn parse_logical<F>(&mut self, token_type: TokenType, parse: F) -> Result<Expr>
  where
    F: Fn(&mut Parser<'a>) -> Result<Expr>,
  {
    let mut expr = parse(self)?;
    while let Some(operator) = self.consume_if(token_type) {
      let right = parse(self)?;
      expr = Expr::Logical(Logical {
        left: Box::new(expr),
        operator: operator
          .try_into()
          .expect("unexpected non-logical operator"),
        right: Box::new(right),
      });
    }
    Ok(expr)
  }

  fn parse_equality(&mut self) -> Result<Expr> {
    self.parse_binary(&[BangEqual, EqualEqual], Self::parse_comparison)
  }

  fn parse_comparison(&mut self) -> Result<Expr> {
    self.parse_binary(&[Greater, GreaterEqual, Less, LessEqual], Self::parse_term)
  }

  fn parse_term(&mut self) -> Result<Expr> {
    self.parse_binary(&[Minus, Plus], Self::parse_factor)
  }

  fn parse_factor(&mut self) -> Result<Expr> {
    self.parse_binary(&[Slash, Star], Self::parse_unary)
  }

  fn parse_binary<F>(&mut self, op_types: &[TokenType], parse: F) -> Result<Expr>
  where
    F: Fn(&mut Parser<'a>) -> Result<Expr>,
  {
    let mut expr = parse(self)?;
    while let Some(operator) = self.consume_one_of(op_types) {
      expr = Expr::Binary(Binary {
        left: Box::new(expr),
        operator: operator.try_into().expect("unexpected non-binary operator"),
        right: Box::new(parse(self)?),
      });
    }
    Ok(expr)
  }

  fn parse_unary(&mut self) -> Result<Expr> {
    if let Some(operator) = self.consume_one_of(&[Bang, Minus]) {
      Ok(Expr::Unary(Unary {
        operator: operator.try_into().expect("unexpected non-unary operator"),
        right: Box::new(self.parse_unary()?),
      }))
    } else {
      self.parse_call()
    }
  }

  fn parse_call(&mut self) -> Result<Expr> {
    let mut expr = self.parse_primary()?;
    // bob says loop will make sense when we handle properties on objects...
    loop {
      if self.consume_discarding(LeftParen) {
        expr = self.finish_call(expr)?;
      } else {
        break;
      }
    }
    Ok(expr)
  }

  fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
    let mut args = Vec::new();
    if !self.peek_is(&RightParen) {
      loop {
        args.push(self.parse_expression()?);
        if !self.consume_discarding(Comma) {
          break;
        }
      }
    }
    let paren = self.consume_expecting(RightParen, "expected `)` after arguments")?;
    Ok(Expr::Call(Call {
      callee: Box::new(callee),
      paren,
      args,
    }))
  }

  fn parse_primary(&mut self) -> Result<Expr> {
    if self.consume_discarding(False) {
      Ok(Expr::Literal(Literal::False))
    } else if self.consume_discarding(True) {
      Ok(Expr::Literal(Literal::True))
    } else if self.consume_discarding(Nil) {
      Ok(Expr::Literal(Literal::Nil))
    } else if let Some(token) = self.consume_if(Identifier) {
      Ok(Expr::Variable(Variable {
        name: token,
        distance: None,
      }))
    } else if let Some(token) = self.consume_one_of(&[Number, Str]) {
      match token {
        Token::Number(_, number) => Ok(Expr::Literal(Literal::Number(number))),
        Token::String(_, string) => Ok(Expr::Literal(Literal::String(string))),
        _ => panic!("unreachable"),
      }
    } else if self.consume_discarding(LeftParen) {
      let expr = Box::new(self.parse_expression()?);
      self.consume_expecting(RightParen, "expected `)` after expression")?;
      Ok(Expr::Grouping(Grouping { expr }))
    } else {
      Err(self.parse_error("expected an expression"))
    }
  }

  fn synchronize_after_error(&mut self) {
    while !self.is_at_end() {
      if self.consume_discarding(Semicolon) {
        return;
      } else if self.peek_one_of(&[Class, Fun, Var, For, If, While, Print, Return]) {
        return;
      } else {
        self.tokens.next();
      }
    }
  }

  fn consume_expecting(
    &mut self,
    token_type: TokenType,
    error_message: &'static str,
  ) -> Result<Token> {
    if let Some(token) = self.consume_if(token_type) {
      Ok(token)
    } else {
      Err(self.parse_error(error_message))
    }
  }

  fn consume_discarding(&mut self, token_type: TokenType) -> bool {
    self.consume_if(token_type).is_some()
  }

  fn consume_one_of(&mut self, token_types: &[TokenType]) -> Option<Token> {
    if self.peek_one_of(token_types) {
      self.tokens.next()
    } else {
      None
    }
  }

  fn consume_if(&mut self, token_type: TokenType) -> Option<Token> {
    if self.peek_is(&token_type) {
      return self.tokens.next();
    }
    None
  }

  fn peek_is(&mut self, token_type: &TokenType) -> bool {
    let peeked = self.tokens.peek();
    !peeked.is_none() && peeked.unwrap().is_type(*token_type)
  }

  fn peek_one_of(&mut self, token_types: &[TokenType]) -> bool {
    for token_type in token_types {
      if self.peek_is(token_type) {
        return true;
      }
    }
    false
  }

  fn is_at_end(&mut self) -> bool {
    self.tokens.peek().is_none()
  }

  fn parse_error<S>(&mut self, message: S) -> LoxErr
  where
    S: Into<String>,
  {
    LoxErr::Parse {
      line: self.tokens.peek().map(Token::line).unwrap_or(0),
      message: message.into(),
    }
  }

  pub fn new(source: &'a String) -> Self {
    Parser {
      tokens: Scanner::new(source).peekable(),
    }
  }

  pub fn from_str(source: &'a str) -> Self {
    Parser {
      tokens: Scanner::from_str(source).peekable(),
    }
  }
}

#[derive(Debug, PartialEq, Eq)]
enum FnKind {
  Function,
  Method,
}

impl FnKind {
  fn ident_error(&self) -> &'static str {
    match self {
      FnKind::Function => "expected function name",
      FnKind::Method => "expected method name",
    }
  }
  fn lparen_error(&self) -> &'static str {
    match self {
      FnKind::Function => "expected `(` after function name",
      FnKind::Method => "expected `(` after method name",
    }
  }
  fn lbrace_error(&self) -> &'static str {
    match self {
      FnKind::Function => "expected `{` before function body",
      FnKind::Method => "expected `{` before method body",
    }
  }
}

// tests

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_parse_literal_exprs() {
    assert_parsed_expressions(vec![
      ("\"hi\"", Expr::Literal(Literal::String("hi".to_string()))),
      ("true", Expr::Literal(Literal::True)),
      ("false", Expr::Literal(Literal::False)),
      ("nil", Expr::Literal(Literal::Nil)),
      ("33.33", Expr::Literal(Literal::Number(33.33))),
    ]);
  }

  #[test]
  fn test_parse_unary_exprs() {
    assert_parsed_expressions(vec![
      (
        "!true",
        Expr::Unary(Unary {
          operator: UnaryOp::Bang(1),
          right: Box::new(Expr::Literal(Literal::True)),
        }),
      ),
      (
        "-33.33",
        Expr::Unary(Unary {
          operator: UnaryOp::Minus(1),
          right: Box::new(Expr::Literal(Literal::Number(33.33))),
        }),
      ),
    ]);
  }

  #[test]
  fn test_parse_binary_exprs() {
    assert_parsed_expressions(vec![
      (
        "true == true",
        Expr::Binary(Binary {
          left: Box::new(Expr::Literal(Literal::True)),
          operator: BinaryOp::EqualEqual(1),
          right: Box::new(Expr::Literal(Literal::True)),
        }),
      ),
      (
        "5 <= 6",
        Expr::Binary(Binary {
          left: Box::new(Expr::Literal(Literal::Number(5.0))),
          operator: BinaryOp::LessEqual(1),
          right: Box::new(Expr::Literal(Literal::Number(6.0))),
        }),
      ),
    ]);
  }

  #[test]
  fn test_parse_assignment() {
    assert_parsed_expressions(vec![
      (
        "x = true",
        Expr::Assign(Assign {
          name: Token::Identifier(1, "x".to_string()),
          value: Box::new(Expr::Literal(Literal::True)),
          distance: None,
        }),
      ),
      (
        "foobar = 33 + 1.3",
        Expr::Assign(Assign {
          name: Token::Identifier(1, "foobar".to_string()),
          value: Box::new(Expr::Binary(Binary {
            left: Box::new(Expr::Literal(Literal::Number(33.0))),
            operator: BinaryOp::Plus(1),
            right: Box::new(Expr::Literal(Literal::Number(1.3))),
          })),
          distance: None,
        }),
      ),
    ]);
  }

  #[test]
  fn test_parse_logical() {
    assert_parsed_expressions(vec![
      (
        "x or true",
        Expr::Logical(Logical {
          left: Box::new(Expr::Variable(Variable {
            name: Token::Identifier(1, "x".to_string()),
            distance: None,
          })),
          operator: LogicalOp::Or(1),
          right: Box::new(Expr::Literal(Literal::True)),
        }),
      ),
      (
        "true and false",
        Expr::Logical(Logical {
          left: Box::new(Expr::Literal(Literal::True)),
          operator: LogicalOp::And(1),
          right: Box::new(Expr::Literal(Literal::False)),
        }),
      ),
    ]);
  }

  #[test]
  fn test_parse_call_exprs() {
    assert_parsed_expressions(vec![
      (
        "foo()",
        Expr::Call(Call {
          callee: Box::new(Expr::Variable(Variable {
            name: Token::Identifier(1, "foo".to_string()),
            distance: None,
          })),
          paren: Token::RightParen(1),
          args: vec![],
        }),
      ),
      (
        "foo(true)",
        Expr::Call(Call {
          callee: Box::new(Expr::Variable(Variable {
            name: Token::Identifier(1, "foo".to_string()),
            distance: None,
          })),
          paren: Token::RightParen(1),
          args: vec![Expr::Literal(Literal::True)],
        }),
      ),
      (
        "foo(1,2,3)",
        Expr::Call(Call {
          callee: Box::new(Expr::Variable(Variable {
            name: Token::Identifier(1, "foo".to_string()),
            distance: None,
          })),
          paren: Token::RightParen(1),
          args: vec![
            Expr::Literal(Literal::Number(1.0)),
            Expr::Literal(Literal::Number(2.0)),
            Expr::Literal(Literal::Number(3.0)),
          ],
        }),
      ),
    ]);
  }

  #[test]
  fn test_parse_grouped_exprs() {
    assert_parsed_expressions(vec![
      (
        "(true)",
        Expr::Grouping(Grouping {
          expr: Box::new(Expr::Literal(Literal::True)),
        }),
      ),
      (
        "(5 <= 6)",
        Expr::Grouping(Grouping {
          expr: Box::new(Expr::Binary(Binary {
            left: Box::new(Expr::Literal(Literal::Number(5.0))),
            operator: BinaryOp::LessEqual(1),
            right: Box::new(Expr::Literal(Literal::Number(6.0))),
          })),
        }),
      ),
    ]);
  }

  #[test]
  fn test_parse_errors() {
    let cases = vec![
      (
        "(true;",
        LoxErr::Parse {
          line: 1,
          message: "expected `)` after expression".to_string(),
        },
      ),
      (
        "*;",
        LoxErr::Parse {
          line: 1,
          message: "expected an expression".to_string(),
        },
      ),
      (
        "a + b = c",
        LoxErr::Parse {
          line: 1,
          message: "invalid assignment target".to_string(),
        },
      ),
      (
        "(a) = 2",
        LoxErr::Parse {
          line: 1,
          message: "invalid assignment target".to_string(),
        },
      ),
      (
        "(true; *;",
        LoxErr::Many(vec![
          Box::new(LoxErr::Parse {
            line: 1,
            message: "expected `)` after expression".to_string(),
          }),
          Box::new(LoxErr::Parse {
            line: 1,
            message: "expected an expression".to_string(),
          }),
        ]),
      ),
    ];
    for (input, expected_err) in cases {
      let mut parser = Parser::from_str(input);
      assert_eq!(parser.parse(), Err(expected_err));
    }
  }

  #[test]
  fn test_parse_variable_decls() {
    assert_parsed_statements(vec![
      (
        "var x;",
        Stmt::Var {
          name: Token::Identifier(1, "x".to_string()),
          initializer: None,
        },
      ),
      (
        "var foobar = 33;",
        Stmt::Var {
          name: Token::Identifier(1, "foobar".to_string()),
          initializer: Some(Expr::Literal(Literal::Number(33.0))),
        },
      ),
    ])
  }

  #[test]
  fn test_parse_fn_decls() {
    assert_parsed_statements(vec![
      (
        "fun ident(x) { return x; }",
        Stmt::Function(FnStmt {
          name: Token::Identifier(1, "ident".to_string()),
          params: vec![Token::Identifier(1, "x".to_string())],
          body: vec![Stmt::Return {
            keyword: Token::Return(1),
            value: Some(Expr::Variable(Variable {
              name: Token::Identifier(1, "x".to_string()),
              distance: None,
            })),
          }],
        }),
      ),
      (
        "fun foo() { nil; }",
        Stmt::Function(FnStmt {
          name: Token::Identifier(1, "foo".to_string()),
          params: vec![],
          body: vec![Stmt::Expression(Expr::Literal(Literal::Nil))],
        }),
      ),
      (
        "fun foo(a,b,c) { true; }",
        Stmt::Function(FnStmt {
          name: Token::Identifier(1, "foo".to_string()),
          params: vec![
            Token::Identifier(1, "a".to_string()),
            Token::Identifier(1, "b".to_string()),
            Token::Identifier(1, "c".to_string()),
          ],
          body: vec![Stmt::Expression(Expr::Literal(Literal::True))],
        }),
      ),
    ])
  }

  #[test]
  fn test_parse_blocks() {
    assert_parsed_statements(vec![
      (
        "{ var x; }",
        Stmt::Block(vec![Stmt::Var {
          name: Token::Identifier(1, "x".to_string()),
          initializer: None,
        }]),
      ),
      ("{}", Stmt::Block(vec![])),
    ]);
  }

  #[test]
  fn test_parse_if_stmts() {
    assert_parsed_statements(vec![
      (
        "if (x) { 3; }",
        Stmt::If(IfStmt {
          condition: Expr::Variable(Variable {
            name: Token::Identifier(1, "x".to_string()),
            distance: None,
          }),
          then_branch: Box::new(Stmt::Block(vec![Stmt::Expression(Expr::Literal(
            Literal::Number(3.0),
          ))])),
          else_branch: None,
        }),
      ),
      (
        "if (x) { 3; } else { nil; }",
        Stmt::If(IfStmt {
          condition: Expr::Variable(Variable {
            name: Token::Identifier(1, "x".to_string()),
            distance: None,
          }),
          then_branch: Box::new(Stmt::Block(vec![Stmt::Expression(Expr::Literal(
            Literal::Number(3.0),
          ))])),
          else_branch: Some(Box::new(Stmt::Block(vec![Stmt::Expression(
            Expr::Literal(Literal::Nil),
          )]))),
        }),
      ),
    ]);
  }

  #[test]
  fn test_parse_class_stmts() {
    assert_parsed_statements(vec![
      (
        "class Breakfast {}",
        Stmt::Class(Class {
          name: Token::Identifier(1, "Breakfast".to_string()),
          methods: vec![],
        }),
      ),
      (
        "class Breakfast { foo() { nil; } }",
        Stmt::Class(Class {
          name: Token::Identifier(1, "Breakfast".to_string()),
          methods: vec![Stmt::Function(FnStmt {
            name: Token::Identifier(1, "foo".to_string()),
            params: vec![],
            body: vec![Stmt::Expression(Expr::Literal(Literal::Nil))],
          })],
        }),
      ),
    ])
  }

  #[test]
  fn test_parse_while_stmts() {
    assert_parsed_statements(vec![
      (
        "while (x) { 3; }",
        Stmt::While(WhileStmt {
          condition: Expr::Variable(Variable {
            name: Token::Identifier(1, "x".to_string()),
            distance: None,
          }),
          body: Box::new(Stmt::Block(vec![Stmt::Expression(Expr::Literal(
            Literal::Number(3.0),
          ))])),
        }),
      ),
      (
        "while (true) {}",
        Stmt::While(WhileStmt {
          condition: Expr::Literal(Literal::True),
          body: Box::new(Stmt::Block(vec![])),
        }),
      ),
      (
        // test for loops de-sugar to while loops
        "for (var i = 0; i < 10; i = i + 1) print i;",
        Stmt::Block(vec![
          Stmt::Var {
            name: Token::Identifier(1, "i".to_string()),
            initializer: Some(Expr::Literal(Literal::Number(0.0))),
          },
          Stmt::While(WhileStmt {
            condition: Expr::Binary(Binary {
              left: Box::new(Expr::Variable(Variable {
                name: Token::Identifier(1, "i".to_string()),
                distance: None,
              })),
              operator: BinaryOp::Less(1),
              right: Box::new(Expr::Literal(Literal::Number(10.0))),
            }),
            body: Box::new(Stmt::Block(vec![
              Stmt::Print(Expr::Variable(Variable {
                name: Token::Identifier(1, "i".to_string()),
                distance: None,
              })),
              Stmt::Expression(Expr::Assign(Assign {
                name: Token::Identifier(1, "i".to_string()),
                value: Box::new(Expr::Binary(Binary {
                  left: Box::new(Expr::Variable(Variable {
                    name: Token::Identifier(1, "i".to_string()),
                    distance: None,
                  })),
                  operator: BinaryOp::Plus(1),
                  right: Box::new(Expr::Literal(Literal::Number(1.0))),
                })),
                distance: None,
              })),
            ])),
          }),
        ]),
      ),
    ]);
  }

  fn assert_parsed_expressions(cases: Vec<(&str, Expr)>) {
    for (input, expected) in cases {
      let mut parser = Parser::from_str(input);
      assert_eq!(parser.parse_expression().unwrap(), expected);
    }
  }

  fn assert_parsed_statements(cases: Vec<(&str, Stmt)>) {
    for (input, expected) in cases {
      let mut parser = Parser::from_str(input);
      let program = parser.parse().unwrap();
      assert_eq!(program.len(), 1);
      assert_eq!(program[0], expected);
    }
  }

  fn assert_parsed_programs(cases: Vec<(&str, Vec<Stmt>)>) {
    for (input, expected) in cases {
      let mut parser = Parser::from_str(input);
      let program = parser.parse().unwrap();
      assert_eq!(program, expected);
    }
  }
}
