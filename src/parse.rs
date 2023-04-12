use crate::err::{LoxErr, Result};
use crate::expr::*;
use crate::scan::Scanner;
use crate::stmt::{Stmt, *};
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
    if self.consume_discarding(Var) {
      self.parse_variable_declaration()
    } else {
      self.parse_statement()
    }
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
    } else if self.consume_discarding(If) {
      self.parse_if_stmt()
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

  fn parse_if_stmt(&mut self) -> Result<Stmt> {
    self.consume_expecting(LeftParen, "expected `(` after `if`")?;
    let condition = self.parse_expression()?;
    self.consume_expecting(RightParen, "expected `)` after `if`")?;
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
    let expr = self.parse_equality()?;
    if let Some(equals) = self.consume_if(Equal) {
      let value = self.parse_assignment()?;
      match expr {
        Expr::Variable(token) => Ok(Expr::Assign(Assign {
          name: token,
          value: Box::new(value),
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
      self.parse_primary()
    }
  }

  // 👍 todo:
  // then, implement environment, empty visit stmt fns in Interpreter
  // also, rework/extend test in eval.rs

  fn parse_primary(&mut self) -> Result<Expr> {
    if self.consume_discarding(False) {
      Ok(Expr::Literal(Literal::False))
    } else if self.consume_discarding(True) {
      Ok(Expr::Literal(Literal::True))
    } else if self.consume_discarding(Nil) {
      Ok(Expr::Literal(Literal::Nil))
    } else if let Some(token) = self.consume_if(Identifier) {
      Ok(Expr::Variable(token))
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
          condition: Expr::Variable(Token::Identifier(1, "x".to_string())),
          then_branch: Box::new(Stmt::Block(vec![Stmt::Expression(Expr::Literal(
            Literal::Number(3.0),
          ))])),
          else_branch: None,
        }),
      ),
      (
        "if (x) { 3; } else { nil; }",
        Stmt::If(IfStmt {
          condition: Expr::Variable(Token::Identifier(1, "x".to_string())),
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
}
