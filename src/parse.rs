use crate::err::{LoxErr, Result};
use crate::expr::*;
use crate::scan::Scanner;
use crate::tok::{Token, TokenType, TokenType::*};
use std::iter::Peekable;

pub struct Parser<'a> {
  tokens: Peekable<Scanner<'a>>,
}

impl<'a> Parser<'a> {
  pub fn parse(&mut self) -> Result<Expr> {
    self.parse_expression()
  }

  fn parse_expression(&mut self) -> Result<Expr> {
    self.parse_equality()
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
        operator,
        right: Box::new(parse(self)?),
      });
    }
    Ok(expr)
  }

  fn parse_unary(&mut self) -> Result<Expr> {
    if let Some(operator) = self.consume_one_of(&[Bang, Minus]) {
      Ok(Expr::Unary(Unary {
        operator,
        right: Box::new(self.parse_unary()?),
      }))
    } else {
      self.parse_primary()
    }
  }

  fn parse_primary(&mut self) -> Result<Expr> {
    if self.consume_discarding(False) {
      Ok(Expr::Literal(Literal::False))
    } else if self.consume_discarding(True) {
      Ok(Expr::Literal(Literal::True))
    } else if self.consume_discarding(Nil) {
      Ok(Expr::Literal(Literal::Nil))
    } else if let Some(token) = self.consume_if(Number) {
      match token {
        Token::Number(_, number) => Ok(Expr::Literal(Literal::Number(number))),
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

  // https://craftinginterpreters.com/parsing-expressions.html
  // search `private void synchronize`
  fn synchronize_after_err(&mut self) {
    todo!()
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
    for token_type in token_types {
      if self.peek_is(token_type) {
        return self.tokens.next();
      }
    }
    None
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
    assert_parsed_cases(vec![
      ("true", Expr::Literal(Literal::True)),
      ("false", Expr::Literal(Literal::False)),
      ("nil", Expr::Literal(Literal::Nil)),
      ("33.33", Expr::Literal(Literal::Number(33.33))),
    ]);
  }

  #[test]
  fn test_parse_unary_exprs() {
    assert_parsed_cases(vec![
      (
        "!true",
        Expr::Unary(Unary {
          operator: Token::Bang(1),
          right: Box::new(Expr::Literal(Literal::True)),
        }),
      ),
      (
        "-33.33",
        Expr::Unary(Unary {
          operator: Token::Minus(1),
          right: Box::new(Expr::Literal(Literal::Number(33.33))),
        }),
      ),
    ]);
  }

  #[test]
  fn test_parse_binary_exprs() {
    assert_parsed_cases(vec![
      (
        "true == true",
        Expr::Binary(Binary {
          left: Box::new(Expr::Literal(Literal::True)),
          operator: Token::EqualEqual(1),
          right: Box::new(Expr::Literal(Literal::True)),
        }),
      ),
      (
        "5 <= 6",
        Expr::Binary(Binary {
          left: Box::new(Expr::Literal(Literal::Number(5.0))),
          operator: Token::LessEqual(1),
          right: Box::new(Expr::Literal(Literal::Number(6.0))),
        }),
      ),
    ]);
  }

  #[test]
  fn test_parse_grouped_exprs() {
    assert_parsed_cases(vec![
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
            operator: Token::LessEqual(1),
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
    ];
    for (input, expected_err) in cases {
      let mut parser = Parser::from_str(input);
      assert_eq!(parser.parse_expression(), Err(expected_err));
    }
  }

  fn assert_parsed_cases(cases: Vec<(&str, Expr)>) {
    for (input, expected) in cases {
      let mut parser = Parser::from_str(input);
      assert_eq!(parser.parse_expression().unwrap(), expected);
    }
  }
}
