use crate::tok::Token;
use std::{iter::Peekable, str::Chars};

pub struct Scanner<'a> {
  source: Peekable<Chars<'a>>,
  start: usize,
  current: usize,
  line: usize,
}

impl<'a> Scanner<'a> {
  pub fn new(source: &'a String) -> Self {
    Scanner {
      source: source.chars().peekable(),
      start: 0,
      current: 0,
      line: 1,
    }
  }

  pub fn from_str(source: &'a str) -> Self {
    Scanner {
      source: source.chars().peekable(),
      start: 0,
      current: 0,
      line: 1,
    }
  }

  pub fn tokens(&mut self) -> Vec<Token> {
    self.into_iter().collect()
  }

  fn is_at_end(&mut self) -> bool {
    self.source.peek().is_none()
  }
}

impl<'a> Iterator for Scanner<'a> {
  type Item = Token;
  fn next(&mut self) -> Option<Token> {
    let char = self.source.next()?;
    let token = match char {
      '(' => Token::LeftParen(self.line),
      ')' => Token::RightParen(self.line),
      '{' => Token::LeftBrace(self.line),
      '}' => Token::RightBrace(self.line),
      ',' => Token::Comma(self.line),
      '.' => Token::Dot(self.line),
      '-' => Token::Minus(self.line),
      '+' => Token::Plus(self.line),
      ';' => Token::Semicolon(self.line),
      '*' => Token::Star(self.line),
      _ => todo!("todo: handle illegal char {}", char),
    };
    Some(token)
  }
}

// tests

#[cfg(test)]
mod tests {
  use super::*;
  use crate::tok::Token::*;

  #[test]
  fn test_scan_tokens() {
    let mut scanner = Scanner::from_str("(){},.-+;*");
    assert_eq!(
      scanner.tokens(),
      vec![
        LeftParen(1),
        RightParen(1),
        LeftBrace(1),
        RightBrace(1),
        Comma(1),
        Dot(1),
        Minus(1),
        Plus(1),
        Semicolon(1),
        Star(1)
      ]
    )
  }
}
