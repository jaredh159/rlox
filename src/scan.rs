use crate::{err::LoxErr, tok::Token};
use std::{iter::Peekable, str::Chars};

pub struct Scanner<'a> {
  source: Peekable<Chars<'a>>,
  line: usize,
  errors: Vec<LoxErr>,
}

impl<'a> Scanner<'a> {
  pub fn new(source: &'a String) -> Self {
    Scanner {
      source: source.chars().peekable(),
      line: 1,
      errors: vec![],
    }
  }

  pub fn from_str(source: &'a str) -> Self {
    Scanner {
      source: source.chars().peekable(),
      line: 1,
      errors: vec![],
    }
  }

  pub fn tokens(&mut self) -> Vec<Token> {
    self.into_iter().collect()
  }

  fn is_at_end(&mut self) -> bool {
    self.source.peek().is_none()
  }

  fn advance_if(&mut self, expected: char) -> bool {
    let peeked = self.source.peek();
    if peeked.is_none() || *peeked.unwrap() != expected {
      false
    } else {
      _ = self.source.next();
      true
    }
  }

  fn advance_if_not(&mut self, expected: char) -> bool {
    let peeked = self.source.peek();
    if peeked.is_none() || *peeked.unwrap() == expected {
      false
    } else {
      _ = self.source.next();
      true
    }
  }

  fn advance_until(&mut self, expected: char) {
    while self.advance_if_not(expected) {}
  }

  fn advance_thru(&mut self, expected: char) {
    self.advance_until(expected);
    _ = self.source.next(); // consume expected
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
      '/' => {
        if self.advance_if('/') {
          // incr lines?
          self.advance_thru('\n');
          match self.next() {
            Some(token) => token,
            None => return None,
          }
        } else {
          Token::Slash(self.line)
        }
      }
      '!' => {
        if self.advance_if('=') {
          Token::BangEqual(self.line)
        } else {
          Token::Bang(self.line)
        }
      }
      '=' => {
        if self.advance_if('=') {
          Token::EqualEqual(self.line)
        } else {
          Token::Equal(self.line)
        }
      }
      '<' => {
        if self.advance_if('=') {
          Token::LessEqual(self.line)
        } else {
          Token::Less(self.line)
        }
      }
      '>' => {
        if self.advance_if('=') {
          Token::GreaterEqual(self.line)
        } else {
          Token::Greater(self.line)
        }
      }
      _ => {
        self.errors.push(LoxErr::Scan {
          line: self.line,
          message: format!("Unexpected character: {}", char),
        });
        Token::Illegal(self.line)
      }
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
  fn test_scan_simple_punctuation_tokens() {
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
    );
    assert!(scanner.errors.is_empty());
  }

  #[test]
  fn test_scan_comment() {
    let mut scanner = Scanner::from_str("// foo bar all a comment");
    assert!(scanner.tokens().is_empty());
    assert!(scanner.errors.is_empty());
  }

  #[test]
  fn test_scan_two_char_tokens() {
    let mut scanner = Scanner::from_str("!=;==;<=;>=;=;!;<;>;");
    assert_eq!(
      scanner.tokens(),
      vec![
        BangEqual(1),
        Semicolon(1),
        EqualEqual(1),
        Semicolon(1),
        LessEqual(1),
        Semicolon(1),
        GreaterEqual(1),
        Semicolon(1),
        Equal(1),
        Semicolon(1),
        Bang(1),
        Semicolon(1),
        Less(1),
        Semicolon(1),
        Greater(1),
        Semicolon(1),
      ]
    );
    assert!(scanner.errors.is_empty());
  }
}
