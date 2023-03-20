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

  fn advance(&mut self) -> Option<char> {
    self.source.next()
  }

  fn advance_if(&mut self, expected: char) -> bool {
    if !self.peek_is(expected) {
      false
    } else {
      self.advance();
      true
    }
  }

  fn advance_if_not(&mut self, expected: char) -> bool {
    if self.peek_is(expected) {
      false
    } else {
      self.advance();
      true
    }
  }

  fn advance_until(&mut self, expected: char) {
    while self.advance_if_not(expected) {}
  }

  fn advance_thru(&mut self, expected: char) {
    self.advance_until(expected);
    self.advance(); // consume expected
  }

  fn peek_is(&mut self, ch: char) -> bool {
    let peeked = self.source.peek();
    !peeked.is_none() && *peeked.unwrap() == ch
  }

  fn string(&mut self) -> Token {
    let mut chars: Vec<char> = Vec::new();
    while let Some(next) = self.advance() {
      if next == '\n' {
        self.line += 1;
      } else if next == '"' {
        return Token::String(self.line, chars.iter().collect());
      }
      chars.push(next);
    }
    self.errors.push(LoxErr::Scan {
      line: self.line,
      message: "unterminated string".to_string(),
    });
    return Token::String(self.line, chars.iter().collect());
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
      '"' => self.string(),
      ' ' | '\r' | '\t' => return self.next(),
      '\n' => {
        self.line += 1;
        return self.next();
      }
      '/' => {
        if self.advance_if('/') {
          self.advance_until('\n');
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
  fn test_scan_string() {
    let mut scanner = Scanner::from_str(r#""foobar baz""#);
    assert_eq!(
      scanner.tokens(),
      vec![Token::String(1, "foobar baz".to_string())]
    );
    assert!(scanner.errors.is_empty());
  }

  #[test]
  fn test_scan_errors() {
    let cases = vec![(
      "\"unterminated string",
      vec![LoxErr::Scan {
        line: 1,
        message: "unterminated string".to_string(),
      }],
    )];

    for (input, expected_errs) in cases {
      let mut scanner = Scanner::from_str(input);
      scanner.tokens();
      assert_eq!(scanner.errors, expected_errs);
    }
  }

  #[test]
  fn test_skips_whitespace() {
    let mut scanner = Scanner::from_str(" ;\t;\n ;");
    assert_eq!(
      scanner.tokens(),
      vec![Semicolon(1), Semicolon(1), Semicolon(2)]
    );
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
