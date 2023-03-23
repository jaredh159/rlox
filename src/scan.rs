use crate::{err::LoxErr, peektwo::PeekTwo, tok::Token};
use phf::phf_map;
use std::str::Chars;

pub struct Scanner<'a> {
  source: PeekTwo<Chars<'a>>,
  line: usize,
  errors: Vec<LoxErr>,
}

static KEYWORDS: phf::Map<&'static str, fn(usize) -> Token> = phf_map! {
  "and" => |line| Token::And(line),
  "class" => |line| Token::Class(line),
  "else" => |line| Token::Else(line),
  "false" => |line| Token::False(line),
  "for" => |line| Token::For(line),
  "fun" => |line| Token::Fun(line),
  "if" => |line| Token::If(line),
  "nil" => |line| Token::Nil(line),
  "or" => |line| Token::Or(line),
  "print" => |line| Token::Print(line),
  "return" => |line| Token::Return(line),
  "super" => |line| Token::Super(line),
  "this" => |line| Token::This(line),
  "true" => |line| Token::True(line),
  "var" => |line| Token::Var(line),
  "while" => |line| Token::While(line),
};

impl<'a> Scanner<'a> {
  pub fn new(source: &'a String) -> Self {
    Scanner {
      source: PeekTwo::new(source.chars()),
      line: 1,
      errors: vec![],
    }
  }

  pub fn from_str(source: &'a str) -> Self {
    Scanner {
      source: PeekTwo::new(source.chars()),
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
    if self.is_at_end() || self.peek_is(expected) {
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

  fn peek_satisfies(&mut self, predicate: fn(&char) -> bool) -> bool {
    let peeked = self.source.peek();
    !peeked.is_none() && predicate(peeked.unwrap())
  }

  fn peek_next_is(&mut self, ch: char) -> bool {
    let peeked_next = self.source.peek_next();
    !peeked_next.is_none() && *peeked_next.unwrap() == ch
  }

  fn peek_next_satisfies(&mut self, predicate: fn(&char) -> bool) -> bool {
    let peeked_next = self.source.peek_next();
    !peeked_next.is_none() && predicate(peeked_next.unwrap())
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
    Token::String(self.line, chars.iter().collect())
  }

  fn number(&mut self, first: char) -> Token {
    let mut chars = vec![first];
    while self.peek_satisfies(char::is_ascii_digit) {
      chars.push(self.advance().unwrap());
    }

    if self.peek_is('.') && self.peek_next_satisfies(char::is_ascii_digit) {
      chars.push(self.advance().unwrap()); // consume dot
      while self.peek_satisfies(char::is_ascii_digit) {
        chars.push(self.advance().unwrap());
      }
    }

    let string: String = chars.iter().collect();
    let parse_result = string.parse::<f64>();
    match parse_result {
      Err(_) => Token::Illegal(self.line, string),
      Ok(float) => Token::Number(self.line, float),
    }
  }

  fn identifier(&mut self, first: char) -> Token {
    let mut chars = vec![first];
    while self.peek_satisfies(is_alpha_numeric) {
      chars.push(self.advance().unwrap());
    }
    let ident: String = chars.iter().collect();
    match KEYWORDS.get(&ident) {
      Some(make_token) => make_token(self.line),
      None => Token::Identifier(self.line, ident),
    }
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
      ch if ch.is_ascii_digit() => self.number(ch),
      ch if is_alpha(&ch) => self.identifier(ch),
      ch => {
        self.errors.push(LoxErr::Scan {
          line: self.line,
          message: format!("Unexpected character: {}", char),
        });
        Token::Illegal(self.line, ch.to_string())
      }
    };
    Some(token)
  }
}

fn is_alpha(ch: &char) -> bool {
  ch.is_ascii_alphabetic() || *ch == '_'
}

fn is_alpha_numeric(ch: &char) -> bool {
  is_alpha(ch) || ch.is_ascii_digit()
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
  fn test_scan_identifiers() {
    let cases = vec![
      ("and", And(1)),
      ("class", Class(1)),
      ("else", Else(1)),
      ("false", False(1)),
      ("for", For(1)),
      ("fun", Fun(1)),
      ("if", If(1)),
      ("nil", Nil(1)),
      ("or", Or(1)),
      ("print", Print(1)),
      ("return", Return(1)),
      ("super", Super(1)),
      ("this", This(1)),
      ("true", True(1)),
      ("var", Var(1)),
      ("while", While(1)),
      ("foobar", Identifier(1, "foobar".to_string())),
      ("_1lol", Identifier(1, "_1lol".to_string())),
    ];
    for (input, expected) in cases {
      let mut scanner = Scanner::from_str(input);
      assert_eq!(scanner.tokens(), vec![expected]);
      assert!(scanner.errors.is_empty());
    }
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
  fn test_scan_number() {
    let mut scanner = Scanner::from_str("123 123.45 0.00");
    assert_eq!(
      scanner.tokens(),
      vec![Number(1, 123.0), Number(1, 123.45), Number(1, 0.0)]
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
