use crate::err::LoxErr;
use crate::resolver::Resolvable;
use crate::tok::{Token, TokenType};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
  Assign(Assign),
  Binary(Binary),
  Call(Call),
  Get(Get),
  Literal(Literal),
  Logical(Logical),
  Grouping(Grouping),
  Set(Set),
  Super(Super),
  This(This),
  Unary(Unary),
  Variable(Variable),
}

#[derive(Debug, PartialEq, Clone)]
pub struct This {
  pub keyword: Token,
  pub distance: Option<usize>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Super {
  pub keyword: Token,
  pub method: Token,
  pub distance: Option<usize>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Get {
  pub name: Token,
  pub object: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Set {
  pub name: Token,
  pub object: Box<Expr>,
  pub value: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
  pub name: Token,
  pub distance: Option<usize>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
  pub name: Token,
  pub value: Box<Expr>,
  pub distance: Option<usize>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binary {
  pub left: Box<Expr>,
  pub operator: BinaryOp,
  pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
  pub callee: Box<Expr>,
  pub paren: Token,
  pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Logical {
  pub left: Box<Expr>,
  pub operator: LogicalOp,
  pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Unary {
  pub operator: UnaryOp,
  pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
  Nil,
  True,
  False,
  String(String),
  Number(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Grouping {
  pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum LogicalOp {
  And(usize),
  Or(usize),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum UnaryOp {
  Bang(usize),
  Minus(usize),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum BinaryOp {
  Minus(usize),
  Star(usize),
  Slash(usize),
  Plus(usize),
  Bang(usize),
  BangEqual(usize),
  Greater(usize),
  GreaterEqual(usize),
  Less(usize),
  LessEqual(usize),
  EqualEqual(usize),
}

impl BinaryOp {
  pub const fn lexeme(&self) -> &'static str {
    match self {
      Self::Minus(_) => "-",
      Self::Star(_) => "*",
      Self::Slash(_) => "/",
      Self::Plus(_) => "+",
      Self::Bang(_) => "!",
      Self::BangEqual(_) => "!=",
      Self::Greater(_) => ">",
      Self::GreaterEqual(_) => ">=",
      Self::Less(_) => "<",
      Self::LessEqual(_) => "<=",
      Self::EqualEqual(_) => "==",
    }
  }

  pub const fn line(&self) -> &usize {
    match self {
      Self::Minus(line)
      | Self::Star(line)
      | Self::Slash(line)
      | Self::Plus(line)
      | Self::Bang(line)
      | Self::BangEqual(line)
      | Self::Greater(line)
      | Self::GreaterEqual(line)
      | Self::Less(line)
      | Self::LessEqual(line)
      | Self::EqualEqual(line) => line,
    }
  }
}

impl UnaryOp {
  pub const fn lexeme(&self) -> &'static str {
    match self {
      Self::Minus(_) => "-",
      Self::Bang(_) => "+",
    }
  }

  pub const fn line(&self) -> &usize {
    match self {
      Self::Minus(line) | Self::Bang(line) => line,
    }
  }
}

impl LogicalOp {
  pub const fn lexeme(&self) -> &'static str {
    match self {
      Self::And(_) => "and",
      Self::Or(_) => "or",
    }
  }

  pub const fn line(&self) -> &usize {
    match self {
      Self::And(line) | Self::Or(line) => line,
    }
  }
}

impl TryFrom<Token> for BinaryOp {
  type Error = LoxErr;
  fn try_from(token: Token) -> Result<Self, Self::Error> {
    match token.get_type() {
      TokenType::Minus => Ok(Self::Minus(token.line())),
      TokenType::Star => Ok(Self::Star(token.line())),
      TokenType::Slash => Ok(Self::Slash(token.line())),
      TokenType::Bang => Ok(Self::Bang(token.line())),
      TokenType::BangEqual => Ok(Self::BangEqual(token.line())),
      TokenType::Greater => Ok(Self::Greater(token.line())),
      TokenType::GreaterEqual => Ok(Self::GreaterEqual(token.line())),
      TokenType::Less => Ok(Self::Less(token.line())),
      TokenType::LessEqual => Ok(Self::LessEqual(token.line())),
      TokenType::EqualEqual => Ok(Self::EqualEqual(token.line())),
      TokenType::Plus => Ok(Self::Plus(token.line())),
      _ => Err(LoxErr::Parse {
        line: token.line(),
        message: format!("invalid token `{}` for binary operator", token.lexeme()),
      }),
    }
  }
}

impl TryFrom<Token> for UnaryOp {
  type Error = LoxErr;
  fn try_from(token: Token) -> Result<Self, Self::Error> {
    match token.get_type() {
      TokenType::Minus => Ok(Self::Minus(token.line())),
      TokenType::Bang => Ok(Self::Bang(token.line())),
      _ => Err(LoxErr::Parse {
        line: token.line(),
        message: format!("invalid token `{}` for unary operator", token.lexeme()),
      }),
    }
  }
}

impl TryFrom<Token> for LogicalOp {
  type Error = LoxErr;
  fn try_from(token: Token) -> Result<Self, Self::Error> {
    match token.get_type() {
      TokenType::And => Ok(Self::And(token.line())),
      TokenType::Or => Ok(Self::Or(token.line())),
      _ => Err(LoxErr::Parse {
        line: token.line(),
        message: format!("invalid token `{}` for logical operator", token.lexeme()),
      }),
    }
  }
}

impl Resolvable for Assign {
  fn name(&self) -> &Token {
    &self.name
  }
  fn set_distance(&mut self, distance: usize) {
    self.distance = Some(distance);
  }
  fn get_distance(&self) -> Option<usize> {
    self.distance
  }
}

impl Resolvable for Super {
  fn name(&self) -> &Token {
    &self.keyword
  }
  fn set_distance(&mut self, distance: usize) {
    self.distance = Some(distance);
  }
  fn get_distance(&self) -> Option<usize> {
    self.distance
  }
}

impl Resolvable for This {
  fn name(&self) -> &Token {
    &self.keyword
  }
  fn set_distance(&mut self, distance: usize) {
    self.distance = Some(distance);
  }
  fn get_distance(&self) -> Option<usize> {
    self.distance
  }
}

impl Resolvable for Variable {
  fn name(&self) -> &Token {
    &self.name
  }
  fn set_distance(&mut self, distance: usize) {
    self.distance = Some(distance);
  }
  fn get_distance(&self) -> Option<usize> {
    self.distance
  }
}
