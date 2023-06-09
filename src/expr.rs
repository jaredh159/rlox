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
  pub fn lexeme(&self) -> &'static str {
    match self {
      BinaryOp::Minus(_) => "-",
      BinaryOp::Star(_) => "*",
      BinaryOp::Slash(_) => "/",
      BinaryOp::Plus(_) => "+",
      BinaryOp::Bang(_) => "+",
      BinaryOp::BangEqual(_) => "!=",
      BinaryOp::Greater(_) => ">",
      BinaryOp::GreaterEqual(_) => ">=",
      BinaryOp::Less(_) => "<",
      BinaryOp::LessEqual(_) => "<=",
      BinaryOp::EqualEqual(_) => "==",
    }
  }

  pub fn line(&self) -> &usize {
    match self {
      BinaryOp::Minus(line) => line,
      BinaryOp::Star(line) => line,
      BinaryOp::Slash(line) => line,
      BinaryOp::Plus(line) => line,
      BinaryOp::Bang(line) => line,
      BinaryOp::BangEqual(line) => line,
      BinaryOp::Greater(line) => line,
      BinaryOp::GreaterEqual(line) => line,
      BinaryOp::Less(line) => line,
      BinaryOp::LessEqual(line) => line,
      BinaryOp::EqualEqual(line) => line,
    }
  }
}

impl UnaryOp {
  pub fn lexeme(&self) -> &'static str {
    match self {
      UnaryOp::Minus(_) => "-",
      UnaryOp::Bang(_) => "+",
    }
  }

  pub fn line(&self) -> &usize {
    match self {
      UnaryOp::Minus(line) => line,
      UnaryOp::Bang(line) => line,
    }
  }
}

impl LogicalOp {
  pub fn lexeme(&self) -> &'static str {
    match self {
      LogicalOp::And(_) => "and",
      LogicalOp::Or(_) => "or",
    }
  }

  pub fn line(&self) -> &usize {
    match self {
      LogicalOp::And(line) => line,
      LogicalOp::Or(line) => line,
    }
  }
}

impl TryFrom<Token> for BinaryOp {
  type Error = LoxErr;
  fn try_from(token: Token) -> Result<Self, Self::Error> {
    match token.get_type() {
      TokenType::Minus => Ok(BinaryOp::Minus(token.line())),
      TokenType::Star => Ok(BinaryOp::Star(token.line())),
      TokenType::Slash => Ok(BinaryOp::Slash(token.line())),
      TokenType::Bang => Ok(BinaryOp::Bang(token.line())),
      TokenType::BangEqual => Ok(BinaryOp::BangEqual(token.line())),
      TokenType::Greater => Ok(BinaryOp::Greater(token.line())),
      TokenType::GreaterEqual => Ok(BinaryOp::GreaterEqual(token.line())),
      TokenType::Less => Ok(BinaryOp::Less(token.line())),
      TokenType::LessEqual => Ok(BinaryOp::LessEqual(token.line())),
      TokenType::EqualEqual => Ok(BinaryOp::EqualEqual(token.line())),
      TokenType::Plus => Ok(BinaryOp::Plus(token.line())),
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
      TokenType::Minus => Ok(UnaryOp::Minus(token.line())),
      TokenType::Bang => Ok(UnaryOp::Bang(token.line())),
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
      TokenType::And => Ok(LogicalOp::And(token.line())),
      TokenType::Or => Ok(LogicalOp::Or(token.line())),
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
    self.distance = Some(distance)
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
    self.distance = Some(distance)
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
    self.distance = Some(distance)
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
    self.distance = Some(distance)
  }
  fn get_distance(&self) -> Option<usize> {
    self.distance
  }
}
