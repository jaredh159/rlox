#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Token {
  LeftParen(usize),
  RightParen(usize),
  LeftBrace(usize),
  RightBrace(usize),
  Comma(usize),
  Dot(usize),
  Minus(usize),
  Plus(usize),
  Semicolon(usize),
  Slash(usize),
  Star(usize),
  Bang(usize),
  BangEqual(usize),
  Equal(usize),
  EqualEqual(usize),
  Greater(usize),
  GreaterEqual(usize),
  Less(usize),
  LessEqual(usize),
  Identifier(usize, String),
  String(usize, String),
  Number(usize, String),
  And(usize),
  Class(usize),
  Else(usize),
  False(usize),
  Fun(usize),
  For(usize),
  If(usize),
  Nil(usize),
  Or(usize),
  Print(usize),
  Return(usize),
  Super(usize),
  This(usize),
  True(usize),
  Var(usize),
  While(usize),
  Eof(usize),
  Illegal(usize),
}
