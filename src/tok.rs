#[derive(Debug, PartialEq, Clone)]
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
  Number(usize, f64),
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
  Illegal(usize, String),
}

impl Token {
  pub const fn line(&self) -> usize {
    match self {
      Self::LeftParen(line)
      | Self::RightParen(line)
      | Self::LeftBrace(line)
      | Self::RightBrace(line)
      | Self::Comma(line)
      | Self::Dot(line)
      | Self::Minus(line)
      | Self::Plus(line)
      | Self::Semicolon(line)
      | Self::Slash(line)
      | Self::Star(line)
      | Self::Bang(line)
      | Self::BangEqual(line)
      | Self::Equal(line)
      | Self::EqualEqual(line)
      | Self::Greater(line)
      | Self::GreaterEqual(line)
      | Self::Less(line)
      | Self::LessEqual(line)
      | Self::Identifier(line, _)
      | Self::String(line, _)
      | Self::Number(line, _)
      | Self::And(line)
      | Self::Class(line)
      | Self::Else(line)
      | Self::False(line)
      | Self::Fun(line)
      | Self::For(line)
      | Self::If(line)
      | Self::Nil(line)
      | Self::Or(line)
      | Self::Print(line)
      | Self::Return(line)
      | Self::Super(line)
      | Self::This(line)
      | Self::True(line)
      | Self::Var(line)
      | Self::While(line)
      | Self::Eof(line)
      | Self::Illegal(line, _) => *line,
    }
  }

  pub fn lexeme(&self) -> &str {
    match self {
      Self::LeftParen(_) => "(",
      Self::RightParen(_) => ")",
      Self::LeftBrace(_) => "[",
      Self::RightBrace(_) => "]",
      Self::Comma(_) => ",",
      Self::Dot(_) => ".",
      Self::Minus(_) => "-",
      Self::Plus(_) => "+",
      Self::Semicolon(_) => ";",
      Self::Slash(_) => "/",
      Self::Star(_) => "*",
      Self::Bang(_) => "!",
      Self::BangEqual(_) => "!=",
      Self::Equal(_) => "=",
      Self::EqualEqual(_) => "==",
      Self::Greater(_) => ">",
      Self::GreaterEqual(_) => ">=",
      Self::Less(_) => "<",
      Self::LessEqual(_) => "<=",
      Self::Identifier(_, ident) => ident,
      Self::String(_, string) => string,
      Self::Number(_, _) => todo!("switch Self::Number to hold str, make eq, hash"),
      Self::And(_) => "and",
      Self::Class(_) => "class",
      Self::Else(_) => "else",
      Self::False(_) => "false",
      Self::Fun(_) => "fun",
      Self::For(_) => "for",
      Self::If(_) => "if",
      Self::Nil(_) => "nil",
      Self::Or(_) => "or",
      Self::Print(_) => "print",
      Self::Return(_) => "return",
      Self::Super(_) => "super",
      Self::This(_) => "this",
      Self::True(_) => "true",
      Self::Var(_) => "var",
      Self::While(_) => "while",
      Self::Eof(_) => "",
      Self::Illegal(_, illegal) => illegal,
    }
  }

  pub const fn get_type(&self) -> TokenType {
    match self {
      Self::LeftParen(_) => TokenType::LeftParen,
      Self::RightParen(_) => TokenType::RightParen,
      Self::LeftBrace(_) => TokenType::LeftBrace,
      Self::RightBrace(_) => TokenType::RightBrace,
      Self::Comma(_) => TokenType::Comma,
      Self::Dot(_) => TokenType::Dot,
      Self::Minus(_) => TokenType::Minus,
      Self::Plus(_) => TokenType::Plus,
      Self::Semicolon(_) => TokenType::Semicolon,
      Self::Slash(_) => TokenType::Slash,
      Self::Star(_) => TokenType::Star,
      Self::Bang(_) => TokenType::Bang,
      Self::BangEqual(_) => TokenType::BangEqual,
      Self::Equal(_) => TokenType::Equal,
      Self::EqualEqual(_) => TokenType::EqualEqual,
      Self::Greater(_) => TokenType::Greater,
      Self::GreaterEqual(_) => TokenType::GreaterEqual,
      Self::Less(_) => TokenType::Less,
      Self::LessEqual(_) => TokenType::LessEqual,
      Self::Identifier(_, _) => TokenType::Identifier,
      Self::String(_, _) => TokenType::Str,
      Self::Number(_, _) => TokenType::Number,
      Self::And(_) => TokenType::And,
      Self::Class(_) => TokenType::Class,
      Self::Else(_) => TokenType::Else,
      Self::False(_) => TokenType::False,
      Self::Fun(_) => TokenType::Fun,
      Self::For(_) => TokenType::For,
      Self::If(_) => TokenType::If,
      Self::Nil(_) => TokenType::Nil,
      Self::Or(_) => TokenType::Or,
      Self::Print(_) => TokenType::Print,
      Self::Return(_) => TokenType::Return,
      Self::Super(_) => TokenType::Super,
      Self::This(_) => TokenType::This,
      Self::True(_) => TokenType::True,
      Self::Var(_) => TokenType::Var,
      Self::While(_) => TokenType::While,
      Self::Eof(_) => TokenType::Eof,
      Self::Illegal(_, _) => TokenType::Illegal,
    }
  }

  pub fn is_type(&self, token_type: TokenType) -> bool {
    match self {
      Self::LeftParen(_) => token_type == TokenType::LeftParen,
      Self::RightParen(_) => token_type == TokenType::RightParen,
      Self::LeftBrace(_) => token_type == TokenType::LeftBrace,
      Self::RightBrace(_) => token_type == TokenType::RightBrace,
      Self::Comma(_) => token_type == TokenType::Comma,
      Self::Dot(_) => token_type == TokenType::Dot,
      Self::Minus(_) => token_type == TokenType::Minus,
      Self::Plus(_) => token_type == TokenType::Plus,
      Self::Semicolon(_) => token_type == TokenType::Semicolon,
      Self::Slash(_) => token_type == TokenType::Slash,
      Self::Star(_) => token_type == TokenType::Star,
      Self::Bang(_) => token_type == TokenType::Bang,
      Self::BangEqual(_) => token_type == TokenType::BangEqual,
      Self::Equal(_) => token_type == TokenType::Equal,
      Self::EqualEqual(_) => token_type == TokenType::EqualEqual,
      Self::Greater(_) => token_type == TokenType::Greater,
      Self::GreaterEqual(_) => token_type == TokenType::GreaterEqual,
      Self::Less(_) => token_type == TokenType::Less,
      Self::LessEqual(_) => token_type == TokenType::LessEqual,
      Self::Identifier(_, _) => token_type == TokenType::Identifier,
      Self::String(_, _) => token_type == TokenType::Str,
      Self::Number(_, _) => token_type == TokenType::Number,
      Self::And(_) => token_type == TokenType::And,
      Self::Class(_) => token_type == TokenType::Class,
      Self::Else(_) => token_type == TokenType::Else,
      Self::False(_) => token_type == TokenType::False,
      Self::Fun(_) => token_type == TokenType::Fun,
      Self::For(_) => token_type == TokenType::For,
      Self::If(_) => token_type == TokenType::If,
      Self::Nil(_) => token_type == TokenType::Nil,
      Self::Or(_) => token_type == TokenType::Or,
      Self::Print(_) => token_type == TokenType::Print,
      Self::Return(_) => token_type == TokenType::Return,
      Self::Super(_) => token_type == TokenType::Super,
      Self::This(_) => token_type == TokenType::This,
      Self::True(_) => token_type == TokenType::True,
      Self::Var(_) => token_type == TokenType::Var,
      Self::While(_) => token_type == TokenType::While,
      Self::Eof(_) => token_type == TokenType::Eof,
      Self::Illegal(_, _) => token_type == TokenType::Illegal,
    }
  }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TokenType {
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  Comma,
  Dot,
  Minus,
  Plus,
  Semicolon,
  Slash,
  Star,
  Bang,
  BangEqual,
  Equal,
  EqualEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  Identifier,
  Str,
  Number,
  And,
  Class,
  Else,
  False,
  Fun,
  For,
  If,
  Nil,
  Or,
  Print,
  Return,
  Super,
  This,
  True,
  Var,
  While,
  Eof,
  Illegal,
}
