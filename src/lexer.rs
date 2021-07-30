use crate::token_kinds::TokenKind::*;
use crate::token_kinds::Literal::*;
use crate::token_kinds::BinOp::*;
use crate::token_kinds::TokenKind;
use std::str;

#[derive(Clone, Copy)]
pub struct Token {
  pub kind:    TokenKind,
  pub val: &'static [u8],
  pub length: usize
}

impl Token {
  pub fn new(kind: TokenKind, val: &'static [u8], length: usize) -> Token {
    Token {kind: kind, val: val, length: length}
  }

  pub fn combine_tokens(&self, tok: Token) -> TokenKind {
    match self.kind {
      Eq => match tok.kind {
        Eq => DEq,
        Bin(Gt) => FatRArrow,
        _ => Fail
      },
      Bin(Lt) => match tok.kind {
        Eq => LEq,
        _ => Fail
      },
      Bin(Gt) => match tok.kind {
        Eq => GEq,
        _ => Fail
      },
      Not => match tok.kind {
        Eq => NotEq,
        _ => Fail
      },
      Bin(Add) => match tok.kind {
        Eq => AddEq,
        _ => Fail
      },
      Bin(Sub) => match tok.kind {
        Eq => SubEq,
        _ => Fail
      },
      Bin(Mul) => match tok.kind {
        Eq => MulEq,
        _ => Fail
      },
      Bin(Div) => match tok.kind {
        Eq => DivEq,
        _ => Fail
      },
      Dot => match tok.kind {
        Dot => DotDot,
        _ => Fail
      },
      Colon => match tok.kind {
        Colon => ColonColon,
        _ => Fail
      },
      _ => Fail
    }
  }

  pub fn split(&mut self) -> (TokenKind, TokenKind) {
    match self.kind {
      DEq => (Eq, Eq),
      NotEq => (Not, Eq),
      AndAnd => (And, And),
      ColonColon => (Colon, Colon),
      OrOr => (Or, Or),
      RArrow => (Bin(Sub), Bin(Gt)),
      FatRArrow => (Eq, Bin(Gt)),
      AddEq => (Bin(Add), Eq),
      SubEq => (Bin(Sub), Eq),
      MulEq => (Bin(Mul), Eq),
      DivEq => (Bin(Div), Eq),
      _ => (Fail, Fail)
    }
  }

  pub fn keyword(&mut self) -> TokenKind {
    match str::from_utf8(self.val).unwrap() {
      "while" => While,
      "as" => As,
      "break" => Break,
      "continue" => Continue,
      "if" => If,
      "else" => Else,
      "true" => True,
      "false" => False,
      "for" => For,
      "in" => In,
      "switch" => Switch,
      "const" => Const,
      "return" => Return,
      "i32" => I32Type,
      "i64" => I64Type,
      "f32" => F32Type,
      _ => Fail
    }
  }
}

struct Lexer {
  input: &'static [u8],
  index: usize
}

impl Lexer {
  pub fn new(input_string: &'static str) -> Lexer {
    Lexer {input: input_string.as_bytes(), index: 0}
  }

  fn bump(&mut self, with: usize) {
    self.index += with;
  }

  fn first(&mut self) -> char {
    self.input[self.index] as char
  }

  fn second(&mut self) -> char {
    self.input[self.index + 1] as char
  }

  fn block_comment(&mut self) -> TokenKind {
    while (self.first(), self.second()) != ('*', '/') {
      self.bump(1);
    }
    self.bump(2);

    Comment
  }

  fn line_comment(&mut self) -> TokenKind {
    while self.first() != '\n' {
      self.bump(1);
    }
    self.bump(1);

    Comment
  }

  fn slash(&mut self) -> TokenKind {
    let kind = 
      match self.second() {
        '*' => {
          self.block_comment()
        },
        '/' => self.line_comment(),
        _ => Bin(Div)
      };

    kind
  }

  fn is_id_continue(c: char) -> bool {
    c == '_' || c.is_alphanumeric()
  }

  fn eat_while(&mut self, mut condition: impl FnMut(char) -> bool) {
    while !self.is_eoi() && condition(self.first()) {
      self.bump(1);
    }
  }

  fn ident(&mut self) -> TokenKind {
    self.eat_while(Lexer::is_id_continue);
    Ident
  }

  fn is_id_start(&mut self) -> bool {
    self.first().is_alphabetic() || self.first() == '_'
  }

  fn is_number(c: char) -> bool {
    c.is_numeric() || c == '_'
  }

  fn number(&mut self) -> TokenKind {
    let mut kind = Lit(IntLiteral);
    while Lexer::is_number(self.first()) && !self.is_eoi() {
      self.bump(1);
      if self.first() == '.' {
        kind = Lit(FloatLiteral);
        self.bump(1)
      }
    }

    kind
  }

  fn is_whitespace(c: char) -> bool {
    matches!(
      c,
      ' '
      | '\t'
      | '\r'
      | '\u{000B}'
    )
  }

  fn whitespace(&mut self) -> TokenKind {
    self.eat_while(Lexer::is_whitespace);

    WhiteSpace
  }

  fn not_is_string_end(c: char) -> bool {
    !(c == '"')
  }

  fn string(&mut self) -> TokenKind {
    self.bump(1);
    self.eat_while(Lexer::not_is_string_end);
    self.bump(1);
    Lit(StringLiteral)
  }

  pub fn is_eoi(&self) -> bool {
    self.index >= self.input.len()
  }

  pub fn advance(&mut self) -> Token {
    let previous_index: usize = self.index;
    let tokind =
      match self.first() {
        '/' => self.slash(),
        '"' => self.string(),
        _c if Lexer::is_whitespace(self.first()) => self.whitespace(),
        _c if self.is_id_start() => self.ident(),
        _c @ '0'..='9' => self.number(),

        ';' => Semi,
        ',' => Comma,
        '.' => Dot,
        '(' => OpenParen,
        ')' => CloseParen,
        '{' => OpenBrace,
        '}' => CloseBrace,
        '[' => OpenBracket,
        ']' => CloseBracket,
        '~' => Tilde,
        ':' => Colon,
        '=' => Eq,
        '!' => Not,
        '<' => Bin(Lt),
        '>' => Bin(Gt),
        '-' => Bin(Sub),
        '&' => And,
        '|' => Or,
        '+' => Bin(Add),
        '*' => Bin(Mul),
        '%' => Bin(Percent),
        '\n' => NewLine,

        //'#' => self.preprocess(),

        _ => panic!("TokenType not found")
      };
    if self.index == previous_index {
      self.bump(1)
    }

    Token::new(tokind, &self.input[previous_index..self.index], self.index - previous_index)
  }
}

pub fn lex(input: &'static str) -> Vec<Token> {
  let mut tokens: Vec<Token> = vec![];
  let mut tokenizer = Lexer::new(input);
  while !tokenizer.is_eoi() {
    let tok = tokenizer.advance();
    if tok.kind != WhiteSpace {
      tokens.push(tok)
    }
  }

  tokens
}