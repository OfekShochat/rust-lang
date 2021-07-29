use std::fmt;

#[derive(Debug, PartialEq, Clone, Copy)]

pub enum Literal {
  IntLiteral,
  FloatLiteral,
  CharLiteral,
  StringLiteral
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOp {
  Add, // '+'
  Sub, // '-'
  Mul, // '*'
  Div, // '/'
  Percent, // '%'
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
  /* - Expresion operators - */
  Eq, // '='
  DEq, // '=='
  Gt, // '>' (Greater Than)
  Lt, // '<' (Less Than)
  LEq, // '<='
  GEq, // '>='
  NotEq, // '!='
  AndAnd, // '&&'
  OrOr, // '||'
  Not, // '!'
  Tilde, // '~' (Wiggle)
  Bin(BinOp),
  And, // '&'
  Or, // '|'
  // Equal-Binary operators Example: '+='
  AddEq, // '+='
  SubEq, // '-='
  MulEq, // '*='
  DivEq, // '/='
  // *(Maybe Add) AndEq, // '&='
  // *(Maybe Add) OrEq, // '|='
  

  /* - Features - */ // TODO(ghostway): find better name for this comment
  Dot, // '.' (Period, Full-Stop)
  DotDot, // '..' (Like python's Range)
  Comma, // ','
  Semi, // ';'
  Colon, // ':'
  ColonColon, // '::'
  RArrow, // '->'
  FatRArrow, // '=>'
  Quote, // "'"

  // delims
  OpenParen, // '('
  CloseParen, // ')'
  OpenBrace, // '{'
  CloseBrace, // '}'
  OpenBracket, // '['
  CloseBracket, // ']
  EmptyDelim, // null

  // Keywords
  While,
  As,
  Break,
  Continue,
  If,
  Else,
  True,
  False,
  For,
  In,
  Switch,
  Const,
  Return,

  Name,
  Comment,
  NewLine, // '\n'

  /* -- Literals -- */
  Lit(Literal),
  WhiteSpace,
  Ident,

  /* - Types - */
  I32Type,
  I64Type,
  F32Type,

  Fail
}

impl fmt::Display for TokenKind {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      write!(f, "{:?}", self)
      // or, alternatively:
      // fmt::Debug::fmt(self, f)
  }
}