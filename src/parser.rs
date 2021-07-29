use lexer::Token;

use crate::token_kinds::{BinOp::*, TokenKind::*};
use crate::token_kinds::TokenKind;
use std::{process, str::from_utf8};

pub struct Node {
  params: Vec<Node>
}

struct Scope {
  subscope: Option<&'static Scope>,
  names: Vec<&'static [u8]>
}

impl Scope {
  pub fn is_in_scope(&self, name: &'static [u8]) -> bool {
    if !self.subscope.is_none() && self.subscope.unwrap().is_in_scope(name) {
      return true
    }
    self.names.contains(&name)
  }
}

pub struct Parser {
  input: Vec<Token>,
  scope: Scope,
  index: usize,
}

pub struct Param {
  pub name: &'static [u8],
  t:    TokenKind
}

fn is_type(kind: TokenKind) -> bool {
  I32 == kind || I64 == kind || F32 == kind
}

impl Parser {
  pub fn new(input_tokens: Vec<Token>) -> Parser {
    Parser {input: input_tokens, scope: Scope {subscope: None, names: vec![]}, index: 0}
  }

  fn first(&self) -> Token {
    self.input[self.index]
  }

  fn second(&self) -> Token {
    self.input[self.index + 1]
  }

  fn bump(&mut self) {
    self.index += 1;
  }

  fn get_function_end(&self) -> usize {
    let mut i = 0;
    while self.input[i + self.index].kind != CloseBrace {
      i += 1
    }
    i + self.index
  }

  fn parse_function_params(&mut self) -> Vec<Param> {
    let mut params: Vec<Param> = vec![];
    if self.first().kind != OpenParen {
      panic!("Syntax Error: Didn't find '(' after function name");
    }
    self.bump();
    while self.first().kind != CloseParen {
      if self.first().kind == Comma {
        self.bump()
      }
      match (self.first().keyword(), self.second().kind) {
        (I32 | I64 | F32, Ident) => {
          params.push(Param {name: self.second().val, t: self.first().keyword()});
          self.bump()
        },
        (Comma, I32 | I64 | F32) => {
          self.bump();
          self.bump()
        },
        _ => {
          eprintln!("'{}' is not a type or '{}' is not an identifier.", from_utf8(self.first().val).unwrap(), from_utf8(self.second().val).unwrap());
          process::exit(1);
        }
      }
      self.bump()
    }
    params
  }

  fn parse_function(&mut self) -> Result<usize /* when error */, usize /* when Ok */ > {
    let t = self.first().keyword();
    if !is_type(t) {
      eprintln!("'{}' is not a type.", from_utf8(self.first().val).unwrap());
      return Err(self.index)
    }
    let name = self.second().val;
    if self.second().kind != Ident {
      eprintln!("{} is {}, not an identifier.", from_utf8(name).unwrap(), self.second().kind);
      return Err(self.index + 1)
    }
    if self.second().keyword() != Fail {
      eprintln!("You should not use the language keywords for function names");
      return Err(self.index + 1)
    }
    self.bump();
    self.bump();
    let parameters = self.parse_function_params();
    self.bump();

    #[cfg(debug_assertions)] {
      println!("function type: {}", t);
      println!("function name: {}", from_utf8(name).unwrap());
      for p in 0..parameters.len() {
        println!("param {}:\ntype: {}\nname: {}\n", p, parameters[p].t, from_utf8(parameters[p].name).unwrap());
      }
    }

    let body = parse(self.input[self.index+1..self.get_function_end()].into());
    Ok(0)
  }

  fn variable_recall(&mut self) {
    println!("{}", self.scope.is_in_scope(self.first().val));
  }

  fn function_variable_recall(&mut self) {
    if !self.scope.is_in_scope(self.first().val) {
      eprintln!("{} is not defined.", from_utf8(self.first().val).unwrap());
      process::exit(1)
    }
    if self.second().kind != OpenParen {
      return self.variable_recall()
    }
    self.bump(); // eat '('
    while self.first().kind != CloseParen {
      self.parse_expression()
    }
    // return function recall
  }

  fn parse_rhs(&mut self) {
    let op = self.first();
    if op.kind == Bin(Add) || op.kind ==  Bin(Sub) ||
       op.kind ==  Bin(Mul) || op.kind ==  Bin(Div) ||
       op.kind ==  Bin(Percent) {
      eprintln!("expected a binary operator");
      process::exit(1)
    }
    self.bump();
    let rhs = self.parse_expression();
  }

  fn binary(&mut self) {
    let lhs = self.parse_expression();
    self.parse_rhs()
  }

  fn parse_expression(&mut self) {
    match self.first().kind {
      Ident => self.function_variable_recall(),
      _ if is_type(self.first().kind) => self.binary(),
      _ => {
        eprintln!("didn't find expression.");
        process::exit(1)
      }
    }
  }

  pub fn function_variable_dec(&mut self) {
    if self.input[self.index + 2].kind == OpenParen {
      self.parse_function().unwrap();
    } else if self.input[self.index + 2].kind == Eq {
      // variable dec
    } else {
      println!("syntax error, unexpected {} was encountered.", self.input[self.index + 1].kind);
    }
  }

  pub fn is_eoi(&self) -> bool {
    self.index >= self.input.len()
  }

  pub fn advance(&mut self) {
    match self.first().kind {
      Ident => self.function_variable_recall(),
      NewLine => {
        self.bump();
        self.advance()
      },
      _ if is_type(self.first().kind) => self.function_variable_dec(),
      _ => {
        eprintln!("unexpected token {}", self.first().kind);
        process::exit(1)
      }
    }
  }
}

pub fn parse(input: Vec<Token>) -> Vec<Node> {
  let mut nodes: Vec<Node> = vec![];
  let mut parser = Parser::new(input);
  while !parser.is_eoi() {
    parser.advance();
  }
  nodes
}