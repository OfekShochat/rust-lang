use lexer::Token;

use crate::token_kinds::{BinOp::*, TokenKind::*, Literal::*};
use crate::token_kinds::TokenKind;
use std::fmt;
use std::process::exit;
use std::{process, str::from_utf8};

pub enum AstTree {
  AstFuncDec(FunctionDec),
  AstBin(BinExpresion),
  AstFuncCall(FuncCall),
  AstVarCall(VarCall),
  Type(Types)
}

#[derive(Debug)]
pub enum Types {
  Int64,
  Int32,
  F32,
  Fail
}

impl fmt::Display for Types {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", self)
    // or, alternatively:
    // fmt::Debug::fmt(self, f)
}
}

pub struct VarCall {
  name: &'static [u8]
}

pub struct FuncCall {
  name: &'static [u8],
  args: Vec<AstTree>
}

pub struct BinExpresion {
  lhs: &'static AstTree,
  rhs: &'static AstTree
}

pub struct FunctionDec {
  pub name: &'static [u8],
  args: Vec<Param>,
  body: Vec<AstTree>,
  pub returns: Types
}

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
  I32Type == kind || I64Type == kind || F32Type == kind
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
    let mut params = vec![];
    if self.first().kind != OpenParen {
      panic!("Syntax Error: Didn't find '(' after function name");
    }
    self.bump();
    while self.first().kind != CloseParen {
      if self.first().kind == Comma {
        self.bump()
      }
      match (self.first().keyword(), self.second().kind) {
        (I32Type | I64Type | F32Type, Ident) => {
          params.push(Param {name: self.second().val, t: self.first().keyword()});
          self.bump()
        },
        (Comma, I32Type | I64Type | F32Type) => {
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

  fn get_type_from_tokenkind(kind: TokenKind) -> Types {
    match kind {
      I64Type => Types::Int64,
      I32Type => Types::Int32,
      F32Type => Types::F32,
      _ => {
        eprintln!("didn't find type from {}", kind);
        Types::Fail
      }
    }
  }

  fn parse_function(&mut self) -> AstTree {
    let t = self.first().keyword();
    if !is_type(t) {
      eprintln!("'{}' is not a type.", from_utf8(self.first().val).unwrap());
      process::exit(1)
    }
    let name = self.second().val;
    if self.second().kind != Ident {
      eprintln!("{} is {}, not an identifier.", from_utf8(name).unwrap(), self.second().kind);
      process::exit(1)
    }
    if self.second().keyword() != Fail {
      eprintln!("You should not use the language keywords for function names");
      process::exit(1)
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
    AstTree::AstFuncDec(FunctionDec {name: name, args: parameters, body: body, returns: Parser::get_type_from_tokenkind(t)})
  }

  fn function_variable_recall(&mut self) -> AstTree {
    let name = self.first().val;
    if !self.scope.is_in_scope(name) {
      eprintln!("{} is not defined.", from_utf8(self.first().val).unwrap());
      process::exit(1)
    }
    if self.second().kind != OpenParen {
      return AstTree::AstVarCall(VarCall{name: name})
    }
    self.bump(); // eat '('
    let mut params = vec![];
    while self.first().kind != CloseParen {
      params.push(self.parse_expression())
    }
    AstTree::AstFuncCall(FuncCall{ name: &name, args: params })
  }

  fn parse_rhs(&mut self) -> AstTree {
    let op = self.first();
    if op.kind == Bin(Add) || op.kind ==  Bin(Sub) ||
       op.kind ==  Bin(Mul) || op.kind ==  Bin(Div) ||
       op.kind ==  Bin(Percent) {
      eprintln!("expected a binary operator");
      process::exit(1)
    }
    self.bump();
    self.parse_expression()
  }

  fn binary(&mut self) -> AstTree {
    let lhs = self.parse_expression();
    self.parse_rhs()
  }

  pub fn parse_expression(&mut self) -> AstTree {
    match self.first().kind {
      Ident => self.function_variable_recall(),
      Lit(IntLiteral) | Lit(FloatLiteral) => self.binary(),
      _ => {
        eprintln!("didn't find expression.");
        process::exit(1)
      }
    }
  }

  pub fn function_variable_dec(&mut self) -> AstTree {
    if self.input[self.index + 2].kind == OpenParen {
      self.parse_function()
    } else if self.input[self.index + 2].kind == Eq {
      // variable dec
      todo!()
    } else {
      println!("syntax error, unexpected {} was encountered.", self.input[self.index + 1].kind);
      exit(1)
    }
  }

  pub fn is_eoi(&self) -> bool {
    self.index >= self.input.len()
  }

  pub fn advance(&mut self) -> AstTree {
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

pub fn parse(input: Vec<Token>) -> Vec<AstTree> {
  let mut nodes = vec![];
  let mut parser = Parser::new(input);
  while !parser.is_eoi() {
    nodes.push(parser.advance());
  }
  nodes
}