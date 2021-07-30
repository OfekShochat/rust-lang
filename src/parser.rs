use lexer::Token;

use crate::token_kinds::{BinOp::*, Literal::*, TokenKind::*};
use crate::token_kinds::TokenKind;
use std::fmt;
use std::process::exit;
use std::str::from_utf8;

pub enum AstTree {
  AstFuncDec(FunctionDec),
  AstBin(BinExpresion),
  AstFuncCall(FuncCall),
  AstVarCall(VarCall),
  Type(Types),
  AstScope(CurleyScope),
  Num(Number),
  AstVarDec(VarDec)
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
  }
}

pub struct VarDec {
  typ: Types,
  name: &'static [u8],
  val: Vec<AstTree>,
}

pub struct Number {
  typ: Types,
  pub val: &'static [u8]
}

pub struct CurleyScope {
  body: Vec<AstTree>
}

pub struct VarCall {
  pub name: &'static [u8]
}

pub struct FuncCall {
  pub name: &'static [u8],
  args: Vec<AstTree>
}

pub struct BinExpresion {
  pub inst: TokenKind,
  pub params: Vec<AstTree>
}

pub struct FunctionDec {
  pub name: &'static [u8],
  args: Vec<Param>,
  body: Vec<AstTree>,
  pub returns: Types
}

struct Scope {
  subscope: Vec<Scope>,
  names: Vec<&'static [u8]>
}

impl Scope {
  pub fn is_in_scope(&self, name: &'static [u8]) -> bool {
    if !self.subscope_empty() && self.subscope[0].is_in_scope(name) {
      return true
    }
    self.names.contains(&name)
  }

  pub fn new_scope(&mut self) {
    if self.subscope_empty() {
      self.subscope.push(Scope {subscope: vec![], names: vec![]})
    } else {
      self.subscope[0].new_scope()
    }
  }

  pub fn subscope_empty(&self) -> bool {
    self.subscope.len() == 0
  }

  pub fn add(&mut self, name: &'static [u8]) {
    if self.subscope_empty() {
      self.names.push(name)
    } else {
      self.subscope[0].add(name)
    }
  }

  pub fn destruct(&mut self) {
    if !self.subscope_empty() {
      self.subscope.remove(0);
    } else {
      self.subscope[0].destruct()
    }
  }
}

pub struct Param {
  pub name: &'static [u8],
  t:    TokenKind
}

fn is_type(kind: TokenKind) -> bool {
  I32Type == kind || I64Type == kind || F32Type == kind
}

fn tokentype_to_type(kind: TokenKind) -> Types {
  match kind {
    I32Type => Types::Int32,
    I64Type => Types::Int64,
    F32Type => Types::F32,
    _ => exit(11)
  }
}

fn litkind_to_type(kind: TokenKind) -> Types {
  match kind {
    Lit(FloatLiteral) => Types::F32,
    Lit(IntLiteral) => Types::Int32,
    _ => {
      exit(12)
    }
  }
}

fn get_precedence(op: TokenKind) -> i8 {
  match op {
    Bin(Gt) => 10,
    Bin(Lt) => 10,
    Bin(Add) => 20,
    Bin(Sub) => 20,
    Bin(Mul) => 30,
    Bin(Div) => 30,
    _ => -1
  }
}

struct Parser {
  input: Vec<Token>,
  scope: Scope,
  index: usize,
}

impl Parser {
  pub fn new(input_tokens: Vec<Token>) -> Parser {
    Parser {input: input_tokens, scope: Scope {subscope: vec![], names: vec![]}, index: 0}
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
    i + self.index + 1
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
          exit(1);
        }
      }
      self.bump()
    }
    params
  }

  fn function_variable_recall(&mut self) -> AstTree {
    let recall_name = self.first().val;
    self.bump(); // eat identifier
    if !self.scope.is_in_scope(recall_name) {
      eprintln!("{} is not defined.", from_utf8(recall_name).unwrap());
      exit(1)
    }
    if self.first().kind != OpenParen {
      return AstTree::AstVarCall(VarCall{name: recall_name})
    }
    self.bump(); // eat '('
    let mut params = vec![];
    while self.first().kind != CloseParen {
      params.push(self.parse_expression(false));
    }
    self.bump(); // eat ')'
    AstTree::AstFuncCall(FuncCall{ name: &recall_name, args: params })
  }

  fn parse_rhs(&mut self, mut lhs: AstTree, this_prec: i8) -> AstTree {
    loop {
      if self.is_eoi() || self.first().kind == NewLine || self.first().kind == Semi {
        self.bump();
        return lhs
      }

      let op = self.first().kind;
      let prec = get_precedence(op);
      if prec < this_prec {
        return lhs
      }
      self.bump(); // eat op

      let mut rhs = self.parse_expression(true);
      let next_prec = get_precedence(self.first().kind);
      if prec < next_prec {
        rhs = self.parse_rhs(rhs, prec + 1)
      }
      lhs = AstTree::AstBin(BinExpresion {params: vec![lhs, rhs], inst: op})
    }
  }

  fn binary(&mut self) -> AstTree {
    let lhs = self.parse_expression(true);
    self.parse_rhs(lhs, 0)
  }

  fn parse_expression(&mut self, in_binary: bool) -> AstTree {
    match self.first().kind {
      Ident => self.function_variable_recall(),
      Lit(IntLiteral) | Lit(FloatLiteral) => {
        if in_binary {
          let n = AstTree::Num(Number{typ: litkind_to_type(self.first().kind), val: self.first().val});
          self.bump();
          n
        } else {
          let b = self.binary();
          self.bump();
          b
        }
      },
      _ => {
        eprintln!("didn't find expression with {}.", self.first().kind);
        exit(1)
      }
    }
  }

  fn parse_function(&mut self) -> AstTree {
    let t = self.first().keyword(); // already verified in `self.advance()`
    let name = self.second().val;
    if self.second().kind != Ident {
      eprintln!("{} is {}, not an identifier.", from_utf8(name).unwrap(), self.second().kind);
      exit(1)
    }
    if self.second().keyword() != Fail {
      eprintln!("You should not use the language keywords for function names");
      exit(1)
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

    let body = parse(self.input[self.index..self.get_function_end()].into());
    self.scope.add(name);
    AstTree::AstFuncDec(FunctionDec {name: name, args: parameters, body: body, returns: tokentype_to_type(t)})
  }

  fn parse_var(&mut self) -> AstTree {
    let t = self.first().keyword();
    self.bump(); // eat type

    let name = self.first().val;

    if self.first().kind != Ident {
      eprintln!("{} is {}, not an identifier.", from_utf8(name).unwrap(), self.second().kind);
      exit(1)
    }
    self.bump(); // eat name
    self.bump(); // eat '='

    let val = self.parse_expression(false);

    AstTree::AstVarDec(VarDec {typ: tokentype_to_type(t), name: name, val: vec![val]})
  }

  fn function_variable_dec(&mut self) -> AstTree {
    if self.input[self.index + 2].kind == OpenParen {
      self.parse_function()
    } else if self.input[self.index + 2].kind == Eq {
      // variable dec
      self.parse_var()
    } else {
      eprintln!("Syntax Error: unexpected {} was encountered after identifier declaration (expected ';', '\\n', '=' or '(').", self.input[self.index + 2].kind);
      exit(1)
    }
  }

  fn keyword_expr(&mut self) -> AstTree {
    match self.first().keyword() {
      Return => {
        self.bump();
        self.parse_expression(false)
      }
      _ => {
        eprintln!("this should never happen.");
        exit(-1)
      }
    }
  }

  fn parse_scope(&mut self) -> Vec<AstTree> {
    let mut nodes = vec![];
    self.bump(); // eat '{'
    while self.first().kind != CloseBrace && !self.is_eoi() {
      let n = self.advance();
      if !n.is_none() {
        nodes.push(n.unwrap())
      }
    }
    self.bump(); // eat '}'
    self.scope.destruct(); // destruct the scope
    nodes
  }

  pub fn is_eoi(&self) -> bool {
    self.index >= self.input.len()
  }

  pub fn advance(&mut self) -> Option<AstTree> {
    match self.first().kind {
      _ if is_type(self.first().keyword()) => Some(self.function_variable_dec()),
      _ if self.first().keyword() != Fail => Some(self.keyword_expr()),
      Ident => Some(self.function_variable_recall()),
      NewLine | Semi => {
        self.bump();
        None
      },
      OpenBrace => {
        self.scope.new_scope();
        Some(AstTree::AstScope(CurleyScope{body: self.parse_scope()}))
      },
      Lit(IntLiteral) | Lit(FloatLiteral) => Some(self.parse_expression(false)),
      _ => {
        eprintln!("unexpected token {}", self.first().kind);
        exit(1)
      }
    }
  }
}

pub fn parse(input: Vec<Token>) -> Vec<AstTree> {
  let mut nodes = vec![];
  let mut parser = Parser::new(input);
  while !parser.is_eoi() {
    let n = parser.advance();
    if n.is_none() {
      continue
    } else {
      nodes.push(n.unwrap())
    }
  }
  nodes
}
