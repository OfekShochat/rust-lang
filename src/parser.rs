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
  AstVarDec(VarDec),
  AstRawLLVM(RawLLVM),
  AstIf(IfStatement),
  AstVarSet(VarSet)
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

pub struct IfStatement {
  expr: Vec<AstTree>,
  body: Vec<AstTree>
}

pub struct RawLLVM {
  filename: &'static [u8]
}

pub struct VarSet {
  name: &'static [u8],
  value: Vec<AstTree>
}

pub struct VarDec {
  pub typ: Types,
  pub name: &'static [u8],
  val: Vec<AstTree>,
}

pub struct Number {
  typ: Types,
  pub val: &'static [u8]
}

pub struct CurleyScope {
  pub body: Vec<AstTree>
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
  pub args: Vec<Param>,
  pub body: Vec<AstTree>,
  pub returns: Types
}

struct Scope {
  subscope: Vec<Scope>,
  names: Vec<&'static [u8]>,
  types: Vec<Types>
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
      self.subscope.push(Scope {subscope: vec![], names: vec![], types: vec![]})
    } else {
      self.subscope[0].new_scope()
    }
  }

  pub fn subscope_empty(&self) -> bool {
    self.subscope.len() == 0
  }

  pub fn add(&mut self, name: &'static [u8], typ: Types) {
    if self.subscope_empty() {
      self.names.push(name);
      self.types.push(typ)
    } else {
      self.subscope[0].add(name, typ)
    }
  }

  fn sub_subscope_empty(&mut self) -> bool {
    self.subscope[0].subscope_empty()
  }

  pub fn destruct(&mut self) {
    if self.sub_subscope_empty() {
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
    Bin(GEq) => 10,
    Bin(LEq) => 10,
    Bin(NotEq) => 10,
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

  // debug info
  line_num: usize,
  filename: &'static str
}

impl Parser {
  pub fn new(input_tokens: Vec<Token>, filename: &'static str) -> Parser {
    Parser {input: input_tokens, scope: Scope {subscope: vec![], names: vec![], types: vec![]}, index: 0, line_num: 0, filename: filename}
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
          let typ = self.first().keyword();
          params.push(Param {name: self.second().val, t: typ});
          self.scope.add(self.second().val, tokentype_to_type(typ));
          self.bump()
        },
        (Comma, I32Type | I64Type | F32Type) => {
          self.bump();
          self.bump()
        },
        _ => {
          eprintln!("{}'{}' is not a type or '{}' is not an identifier.", self.print_line(), from_utf8(self.first().val).unwrap(), from_utf8(self.second().val).unwrap());
          panic!();
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
      eprintln!("{}{} is not yet defined.", self.print_line(), from_utf8(recall_name).unwrap());
      panic!()
    }
    if self.first().kind != OpenParen {
      return AstTree::AstVarCall(VarCall{name: recall_name})
    }
    self.bump(); // eat '('
    let mut params = vec![];
    while self.first().kind != CloseParen {
      params.push(self.parse_expression(false, false));
    }
    self.bump(); // eat ')'
    AstTree::AstFuncCall(FuncCall{ name: &recall_name, args: params })
  }

  fn parse_rhs(&mut self, mut lhs: AstTree, this_prec: i8, is_if_statement: bool) -> AstTree {
    if self.is_eoi() {
      eprintln!("{}EOI encountered before end_of_expression indicator (;, \\n).", self.print_line());
      panic!()
    }
    if self.first().kind == NewLine || self.first().kind == Semi || (is_if_statement && self.first().kind == OpenBrace) {
      // there is only one number until end of the expression.
      return lhs
    }
    loop {
      if self.is_eoi() || self.first().kind == NewLine || self.first().kind == Semi {
        // self.bump();
        return lhs
      } else if is_if_statement && self.first().kind == OpenBrace {
        return lhs
      }

      let op = self.first().kind;
      let prec = get_precedence(op);
      if prec < this_prec {
        return lhs
      }
      self.bump(); // eat op

      if self.is_eoi() || self.first().kind == NewLine || self.first().kind == Semi { // they forgot a end_of_expression indicator
        eprintln!("{}expected an identifier or a number. Instead got {}.", self.print_line(), self.first().kind);
        panic!()
      }

      let mut rhs = self.parse_expression(true, is_if_statement);
      let next_prec = get_precedence(self.first().kind);
      if prec < next_prec {
        rhs = self.parse_rhs(rhs, prec + 1, is_if_statement)
      }
      lhs = AstTree::AstBin(BinExpresion {params: vec![lhs, rhs], inst: op})
    }
  }

  fn assignment(&mut self) -> AstTree {
    let n = self.first().val;
    self.scope.is_in_scope(n); // TODO(ghostway):
                                    // Should ensure that the type of the expression is correct.
                                    // That can be achieved by returning the type of the expression
                                    // from `self.parse_expression()`.
    self.bump(); // eat ident
    self.bump(); // eat '='

    AstTree::AstVarSet(VarSet{name: n, value: vec![self.parse_expression(false, false)]})
  }

  fn binary(&mut self, is_if_statement: bool) -> AstTree {
    let lhs = self.parse_expression(true, is_if_statement);
    self.parse_rhs(lhs, 0, is_if_statement)
  }

  fn parse_expression(&mut self, in_binary: bool, is_if_statement: bool) -> AstTree {
    match self.first().kind {
      Ident => {
        if in_binary {
          self.function_variable_recall()
        } else {
          let b = self.binary(is_if_statement);
          self.bump();
          b
        }
      },
      Lit(IntLiteral) | Lit(FloatLiteral) => {
        if in_binary {
          let n = AstTree::Num(Number{typ: litkind_to_type(self.first().kind), val: self.first().val});
          self.bump();
          n
        } else {
          let b = self.binary(is_if_statement);
          self.bump();
          b
        }
      },
      _ => {
        eprintln!("{}didn't find expression with {}.", self.print_line(), self.first().kind);
        panic!()
      }
    }
  }

  fn parse_function(&mut self) -> AstTree {
    let t = self.first().keyword(); // already verified in `self.advance()`
    let name = self.second().val;
    if self.second().kind != Ident {
      eprintln!("{}{} is {}, not an identifier.", self.print_line(), from_utf8(name).unwrap(), self.second().kind);
      panic!()
    }
    if self.second().keyword() != Fail {
      eprintln!("{}You should not use the language keywords for function names", self.print_line());
      panic!()
    }
    self.bump();
    self.bump();
    self.scope.new_scope();
    let parameters = self.parse_function_params();
    self.bump();

    #[cfg(debug_assertions)] {
      println!("function type: {}", t);
      println!("function name: {}", from_utf8(name).unwrap());
      for p in 0..parameters.len() {
        println!("param {}:\ntype: {}\nname: {}\n", p, parameters[p].t, from_utf8(parameters[p].name).unwrap());
      }
    }

    self.scope.add(name, tokentype_to_type(t)); // recursion
    let body = self.parse_scope(true);
    AstTree::AstFuncDec(FunctionDec {name: name, args: parameters, body: vec![body], returns: tokentype_to_type(t)})
  }

  fn parse_var(&mut self) -> AstTree {
    let t = self.first().keyword();
    self.bump(); // eat type

    let name = self.first().val;
    if from_utf8(name).unwrap() == "main" {
      eprintln!("{}don't use 'main' as a variable name. it is the entrypoint to the program.", self.print_line());
      panic!()
    } 

    if self.first().kind != Ident {
      eprintln!("{}{} is {}, not an identifier.", self.print_line(), from_utf8(name).unwrap(), self.second().kind);
      panic!()
    }
    self.bump(); // eat name
    self.bump(); // eat '='

    let val = self.parse_expression(false, false);

    self.scope.add(name, tokentype_to_type(t));
    AstTree::AstVarDec(VarDec {typ: tokentype_to_type(t), name: name, val: vec![val]})
  }

  fn function_variable_dec(&mut self) -> AstTree {
    if self.input[self.index + 2].kind == OpenParen {
      self.parse_function()
    } else if self.input[self.index + 2].kind == Eq {
      self.parse_var()
    } else {
      eprintln!("{}Syntax Error: unexpected {} was encountered after identifier declaration (expected ';', '\\n', '=' or '(').", self.print_line(), self.input[self.index + 2].kind);
      panic!()
    }
  }

  fn if_statement(&mut self) -> AstTree {
    let expr = self.parse_expression(false, true);
    self.index -= 1; // parse_expression is also eating '{'
    self.scope.new_scope();
    let body = self.parse_scope(false);

    AstTree::AstIf(IfStatement{expr: vec![expr], body: vec![body]})
  }

  fn raw_llvm(&mut self) -> AstTree {
    let fname = self.first().val;
    self.bump(); // eat filename
    AstTree::AstRawLLVM(RawLLVM {filename: fname})
  }

  fn keyword_expr(&mut self, in_function: bool) -> AstTree {
    match self.first().keyword() {
      Return => {
        if in_function {
          self.bump();
          self.parse_expression(false, false)
        } else {
          eprintln!("return without a scope to return from");
          panic!()
        }
      },
      LLVM => {
        self.bump();
        self.raw_llvm()
      },
      If => {
        self.bump();
        self.if_statement()
      },
      _ => {
        eprintln!("this should never happen.");
        panic!()
      }
    }
  }

  fn parse_scope(&mut self, in_function: bool) -> AstTree {
    let mut nodes = vec![];
    self.bump(); // eat '{'
    while !self.is_eoi() && self.first().kind != CloseBrace {
      let n = self.advance(in_function);
      if !n.is_none() {
        nodes.push(n.unwrap())
      }
    }
    if self.is_eoi() {
      eprintln!("{}EOI was encountered before scope was closed.", self.print_line());
      panic!()
    }
    self.bump(); // eat '}'
    self.scope.destruct(); // destruct the scope
    AstTree::AstScope(CurleyScope{body: nodes})
  }

  fn bump_line(&mut self) {
    self.line_num += 1
  }

  fn print_line(&self) -> String {
    format!("{}:{}: ", self.filename, self.line_num)
  }

  pub fn is_eoi(&self) -> bool {
    self.index >= self.input.len()
  }

  pub fn advance(&mut self, in_function: bool) -> Option<AstTree> {
    match self.first().kind {
      _ if is_type(self.first().keyword()) => Some(self.function_variable_dec()),
      _ if self.first().keyword() != Fail => Some(self.keyword_expr(in_function)),
      _ if self.first().kind == Ident && self.second().kind == Eq => Some(self.assignment()),
      NewLine => {
        self.bump_line();
        self.bump();
        None
      },
      Semi => {
        self.bump();
        None
      }
      Comment => {
        self.bump();
        None
      },
      OpenBrace => {
        self.scope.new_scope();
        Some(self.parse_scope(in_function))
      },
      Lit(IntLiteral) | Lit(FloatLiteral) | Ident => Some(self.parse_expression(false, false)),
      _ => {
        eprintln!("{}unexpected token {}", self.print_line(), self.first().kind);
        panic!()
      }
    }
  }
}

pub fn parse(input: Vec<Token>, filename: &'static str) -> Vec<AstTree> {
  let mut nodes = vec![];
  let mut parser = Parser::new(input, filename);
  while !parser.is_eoi() {
    let n = parser.advance(false);
    if n.is_none() {
      continue
    } else {
      nodes.push(n.unwrap())
    }
  }
  nodes
}
