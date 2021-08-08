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
  AstIfElse(IfElseStatement),
  AstVarSet(VarSet),
  AstForLoop(ForLoop),
  AstKeyword(Keyword),
  AstWhileLoop(WhileLoop),
  AstSwitch(SwitchStatement),
  AstString(StringLit),
  AstCase(SwitchCase),
  AstStructDef(StructDef)
}

#[derive(Debug, Clone, Copy)]
pub enum Types {
  Int64,
  Int32,
  F32,
  StructType,
  Fail
}

impl fmt::Display for Types {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", self)
  }
}

pub struct Keyword {
  kind: TokenKind
}

pub struct IfStatement {
  expr: Vec<AstTree>,
  body: Vec<AstTree>
}

pub struct StringLit {
  pub val: &'static [u8]
}

pub struct IfElseStatement {
  expr: Vec<AstTree>,
  ifbody: Vec<AstTree>,
  elsebody: Vec<AstTree>
}

pub struct SwitchCase {
  expr: Vec<AstTree>,
  body: Vec<AstTree>
}

pub struct ForLoop {
  for_info: Vec<AstTree>,
  body: Vec<AstTree>
}

pub struct WhileLoop {
  condition: Vec<AstTree>,
  body: Vec<AstTree>
}

pub struct StructDef {
  params: Vec<Param>,
  name: &'static [u8]
}

pub struct RawLLVM {
  filename: &'static [u8]
}

pub struct SwitchStatement {
  expr: Vec<AstTree>,
  cases: Vec<AstTree>
}

pub struct VarSet {
  name: &'static [u8],
  value: Vec<AstTree>
}

pub struct VarDec {
  pub typ: Types,
  pub name: &'static [u8],
  val: Vec<AstTree>,
  constant: bool
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
  types: Vec<Types>,
  constants: Vec<bool>
}

impl Scope {
  pub fn is_in_scope(&self, name: &'static [u8]) -> bool {
    if !self.subscope_empty() && self.subscope[0].is_in_scope(name) {
      return true
    }
    self.names.contains(&name)
  }

  pub fn get(&self, name: &'static [u8]) -> (Types, bool) {
    if !self.subscope_empty() {
      return self.subscope[0].get(name)
    }
    let index = self.names.iter().position(|&r| r == name).unwrap();
    (self.types[index], self.constants[index])
  }

  pub fn new_scope(&mut self) {
    if self.subscope_empty() {
      self.subscope.push(Scope {subscope: vec![], names: vec![], types: vec![], constants: vec![]})
    } else {
      self.subscope[0].new_scope()
    }
  }

  pub fn subscope_empty(&self) -> bool {
    self.subscope.len() == 0
  }

  pub fn add(&mut self, name: &'static [u8], typ: Types, constant: bool) {
    if self.subscope_empty() {
      self.names.push(name);
      self.types.push(typ);
      self.constants.push(constant)
    } else {
      self.subscope[0].add(name, typ, constant)
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
    Bin(DEq) => 10,
    AddEq => 10,
    SubEq => 10,
    MulEq => 10,
    DivEq => 10,
    Bin(NotEq) => 10,
    Bin(As) => 10,
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
  line_index: usize,
  filename: &'static str
}

impl Parser {
  pub fn new(input_tokens: Vec<Token>, filename: &'static str) -> Parser {
    Parser {input: input_tokens, scope: Scope {subscope: vec![], names: vec![], types: vec![], constants: vec![]}, index: 0, line_num: 0, line_index: 0, filename: filename}
  }

  fn first(&self) -> Token {
    self.input[self.index]
  }

  fn second(&self) -> Token {
    self.input[self.index + 1]
  }

  fn bump(&mut self) {
    self.line_index += self.first().length;
    self.index += 1;
  }

  fn parse_params(&mut self, stop: TokenKind) -> Vec<Param> {
    let mut params = vec![];
    self.bump();
    while self.first().kind != stop {
      if self.first().kind == Comma {
        self.bump()
      }
      match (self.first().keyword(), self.second().kind) {
        (I32Type | I64Type | F32Type, Ident) => {
          let typ = self.first().keyword();
          params.push(Param {name: self.second().val, t: typ});
          self.scope.add(self.second().val, tokentype_to_type(typ), false);
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
    self.bump(); // skip stop token
    params
  }

  fn parse_function_params(&mut self) -> Vec<Param> {
    if self.first().kind != OpenParen {
      eprintln!("{}expected '(' after function name. got {}", self.print_line(), self.first().kind);
      panic!()
    }
    self.parse_params(CloseParen)
  }

  fn function_variable_recall(&mut self) -> AstTree {
    let k = self.first().keyword();
    if k == True || k == False {
      self.bump();
      return AstTree::AstKeyword(Keyword {kind: self.first().keyword()})
    }
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

  fn parse_rhs(&mut self, mut lhs: AstTree, this_prec: i8, is_before_scope: bool) -> AstTree {
    if self.is_eoi() {
      eprintln!("{}EOI encountered before end_of_expression indicator (;, \\n).", self.print_line());
      panic!()
    }
    if self.first().kind == NewLine || self.first().kind == Semi || (is_before_scope && self.first().kind == OpenBrace) {
      // there is only one number until end of the expression.
      return lhs
    }

    loop {
      if self.is_eoi() || self.first().kind == NewLine || self.first().kind == Semi {
        return lhs
      } else if is_before_scope && self.first().kind == OpenBrace {
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

      let mut rhs = self.parse_expression(true, is_before_scope);
      let next_prec = get_precedence(self.first().kind);
      if prec < next_prec {
        rhs = self.parse_rhs(rhs, prec + 1, is_before_scope)
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
    let info = self.scope.get(n);
    if info.1 {
      eprintln!("{}{} is a constant and cannot be assigned to.", self.print_line(), from_utf8(n).unwrap());
      panic!()
    }
    self.bump(); // eat ident
    self.bump(); // eat '='

    AstTree::AstVarSet(VarSet{name: n, value: vec![self.parse_expression(false, false)]})
  }

  fn binary(&mut self, is_before_scope: bool) -> AstTree {
    let lhs = self.parse_expression(true, is_before_scope);
    self.parse_rhs(lhs, 0, is_before_scope)
  }

  fn parse_expression(&mut self, in_binary: bool, is_before_scope: bool) -> AstTree {
    match self.first().kind {
      Ident => {
        if in_binary {
          self.function_variable_recall()
        } else {
          let b = self.binary(is_before_scope);
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
          let b = self.binary(is_before_scope);
          self.bump();
          b
        }
      },
      Lit(StringLiteral) => {
        let s = AstTree::AstString(StringLit {val: self.first().val});
        self.bump();
        s
      },
      NewLine => {
        self.bump();
        self.bump_line();
        self.parse_expression(in_binary, is_before_scope)
      }
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

    #[cfg(debug_assertions)] {
      println!("function type: {}", t);
      println!("function name: {}", from_utf8(name).unwrap());
      for p in 0..parameters.len() {
        println!("param {}:\ntype: {}\nname: {}\n", p, parameters[p].t, from_utf8(parameters[p].name).unwrap());
      }
    }

    self.scope.add(name, tokentype_to_type(t), true); // recursion
    let body = self.parse_scope(true, false);
    AstTree::AstFuncDec(FunctionDec {name: name, args: parameters, body: vec![body], returns: tokentype_to_type(t)})
  }

  fn parse_var(&mut self, constant: bool) -> AstTree {
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

    self.scope.add(name, tokentype_to_type(t), constant);
    AstTree::AstVarDec(VarDec {typ: tokentype_to_type(t), name: name, val: vec![val], constant: constant})
  }

  fn function_variable_dec(&mut self) -> AstTree {
    if self.input[self.index + 2].kind == OpenParen {
      self.parse_function()
    } else if self.input[self.index + 2].kind == Eq {
      self.parse_var(false)
    } else {
      eprintln!("{}Syntax Error: unexpected {} was encountered after identifier declaration (expected ';', '\\n', '=' or '(').", self.print_line(), self.input[self.index + 2].kind);
      panic!()
    }
  }

  fn if_statement(&mut self, in_loop: bool) -> AstTree {
    let expr = self.parse_expression(false, true);
    self.index -= 1; // parse_expression is also eating '{'
    self.scope.new_scope();
    let body = self.parse_scope(false, in_loop);

    if !self.is_eoi() && self.first().keyword() == Else {
      self.scope.new_scope();
      self.bump();
      let elsebody = self.parse_scope(false, in_loop);
      AstTree::AstIfElse(IfElseStatement{expr: vec![expr], ifbody: vec![body], elsebody: vec![elsebody]})
    } else {
      AstTree::AstIf(IfStatement{expr: vec![expr], body: vec![body]})
    }
  }

  fn raw_llvm(&mut self) -> AstTree {
    let fname = self.first().val;
    self.bump(); // eat filename
    AstTree::AstRawLLVM(RawLLVM {filename: fname})
  }

  fn for_loop(&mut self) -> AstTree {
    let initializer = self.parse_var(false);
    let condition = self.parse_expression(false, false);
    let after = self.parse_expression(false, false);
    self.scope.new_scope();
    AstTree::AstForLoop(ForLoop {for_info: vec![initializer, condition, after], body: vec![self.parse_scope(false, true)]})
  }

  fn while_loop(&mut self) -> AstTree {
    let condition = self.parse_expression(false, true);
    self.index -= 1; // parse_expression is also eating '{'
    self.scope.new_scope();
    AstTree::AstWhileLoop(WhileLoop {condition: vec![condition], body: vec![self.parse_scope(false, true)]})
  }

  fn parse_switch_body(&mut self, in_function: bool, in_loop: bool) -> Vec<AstTree> {
    let mut nodes = vec![];
    self.bump(); // eat '{'
    while !self.is_eoi() && self.first().kind != CloseBrace {
      let case = self.parse_expression(true, true);
      if self.first().kind != FatRArrow {
        eprintln!("{}expected '=>' after case. got {}", self.print_line(), self.first().kind);
        panic!()
      }

      self.bump(); // eat '=>'
      self.scope.new_scope();
      let body = self.parse_scope(in_function, in_loop);
      nodes.push(AstTree::AstCase(SwitchCase {expr: vec![case], body: vec![body]}));
      if self.first().kind == Comma {
        self.bump()
      } else if self.first().kind != CloseBrace {
        eprintln!("{}expected ',' or '}}' after case body. got {}", self.print_line(), self.first().kind);
        panic!()
      }
    }
    if self.is_eoi() {
      eprintln!("{}EOI was encountered before scope was closed.", self.print_line());
      panic!()
    }
    self.bump(); // eat '}'
    nodes
  }

  fn switch_statement(&mut self, in_function: bool, in_loop: bool) -> AstTree {
    let expr = self.parse_expression(true, true);
    AstTree::AstSwitch(SwitchStatement {expr: vec![expr], cases: self.parse_switch_body(in_function, in_loop)})
  }

  fn parse_struct_params(&mut self) -> Vec<Param> {
    if self.first().kind != OpenBrace {
      eprintln!("{}expected '{{' after struct name. got {}", self.print_line(), self.first().kind);
      panic!()
    }
    self.parse_params(CloseBrace)
  }

  fn parse_struct(&mut self) -> AstTree {
    let name = self.first().val;
    if self.first().keyword() != Fail {
      eprintln!("{}You should not use the language keywords for struct names.", self.print_line());
      panic!()
    }
    self.bump();
    self.scope.add(name, Types::StructType, true);
    AstTree::AstStructDef(StructDef { params: self.parse_struct_params(), name: name})
  }

  fn keyword_expr(&mut self, in_function: bool, in_loop: bool) -> AstTree {
    let k = self.first().keyword();
    match k {
      Return => {
        if in_function {
          self.bump();
          self.parse_expression(false, false)
        } else {
          eprintln!("return without a scope to return from");
          panic!()
        }
      },
      While => {
        self.bump();
        self.while_loop()
      }
      LLVM => {
        self.bump();
        self.raw_llvm()
      },
      Const => {
        self.bump();
        self.parse_var(true)
      },
      If => {
        self.bump();
        self.if_statement(in_loop)
      },
      For => {
        self.bump();
        self.for_loop()
      },
      Break | Continue => {
        if !in_loop {
          eprintln!("{}{} used without a loop.", self.print_line(), self.first().keyword());
          panic!()
        }
        self.bump();
        AstTree::AstKeyword(Keyword {kind: k})
      },
      True | False => {
        self.bump();
        AstTree::AstKeyword(Keyword {kind: k})
      },
      Switch => {
        self.bump();
        self.switch_statement(in_function, in_loop)
      },
      Struct => {
        self.bump();
        self.parse_struct()
      },
      _ => {
        eprintln!("this should never happen.");
        panic!()
      }
    }
  }

  fn parse_scope(&mut self, before_scope: bool, in_loop: bool) -> AstTree {
    let mut nodes = vec![];
    self.bump(); // eat '{'
    while !self.is_eoi() && self.first().kind != CloseBrace {
      let n = self.advance(before_scope, in_loop);
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
    self.line_index = 0;
    self.line_num += 1
  }

  fn print_line(&self) -> String {
    format!("{}:{}:{}: ", self.filename, self.line_num, self.line_index)
  }

  pub fn is_eoi(&self) -> bool {
    self.index >= self.input.len()
  }

  pub fn advance(&mut self, in_function: bool, in_loop: bool) -> Option<AstTree> {
    match self.first().kind {
      _ if is_type(self.first().keyword()) => Some(self.function_variable_dec()),
      _ if self.first().keyword() != Fail => Some(self.keyword_expr(in_function, in_loop)),
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
        Some(self.parse_scope(in_function, in_loop))
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
    let n = parser.advance(false, false);
    if n.is_none() {
      continue
    } else {
      nodes.push(n.unwrap())
    }
  }
  nodes
}
