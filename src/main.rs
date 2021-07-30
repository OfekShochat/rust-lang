pub mod token_kinds;
pub mod lexer;
pub mod parser;

use std::{str::from_utf8, time::Instant};

use parser::BinExpresion;

use crate::parser::AstTree;

fn print_binop(e: BinExpresion, depth: i8) {
  println!("depth: {}", depth);
  for a in e.params {
    if let AstTree::Num(i) = &a {
      println!("bin param: {}", from_utf8(i.val).unwrap())
    } else if let AstTree::AstBin(i) = a {
      print_binop(i, depth+1);
    }
  }
  println!("^bin inst: {}, depth: {}\n", e.inst, depth);
}

fn main() {
  let now = Instant::now();
  let d = lexer::lex("i32 main = 3;\n");
  let p = parser::parse(d);
  let elapsed = now.elapsed();
  for i in p {
    if let AstTree::AstFuncDec(i) = i {
      println!("functiondec name: {}", from_utf8(i.name).unwrap());
      println!("functiondec returns: {}", i.returns)
    } else if let AstTree::Num(i) = i {
      println!("number: {}", from_utf8(i.val).unwrap())
    } else if let AstTree::AstBin(e) = i {
      print_binop(e, 0);
    } else if let AstTree::AstVarCall(i) = i {
      println!("{}", from_utf8(i.name).unwrap())
    } else if let AstTree::AstFuncCall(i) = i {
      println!("funccall name: {}", from_utf8(i.name).unwrap())
    }
  }
  println!("elapsed: {:.2?}", elapsed);
}