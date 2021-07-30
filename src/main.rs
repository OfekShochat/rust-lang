pub mod token_kinds;
pub mod lexer;
pub mod parser;

use std::{str::from_utf8, time::Instant};

use crate::parser::AstTree;

fn main() {
  let now = Instant::now();
  let d = lexer::lex("1 + 2\n");
  let p = parser::parse(d);
  let elapsed = now.elapsed();
  for i in p {
    if let AstTree::AstFuncDec(i) = i {
      println!("functiondec name: {}", from_utf8(i.name).unwrap());
      println!("functiondec returns: {}", i.returns)
    } else if let AstTree::Num(i) = i {
      println!("number: {}", from_utf8(i.val).unwrap())
    } else if let AstTree::AstBin(e) = i {
      for a in e.params {
        if let AstTree::Num(i) = &a {
          println!("bin param: {}", from_utf8(i.val).unwrap())
        }
      }
      println!("bin inst: {}", e.inst)
    } else if let AstTree::AstVarCall(i) = i {
      println!("{}", from_utf8(i.name).unwrap())
    } else if let AstTree::AstFuncCall(i) = i {
      println!("funccall name: {}", from_utf8(i.name).unwrap())
    }
  }
  println!("elapsed: {:.2?}", elapsed);
}