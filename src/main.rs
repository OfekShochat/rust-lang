pub mod token_kinds;
pub mod lexer;
pub mod parser;

use std::{str::from_utf8, time::Instant};

use crate::parser::AstTree;

fn main() {
  let now = Instant::now();
  let d = lexer::lex("i32 main(f32 poop, i64 length) {return 1 + 1}");
  let p = parser::parse(d);
  let elapsed = now.elapsed();
  let mut a = 0;
  for i in p {
    a+=1;
    println!("{}", a);
    if let AstTree::AstFuncDec(i) = i {
      println!("{}", from_utf8(i.name).unwrap());
      println!("{}", i.returns)
    } else if let AstTree::Num(i) = i {
      println!("{}", from_utf8(i.val).unwrap())
    } else if let AstTree::AstBin(e) = i {
      if let AstTree::Num(i) = &e .params[0] {
        println!("{}", from_utf8(i.val).unwrap())
      }
    }
  }
  println!("elapsed: {:.2?}", elapsed);
}