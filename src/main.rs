pub mod token_kinds;
pub mod lexer;
pub mod parser;

use std::{str::from_utf8, time::Instant};

use crate::parser::{Parser, AstTree};

fn main() {
  let now = Instant::now();
  let d = lexer::lex("i32 main(f32 poop, i64 length) {}");
  let mut p = Parser::new(d);
  let a = p.function_variable_dec();
  if let AstTree::AstFuncDec(a) = a {
    println!("{}", from_utf8(a.name).unwrap());
    println!("{}", a.returns)
  }
  let elapsed = now.elapsed();
  println!("elapsed: {:.2?}", elapsed);
}