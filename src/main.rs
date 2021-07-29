pub mod token_kinds;
pub mod lexer;
pub mod parser;

use std::time::Instant;

use crate::parser::Parser;

fn main() {
  let now = Instant::now();
  let d = lexer::lex("f32 main(f32 poop, i64 length) {}");
  let mut p = Parser::new(d);
  p.function_variable_dec(); //.unwrap_or_else(direct_errors);
  let elapsed = now.elapsed();
  println!("elapsed: {:.2?}", elapsed);
}