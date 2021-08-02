#[cfg(test)]
mod parser_tests {
  use lexer;
  use parser;
  
  #[test]
  fn multi_number_expression() {
    let d = lexer::lex("1 + 2 * 3 / 3;");
    parser::parse(d, "test1");
  }

  #[test]
  #[should_panic]
  fn multi_number_expression_fail() {
    let d = lexer::lex("1 + 2 * 3 / 3");
    parser::parse(d, "test2");
  }

  #[test]
  #[should_panic]
  fn return_outside_of_scope() {
    let d = lexer::lex("return 1;");
    parser::parse(d, "test3");
  }

  #[test]
  #[should_panic]
  fn ident_out_of_scpoe() {
    let d = lexer::lex("a");
    parser::parse(d, "test4");
  }

  #[test]
  fn var_dec() {
    let d = lexer::lex("i32 a = 1 + 2;");
    parser::parse(d, "test5");
  }

  #[test]
  fn var_dec_assign() {
    let d = lexer::lex("i32 a = 1 + 2; a = 1;");
    parser::parse(d, "test6");
  }

  #[test]
  fn extern_llvm() {
    let d = lexer::lex("extern_llvm \"file.ll\"");
    parser::parse(d, "test7");
  }

  #[test]
  fn function_dec() {
    let d = lexer::lex("i32 main(i32 a, i32 b) {return a+b;}");
    parser::parse(d, "test8");
  }

  #[test]
  #[should_panic]
  fn return_in_if_statement() {
    let d = lexer::lex("if 1 > 2 {return 1;}");
    parser::parse(d, "test9");
  }

  #[test]
  fn if_statement() {
    let d = lexer::lex("if 1 > 2 {\ni32 d = 0\n}");
    parser::parse(d, "test10");
  }

  #[test]
  fn scope() {
    let d = lexer::lex("{i32 a = 0;}");
    parser::parse(d, "test11");
  }
}

// TODO(ghostway): codegen tests
