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
  fn ident_out_of_spoe() {
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
  fn llvm() {
    let d = lexer::lex("extern_llvm \"file.ll\"");
    parser::parse(d, "test6");
  }
}

// TODO(ghostway): codegen tests
