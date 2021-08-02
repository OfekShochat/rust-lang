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
  fn function_dec() {
    let d = lexer::lex("return 1;");
    parser::parse(d, "test3");
  }
}

// TODO(ghostway): codegen tests