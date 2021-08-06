#[cfg(test)]
mod parser_tests {
  mod binary {
    use lexer;
    use parser;

    #[test]
    fn multi_number_expression() {
      let d = lexer::lex("1 + 2 * 3 / 3;");
      parser::parse(d, "a");
    }

    #[test]
    #[should_panic]
    fn multi_number_expression_eoi() {
      let d = lexer::lex("1 + 2 * 3 / 3");
      parser::parse(d, "b");
    }
  }

  mod out_of_scope {
    use lexer;
    use parser;

    #[test]
    #[should_panic]
    fn return_outside_of_scope() {
      let d = lexer::lex("return 1;");
      parser::parse(d, "a");
    }

    #[test]
    #[should_panic]
    fn ident_out_of_scpoe() {
      let d = lexer::lex("a");
      parser::parse(d, "b");
    }
  }

  mod declarations {
    use lexer;
    use parser;

    #[test]
    fn function_dec() {
      let d = lexer::lex("i32 main(i32 a, i32 b) {return a+b;}");
      parser::parse(d, "a");
    }

    #[test]
    fn var_dec() {
      let d = lexer::lex("i32 a = 1 + 2;");
      parser::parse(d, "b");
    }

    #[test]
    fn var_dec_assign() {
      let d = lexer::lex("i32 a = 1 + 2; a = 1;");
      parser::parse(d, "c");
    }

    #[test]
    fn constant_variable() {
      let d = lexer::lex("const i32 a = 1 + 2;");
      parser::parse(d, "d");
    }

    #[test]
    #[should_panic]
    fn constant_variable_assign() {
      let d = lexer::lex("const i32 a = 1 + 2; a = 1;");
      parser::parse(d, "e");
    }
  }

  mod statements {
    use lexer;
    use parser;

    #[test]
    #[should_panic]
    fn return_in_if_statement() {
      let d = lexer::lex("if 1 > 2 {return 1;}");
      parser::parse(d, "a");
    }

    #[test]
    fn if_statement() {
      let d = lexer::lex("if 1 > 2 {\ni32 d = 0\n}");
      parser::parse(d, "b");
    }

    #[test]
    fn if_true_false_statement() {
      let d = lexer::lex("if false {\ni32 d = 0\n}");
      parser::parse(d, "c-1");
      let d = lexer::lex("if true {\ni32 d = 0\n}");
      parser::parse(d, "c-2");
    }

    #[test]
    fn ifelse_statement() {
      let d = lexer::lex("if 1 > 2 {\ni32 d = 0\n} else {i32 a = 1;}");
      parser::parse(d, "d");
    }
  }

  mod scoped {
    use lexer;
    use parser;

    #[test]
    fn scope() {
      let d = lexer::lex("{i32 a = 0;}");
      parser::parse(d, "a");
    }

    #[test]
    #[should_panic]
    fn unclosed_scope() {
      let d = lexer::lex("{i32 a = 0;");
      parser::parse(d, "b");
    }

    #[test]
    fn for_loop() {
      let d = lexer::lex("for i32 poop = 1; poop > 4; poop += 1; {}");
      parser::parse(d, "c");
    }

    #[test]
    fn while_loop() {
      let d = lexer::lex("i32 poop = 0;while poop < 3 { poop += 1; }");
      parser::parse(d, "d");
    }
  }

  mod general {
    use lexer;
    use parser;

    #[test]
    #[should_panic]
    fn string() {
      let d = lexer::lex("\"test-test-123\"");
      parser::parse(d, "a");
    }

    #[test]
    fn expr_with_block_comment() {
      let d = lexer::lex("{i32 a = 0; /* comment comment */}");
      parser::parse(d, "b");
    }

    #[test]
    fn extern_llvm() {
      let d = lexer::lex("extern_llvm \"file.ll\"");
      parser::parse(d, "c");
    }
  }
}

// TODO(ghostway): codegen tests
