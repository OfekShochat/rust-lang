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
    #[should_panic(expected = "index out of bounds: the len is 7 but the index is 7")]
    fn multi_number_expression_eoi() {
      let d = lexer::lex("1 + 2 * 3 / 3");
      parser::parse(d, "b");
    }
  }

  mod out_of_scope {
    use lexer;
    use parser;

    #[test]
    #[should_panic(expected = "19")]
    fn return_outside_of_scope() {
      let d = lexer::lex("return 1;");
      parser::parse(d, "a");
    }

    #[test]
    #[should_panic(expected = "2")]
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
    #[should_panic(expected = "7")]
    fn non_declared_attribute() {
      let d = lexer::lex("struct a {}; a.g;");
      parser::parse(d, "d");
    }

    #[test]
    fn constant_variable() {
      let d = lexer::lex("const i32 a = 1 + 2;");
      parser::parse(d, "e");
    }

    #[test]
    #[should_panic(expected = "5")]
    fn constant_variable_assign() {
      let d = lexer::lex("const i32 a = 1 + 2; a = 1;");
      parser::parse(d, "f");
    }

    #[test]
    fn struct_def() {
      let d = lexer::lex("struct poop {i32 a, i32 b}");
      parser::parse(d, "g");
    }

    #[test]
    #[should_panic(expected = "22")]
    fn function_dec_without_openbrace() {
      let d = lexer::lex("i32 main(i32 a, i32 b) return a+b;}");
      parser::parse(d, "h");
    }
  }

  mod statements {
    use lexer;
    use parser;

    #[test]
    #[should_panic(expected = "19")]
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

    #[test]
    fn multi_case_switch_statement() {
      let d = lexer::lex("match 324 {4 => {}, 5 => {}, 7 => {}}");
      parser::parse(d, "e");
    }

    #[test]
    #[should_panic(expected = "20")]
    fn misplaced_continue_in_switch() {
      let d = lexer::lex("match 324 {4 => {}, 5 => {}, 7 => {continue;}}");
      parser::parse(d, "e");
    }

    #[test]
    fn switch_statement() {
      let d = lexer::lex("match 324 {4 => {}}");
      parser::parse(d, "e");
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
    #[should_panic(expected = "23")]
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
    #[should_panic(expected = "25")] // a:1:1: unexpected token Lit(StringLiteral)
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
