use crate::{AstTree, parser::*, token_kinds::{BinOp, TokenKind}};
use llvm_sys::{*, core::*, prelude::*};
use std::ptr;

pub struct Emitter {
  input: Vec<AstTree>,
  index: usize,
  context: *mut LLVMContext,
  module: *mut LLVMModule,
  builder: *mut LLVMBuilder,
}

impl Emitter {
  pub fn new(input: Vec<AstTree>) -> Emitter {
    unsafe {
      let context = LLVMContextCreate();
      let module = LLVMModuleCreateWithNameInContext(b"poop\0".as_ptr() as *const _, context);
      let builder = LLVMCreateBuilderInContext(context);
      Emitter { input, index: 0, context, module, builder }
    }
  }

  fn get(&self) -> &AstTree {
    &self.input[self.index]
  }

  fn bump(&mut self) {
    self.index += 1;
  }

  fn get_type(&self, typ: Types) -> LLVMTypeRef {
    unsafe {
      match typ {
        Types::F32 => LLVMFloatTypeInContext(self.context),
        Types::Int32 => LLVMInt32TypeInContext(self.context),
        Types::Int64 => LLVMInt64TypeInContext(self.context),
        Types::Void => LLVMVoidTypeInContext(self.context),
        _ => panic!()
      }
    }
  }

  /*fn get_bin_inst(&mut self, kind: TokenKind) -> *mut LLVMValue {
    unsafe {
      match kind {
        TokenKind::Bin(BinOp::Add) => {
          
        }
        _ => panic!()
      }
    }
  }*/

  fn emit_expression(&mut self, f: BinExpresion) {
    f.inst;
  }

  fn function(&self, f: &FunctionDec) {
    let mut args = vec![];
    for a in &f.args {
      args.push(self.get_type(tokentype_to_type(a.t)));
    }
    println!("{:?}", args);
    unsafe {
      let llfunc = LLVMAddFunction(self.module, b"poop\0".as_ptr() as *const _, LLVMFunctionType(self.get_type(f.returns), ptr::null_mut(), 0, 0));
      let bb = LLVMAppendBasicBlockInContext(
        self.context,
        llfunc,
        b"entry\0".as_ptr() as *const _,
      );
      LLVMPositionBuilderAtEnd(self.builder, bb);
      LLVMBuildRetVoid(self.builder);
      LLVMDumpModule(self.module);
    }
  }

  pub fn advance(&mut self) {
    if let AstTree::AstFuncDec(i) = self.get() {
      self.function(i)
    }
  }
}
