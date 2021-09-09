use llvm_sys::{core::*, target};

pub fn initialise_llvm() {
  unsafe {
    if target::LLVM_InitializeNativeTarget() != 0 {
      panic!("Could not initialise target");
    }
    if target::LLVM_InitializeNativeAsmPrinter() != 0 {
      panic!("Could not initialise ASM Printer");
    }
    let context = LLVMContextCreate();
    let module = LLVMModuleCreateWithNameInContext(b"main".as_ptr() as *const _, context);
    let builder = LLVMCreateBuilderInContext(context);
    let i64t = LLVMInt64TypeInContext(context);
    let mut args = [i64t, i64t];
    let function_type = LLVMFunctionType(i64t, args.as_mut_ptr(), args.len() as u32, 0);
    let function = LLVMAddFunction(module, b"main\0".as_ptr() as *const _, function_type);
    let bb = LLVMAppendBasicBlockInContext(context, function, b"entry\0".as_ptr() as *const _);

    LLVMPositionBuilderAtEnd(builder, bb);

    let x = LLVMGetParam(function, 0);
    let y = LLVMGetParam(function, 1);
    let sum = LLVMBuildAdd(builder, x, y, b"hey\0".as_ptr() as *const _);

    LLVMBuildRet(builder, sum);

    LLVMDisposeBuilder(builder);

    LLVMDumpModule(module);
  }
}