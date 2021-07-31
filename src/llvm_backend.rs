extern crate llvm_sys;

fn initialise_llvm() {
  unsafe {
    if target::LLVM_InitializeNativeTarget() != 0 {
      panic!("Could not initialise target");
    }
    if target::LLVM_InitializeNativeAsmPrinter() != 0 {
      panic!("Could not initialise ASM Printer");
    }
  }
}