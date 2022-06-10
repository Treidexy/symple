mod lex;
mod parse;
mod check;
mod emit;
mod compiler;

use lex::{ Lexer, };
use parse::{ Parser, };
use check::{ Checker, };
use emit::{ Emitter, };
use compiler::{ FileId, };

use llvm_sys::core::*;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "full");

    let file_id: FileId = 0;
    let src = std::fs::read_to_string("samples/test.sy").unwrap();

    let tokens = Lexer::lex(file_id, &src);
    for token in &tokens {
        println!("{:?}", token);
    }

    println!("");

    let module_st = Parser::parse(file_id, &tokens);
    for func in &module_st.funcs {
        println!("{:?}", func);
    }

    println!("");

    let module = Checker::check(&module_st);
    for func in &module.funcs {
        println!("{:?}", func);
    }

    println!("\n");

    unsafe {
        let ctx = LLVMContextCreate();
        let irmodule = LLVMModuleCreateWithName("test".as_ptr() as *const i8);
        Emitter::emit(ctx, irmodule, &module);
        let cstr = LLVMPrintModuleToString(irmodule);
        let cstr1 = std::ffi::CStr::from_ptr(cstr).to_owned();
        let _str = cstr1.to_str().unwrap();
        println!("{}", _str);
        LLVMDisposeMessage(cstr);
    }
}