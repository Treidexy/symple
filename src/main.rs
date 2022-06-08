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

use llvm_sys::prelude::*;
use llvm_sys::core::*;

fn main() {
    let file_id: FileId = 0;
    let src = std::fs::read_to_string("samples/test.sy").unwrap();

    let tokens = Lexer::lex(file_id, &src);
    for token in &tokens {
        println!("{:?}", token);
    }

    println!("");

    let module_st = Parser::parse(file_id, &tokens);
    for expr in &module_st.exprs {
        println!("{:?}", expr);
    }

    println!("");

    let module = Checker::check(&module_st);
    for expr in &module.exprs {
        println!("{:?}", expr);
    }

    println!("\n");

    unsafe {
        let ctx = LLVMContextCreate();
        let irmodule = LLVMModuleCreateWithName("test".as_ptr() as *const i8);
        let mut emitter = Emitter::new(ctx, irmodule);
        emitter.emit_module(&module);
        let cstr = LLVMPrintModuleToString(irmodule);
        let cstr1 = std::ffi::CStr::from_ptr(cstr).to_owned();
        let _str = cstr1.to_str().unwrap();
        println!("{}", _str);
    }
}