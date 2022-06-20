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
	let path = "samples/test.sy";
	let src = std::fs::read_to_string(path).unwrap();

	let tokens = Lexer::lex(file_id, &src);
	for token in &tokens {
		// println!("{:?}", token);
	}

	// println!("");

	let parse_result = Parser::parse(file_id, &tokens);
	for message in &parse_result.messages {
		message.print(path, &src)
	}

	if parse_result.has_error {
		std::process::exit(1);
	}

	let module_st = parse_result.result;
	for func in &module_st.funcs {
		// println!("{:?}", func);
	}

	// println!("");

	let check_result = Checker::check(&module_st);
	for message in &check_result.messages {
		message.print(path, &src)
	}

	if check_result.has_error {
		std::process::exit(1);
	}

	let module = check_result.result;
	for func in &module.funcs {
		// println!("{:?}", func);
	}

	// println!("\n");

	unsafe {
		let ctx = LLVMContextCreate();
		let irmodule = LLVMModuleCreateWithName("test".as_ptr() as *const i8);
		Emitter::emit(ctx, irmodule, &module);
		let cstr = LLVMPrintModuleToString(irmodule);
		let cstr1 = std::ffi::CStr::from_ptr(cstr).to_owned();
		let _str = cstr1.to_str().unwrap();
		// println!("{}", _str);
		LLVMDisposeMessage(cstr);
	}

	println!("\n\u{001b}[32mcompiled successfully\u{001b}[0m");
}