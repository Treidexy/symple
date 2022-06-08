use crate::check::{ Module, Expr, BinOp, Type, BuiltinType, ExprKind, };
use llvm_sys::prelude::*;
use llvm_sys::core::*;

unsafe fn cstr(s: & str) -> *const i8 {
	let mut s1 = s.to_owned();
	s1.push('\0');
	s1.as_ptr() as *const i8
}

pub struct Emitter {
	ctx: LLVMContextRef,
	irmodule: LLVMModuleRef,
	builder: LLVMBuilderRef,

	types: Vec<LLVMTypeRef>,
}

impl Emitter {
	pub unsafe fn new(ctx: LLVMContextRef, irmodule: LLVMModuleRef) -> Self {
		let builder = LLVMCreateBuilderInContext(ctx);
		
		Self {
			ctx,
			irmodule,
			builder,
			types: vec![],
		}
	}

	pub unsafe fn emit_module(&mut self, module: &Module) {
		let fnty = LLVMFunctionType(LLVMVoidType(), std::ptr::null_mut(), 0, 0);
		let fn_main = LLVMAddFunction(self.irmodule, cstr("main"), fnty);
		let entry = LLVMAppendBasicBlockInContext(self.ctx, fn_main, cstr("entry"));
		LLVMPositionBuilderAtEnd(self.builder, entry);

		for expr in &module.exprs {
			let val = self.emit_expr(expr);
			let ptr = LLVMBuildAlloca(self.builder, LLVMInt64TypeInContext(self.ctx), cstr("val"));
			LLVMBuildStore(self.builder, val, ptr);
		}
	}

	unsafe fn emit_expr(&mut self, expr: &Expr) -> LLVMValueRef {
		match expr.kind {
			ExprKind::BinOp(op, ref left, ref right) => {
				let left = self.emit_expr(left);
				let right = self.emit_expr(right);
				
				match op {
					BinOp::Add => LLVMBuildAdd(self.builder, left, right, cstr("add")),
					BinOp::Sub => LLVMBuildSub(self.builder, left, right, cstr("sub")),
					BinOp::Mul => LLVMBuildMul(self.builder, left, right, cstr("mul")),
					_ => unimplemented!("unsupported bin op"),
				}
			},
			ExprKind::Int(value) => {
				let irtype = LLVMIntTypeInContext(self.ctx, 64);
				LLVMConstInt(irtype, value as u64, 0)
			}
		}
	}
}