/*
 * Even though this is rust, I am good programmer, so I be using some unsafe stuff.
 * 
 * DO NOT TRY THIS AT HOME!!!
*/

use crate::check::{ Module, Expr, BinOp, Type, TypeId, BuiltinType, ExprKind, };
use llvm_sys::prelude::*;
use llvm_sys::{ LLVMTypeKind::*, };
use llvm_sys::core::*;

unsafe fn cstr(s: & str) -> *const i8 {
	let mut s1 = s.to_owned();
	s1.push('\0');
	s1.as_ptr() as *const i8
}

pub struct Emitter<'a> {
	ctx: LLVMContextRef,
	irmodule: LLVMModuleRef,
	builder: LLVMBuilderRef,

	module: &'a Module,
	types: Vec<LLVMTypeRef>,
}

impl<'a> Emitter<'a> {
	pub unsafe fn emit(ctx: LLVMContextRef, irmodule: LLVMModuleRef, module: &'a Module) {
		let builder = LLVMCreateBuilderInContext(ctx);
		let mut emitter = Self {
			ctx,
			irmodule,
			builder,

			module,
			types: vec![],
		};

		for ty in &module.types {
			let irty = emitter.emit_type(ty);
			emitter.types.push(irty);
		}

		let fnty = LLVMFunctionType(LLVMVoidType(), std::ptr::null_mut(), 0, 0);
		let fn_main = LLVMAddFunction(emitter.irmodule, cstr("main"), fnty);
		let entry = LLVMAppendBasicBlockInContext(emitter.ctx, fn_main, cstr("entry"));
		LLVMPositionBuilderAtEnd(emitter.builder, entry);

		for expr in &module.exprs {
			let val = emitter.emit_expr(expr);
			let ptr = LLVMBuildAlloca(emitter.builder, LLVMInt64TypeInContext(emitter.ctx), cstr("val"));
			LLVMBuildStore(emitter.builder, val, ptr);
		}
	}

	unsafe fn emit_expr(&mut self, expr: &Expr) -> LLVMValueRef {
		match expr.kind {
			ExprKind::BinOp(op, ref leftt, ref rightt) => {
				let left = self.emit_expr(leftt);
				let right = self.emit_expr(rightt);
				let left = self.emit_cast(left, leftt.type_id, expr.type_id);
				let right = self.emit_cast(right, rightt.type_id, expr.type_id);
				
				match op {
					BinOp::Add => LLVMBuildAdd(self.builder, left, right, cstr("add")),
					BinOp::Sub => LLVMBuildSub(self.builder, left, right, cstr("sub")),
					BinOp::Mul => LLVMBuildMul(self.builder, left, right, cstr("mul")),
					BinOp::SDiv => LLVMBuildSDiv(self.builder, left, right, cstr("sdiv")),
					BinOp::SMod => LLVMBuildSRem(self.builder, left, right, cstr("smod")),
					BinOp::UDiv => LLVMBuildSDiv(self.builder, left, right, cstr("udiv")),
					BinOp::UMod => LLVMBuildSRem(self.builder, left, right, cstr("umod")),
					BinOp::FAdd => LLVMBuildFAdd(self.builder, left, right, cstr("fadd")),
					BinOp::FSub => LLVMBuildFSub(self.builder, left, right, cstr("fsub")),
					BinOp::FMul => LLVMBuildFMul(self.builder, left, right, cstr("fmul")),
					BinOp::FDiv => LLVMBuildFDiv(self.builder, left, right, cstr("fdiv")),
					BinOp::FMod => LLVMBuildFRem(self.builder, left, right, cstr("fmod")),
				}
			},
			ExprKind::Int(value) => {
				let irty = LLVMIntTypeInContext(self.ctx, 64);
				LLVMConstInt(irty, value as u64, 0)
			}
		}
	}

	fn find_type(&self, type_id: TypeId) -> LLVMTypeRef {
		for (i, ty) in self.types.iter().enumerate() {
			if type_id == i {
				return *ty;
			}
		}

		panic!("Type not found: {}", type_id);
	}

	unsafe fn emit_type(&mut self, ty: &Type) -> LLVMTypeRef {
		match ty {
			Type::Builtin(builtin_ty) =>
				match builtin_ty {
					BuiltinType::Int(x) => LLVMIntTypeInContext(self.ctx, *x),
					BuiltinType::UInt(x) => LLVMIntTypeInContext(self.ctx, *x),
					BuiltinType::Float(x) => match *x {
						16 => LLVMHalfTypeInContext(self.ctx),
						32 => LLVMFloatTypeInContext(self.ctx),
						64 => LLVMDoubleTypeInContext(self.ctx),
						128 => LLVMFP128TypeInContext(self.ctx),
						_ => panic!("bad code"),
					}
				},
		}
	}

	unsafe fn emit_cast(&mut self, val: LLVMValueRef, from_id: TypeId, to_id: TypeId) -> LLVMValueRef {
		let from = LLVMTypeOf(val);
		let from_kind = LLVMGetTypeKind(from);
		let to = self.find_type(to_id);
		let to_kind = LLVMGetTypeKind(to);

		let from_ty = &self.module.types[from_id];
		let to_ty = &self.module.types[to_id];

		if from_kind == LLVMIntegerTypeKind && to_kind == LLVMIntegerTypeKind {
			LLVMBuildIntCast(self.builder, val, to, cstr("cast"))
		} else if from_kind == LLVMFloatTypeKind && to_kind == LLVMFloatTypeKind {
			LLVMBuildFPCast(self.builder, val, to, cstr("cast"))
		} else if from_kind == LLVMIntegerTypeKind && to_kind == LLVMFloatTypeKind {
			match from_ty {
				Type::Builtin(BuiltinType::Int(_)) => {
					LLVMBuildSIToFP(self.builder, val, to, cstr("cast"))
				},
				Type::Builtin(BuiltinType::UInt(_)) => {
					LLVMBuildUIToFP(self.builder, val, to, cstr("cast"))
				},
				_ => panic!("bad code"),
			}
		}  else if from_kind == LLVMFloatTypeKind && to_kind == LLVMIntegerTypeKind {
			match to_ty {
				Type::Builtin(BuiltinType::Int(_)) => {
					LLVMBuildFPToSI(self.builder, val, to, cstr("cast"))
				},
				Type::Builtin(BuiltinType::UInt(_)) => {
					LLVMBuildFPToUI(self.builder, val, to, cstr("cast"))
				},
				_ => panic!("bad code"),
			}
		} else {
			panic!("bad cast");
		}
	}
}