use crate::compiler::*;
use crate::parse::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(pub usize);

pub const NONE_TYPE_ID  : TypeId = TypeId(0);
pub const I8_TYPE_ID    : TypeId = TypeId(1);
pub const I16_TYPE_ID   : TypeId = TypeId(2);
pub const I32_TYPE_ID   : TypeId = TypeId(3);
pub const I64_TYPE_ID   : TypeId = TypeId(4);
pub const U8_TYPE_ID    : TypeId = TypeId(5);
pub const U16_TYPE_ID   : TypeId = TypeId(6);
pub const U32_TYPE_ID   : TypeId = TypeId(7);
pub const U64_TYPE_ID   : TypeId = TypeId(8);
pub const F16_TYPE_ID   : TypeId = TypeId(9);
pub const F32_TYPE_ID   : TypeId = TypeId(10);
pub const F64_TYPE_ID   : TypeId = TypeId(11);
pub const BOOL_TYPE_ID  : TypeId = TypeId(12);

pub const LAST_BUILTIN_TYPE_ID : TypeId = BOOL_TYPE_ID;

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
	None,
	Builtin,
}

#[derive(Debug)]
pub struct Module {
	pub types: Vec<Type>,
	pub funcs: Vec<Func>,
}

#[derive(Debug, Copy, Clone)]
pub enum BinOp {
	Add,
	Sub,
	Mul,
	SDiv,
	SMod,
	UDiv,
	UMod,
	FAdd,
	FSub,
	FMul,
	FDiv,
	FMod,
}

#[derive(Debug)]
pub enum ExprKind {
	Null,
	BinOp(BinOp, Box<Expr>, Box<Expr>),
	Int(i64),
	Float(f64),
}

#[derive(Debug)]
pub struct Expr {
	pub kind: ExprKind,
	pub type_id: TypeId,
	pub span: Span,
}

#[derive(Debug)]
pub struct Func {
	pub type_id: TypeId,
	pub name: String,
	pub stmts: Vec<Stmt>,
	pub eval: Expr,
	pub span: Span,
}

#[derive(Debug)]
pub enum Stmt {
	None,
	Expr(Expr),
	Func(Func),
}

pub struct Checker {
	module: Module,

	messages: Vec<Message>,
	has_error: bool,

	// hax for name mangling
	scope_name: String,
}

impl Checker {
	pub fn check(module_st: &ModuleST) -> CompilerResult<Module> {
		let mut checker = Checker {
			module: Module {
				types: vec![
					Type::None,
					Type::Builtin, // i8
					Type::Builtin, // i16
					Type::Builtin, // i32
					Type::Builtin, // i64
					Type::Builtin, // u8
					Type::Builtin, // u16
					Type::Builtin, // u32
					Type::Builtin, // u64
					Type::Builtin, // f16
					Type::Builtin, // f32
					Type::Builtin, // f64
					Type::Builtin, // bool
				],
				funcs: vec![],
			},

			messages: vec![],
			has_error: false,

			scope_name: String::new(),
		};

		for func_st in &module_st.funcs {
			let checked = checker.check_func(func_st);
			if checked.is_some() {
				checker.module.funcs.push(checked.unwrap());
			}
		}

		CompilerResult {
			result: checker.module,
			messages: checker.messages,
			has_error: checker.has_error,
		}
	}

	fn check_type(&mut self, ast: &TypeST) -> TypeId {
		match ast.kind {
			TypeSTKind::Name(ref name) => match name.as_str() {
				"i32" => I32_TYPE_ID,
				_ => todo!(),
			},
			TypeSTKind::None | TypeSTKind::Error => NONE_TYPE_ID,
		}
	}

	fn check_func(&mut self, ast: &FuncST) -> Option<Func> {
		let added_dot = if !self.scope_name.is_empty() {
			self.scope_name.push('.');
			1
		} else {
			0
		};

		self.scope_name.push_str(&ast.name);

		let type_id = self.check_type(&ast.ty);

		// todo: better mangling
		let mangled_name = self.scope_name.clone();

		let mut stmts = vec![];
		for stmt_st in &ast.stmts {
			let checked = self.check_stmt(stmt_st);
			stmts.push(checked);
		}

		self.scope_name.truncate(self.scope_name.len() - ast.name.len() - added_dot);

		let eval = if ast.eval.is_some() {
			self.check_expr(ast.eval.as_ref().unwrap())
		} else {
			Expr {
				kind: ExprKind::Null,
				type_id: NONE_TYPE_ID,
				span: Span::new(ast.span.file_id),
			}
		};

		if eval.type_id != type_id {
			self.report_error(format!("expected `{}` because of return type, found `{}`", type_id.to_string(self.module), eval.type_id.to_string(self.module)), eval.span);
			return None;
		}

		Some(Func {
			type_id,
			name: mangled_name,
			stmts,
			eval,
			span: ast.span,
		})
	}

	fn check_stmt(&mut self, ast: &StmtST) -> Stmt {
		match ast {
			StmtST::Expr(expr_st) => {
				let checked = self.check_expr(expr_st);
				Stmt::Expr(checked)
			},
			StmtST::Func(func) => {
				let checked = self.check_func(func);
				Stmt::None
			},
			_ => todo!(),
		}
	}

	fn check_expr(&mut self, ast: &ExprST) -> Expr {
		match ast.kind {
			ExprSTKind::Error => unreachable!(),
			ExprSTKind::BinOp(op, ref left, ref right) => {
				let left = self.check_expr(left);
				let right = self.check_expr(right);
				let span = ast.span;
				let type_id = self.resolve_bin_op_type(op, left.type_id, right.type_id);
				let op = self.bin_op_st_to_bin_op(op, type_id);

				Expr {
					kind: ExprKind::BinOp(op, Box::new(left), Box::new(right)),
					type_id,
					span,
				}
			},
			ExprSTKind::Int(value) => {
				let span = ast.span;
				let type_id = I64_TYPE_ID;

				Expr {
					kind: ExprKind::Int(value),
					type_id,
					span,
				}
			},
			ExprSTKind::Float(value) => {
				let span = ast.span;
				let type_id = F64_TYPE_ID;

				Expr {
					kind: ExprKind::Float(value),
					type_id,
					span,
				}
			},
		}
	}

	fn resolve_bin_op_type(&mut self, _op: BinOpST, left_id: TypeId, right_id: TypeId) -> TypeId {
		if left_id.is_float() {
			left_id
		} else if right_id.is_float() {
			right_id
		} else {
			left_id
		}
	}

	fn bin_op_st_to_bin_op(&self, op: BinOpST, type_id: TypeId) -> BinOp {
		if type_id.is_float() {
			match op {
				BinOpST::Add => BinOp::FAdd,
				BinOpST::Sub => BinOp::FSub,
				BinOpST::Mul => BinOp::FMul,
				BinOpST::Div => BinOp::FDiv,
				BinOpST::Mod => BinOp::FMod,
			}
		} else {
			match op {
				BinOpST::Add => BinOp::Add,
				BinOpST::Sub => BinOp::Sub,
				BinOpST::Mul => BinOp::Mul,
				BinOpST::Div => if type_id.is_sint() { BinOp::SDiv } else { BinOp::UDiv },
				BinOpST::Mod => if type_id.is_sint() { BinOp::SMod } else { BinOp::UMod },
			}
		}
	}

	fn report_error(&mut self, msg: String, span: Span) {
		self.messages.push(Message {
			kind: MessageKind::Error,
			span: span,
			text: msg,
		});

		self.has_error = true;
	}

	fn report_mistake(&mut self, err: String, fix: String, span: Span) {
		self.messages.push(Message {
			kind: MessageKind::Mistake,
			span: span,
			text: format!("{}\u{001b}[93m; {}", err, fix),
		});
	}

	fn report_warning(&mut self, msg: String, span: Span) {
		self.messages.push(Message {
			kind: MessageKind::Warning,
			span: span,
			text: msg,
		});
	}
	
	fn report_note(&mut self, msg: String, span: Span) {
		self.messages.push(Message {
			kind: MessageKind::Note,
			span: span,
			text: msg,
		});
	}
}

impl Module {
	fn find_or_add_type(&mut self, ty: Type) -> TypeId {
		for (i, t) in self.types.iter().enumerate() {
			if t == &ty {
				return TypeId(i);
			}
		}

		let type_id = TypeId(self.types.len());
		self.types.push(ty);
		type_id
	}
}

impl TypeId {
	fn is_float(&self) -> bool {
		self == &F16_TYPE_ID || self == &F32_TYPE_ID || self == &F64_TYPE_ID
	}

	fn is_sint(&self) -> bool {
		self == &I8_TYPE_ID || self == &I16_TYPE_ID || self == &I32_TYPE_ID || self == &I64_TYPE_ID
	}

	fn is_uint(&self) -> bool {
		self == &U8_TYPE_ID || self == &U16_TYPE_ID || self == &U32_TYPE_ID || self == &U64_TYPE_ID
	}

	fn is_builtin(&self) -> bool {
		self.0 <= LAST_BUILTIN_TYPE_ID.0
	}

	fn to_string(&self, module: Module) -> String {
		let ty = module.types[self.0];
		match ty {
			Type::None => "()".to_string(),
			Type::Builtin => match self {
				&I8_TYPE_ID => "i8".to_string(),
				&I16_TYPE_ID => "i16".to_string(),
				&I32_TYPE_ID => "i32".to_string(),
				&I64_TYPE_ID => "i64".to_string(),
				&U8_TYPE_ID => "u8".to_string(),
				&U16_TYPE_ID => "u16".to_string(),
				&U32_TYPE_ID => "u32".to_string(),
				&U64_TYPE_ID => "u64".to_string(),
				&F16_TYPE_ID => "f16".to_string(),
				&F32_TYPE_ID => "f32".to_string(),
				&F64_TYPE_ID => "f64".to_string(),
				&BOOL_TYPE_ID => "bool".to_string(),
			},
		}
	}
}