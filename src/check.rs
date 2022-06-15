use crate::compiler::*;
use crate::parse::*;

pub type TypeId = usize;

#[derive(Debug, Eq, PartialEq)]
pub enum BuiltinType {
	Int(u32),
	UInt(u32),
	Float(u32),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
	None,
	Builtin(BuiltinType),
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
	pub span: Span,
}

#[derive(Debug)]
pub enum Stmt {
	Expr(Expr),
	Func(Func),
}

pub struct Checker {
	module: Module,

	// hax for name mangling
	scope_name: String,
}

impl Checker {
	pub fn check(module_st: &ModuleST) -> Module {
		let mut checker = Checker {
			module: Module {
				types: vec![],
				funcs: vec![],
			},

			scope_name: String::new(),
		};

		for func_st in &module_st.funcs {
			let checked = checker.check_func(func_st);
			checker.module.funcs.push(checked);
		}

		checker.module
	}

	fn check_type(&mut self, ast: &TypeST) -> Type {
		match ast.kind {
			TypeSTKind::Name(ref name) => match name.as_str() {
				"i32" => Type::Builtin(BuiltinType::Int(32)),
				_ => todo!(),
			},
			TypeSTKind::None | TypeSTKind::Error => {
				Type::None
			},
		}
	}

	fn check_func(&mut self, ast: &FuncST) -> Func {
		let added_dot = if !self.scope_name.is_empty() {
			self.scope_name.push('.');
			1
		} else {
			0
		};

		self.scope_name.push_str(&ast.name);

		let ty = self.check_type(&ast.ty);
		let type_id = self.module.find_or_add_type(ty);

		// todo: better mangling
		let mangled_name = self.scope_name.clone();

		let mut stmts = vec![];
		for stmt_st in &ast.stmts {
			let checked = self.check_stmt(stmt_st);
			stmts.push(checked);
		}

		self.scope_name.truncate(self.scope_name.len() - ast.name.len() - added_dot);


		Func {
			type_id,
			name: mangled_name,
			stmts,
			span: ast.span,
		}
	}

	fn check_stmt(&mut self, ast: &StmtST) -> Stmt {
		match ast {
			StmtST::Expr(expr_st) => {
				let checked = self.check_expr(expr_st);
				Stmt::Expr(checked)
			},
			StmtST::Func(func) => {
				let checked = self.check_func(func);
				Stmt::Func(checked)
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
				let ty = Type::Builtin(BuiltinType::Int(64));
				let type_id = self.module.find_or_add_type(ty);

				Expr {
					kind: ExprKind::Int(value),
					type_id,
					span,
				}
			},
			ExprSTKind::Float(value) => {
				let span = ast.span;
				let ty = Type::Builtin(BuiltinType::Float(64));
				let type_id = self.module.find_or_add_type(ty);

				Expr {
					kind: ExprKind::Float(value),
					type_id,
					span,
				}
			},
		}
	}

	fn resolve_bin_op_type(&mut self, _op: BinOpST, left_id: TypeId, right_id: TypeId) -> TypeId {
		let left = &self.module.types[left_id];
		let right = &self.module.types[right_id];

		if left.is_float() {
			left_id
		} else if right.is_float() {
			right_id
		} else {
			left_id
		}
	}

	fn bin_op_st_to_bin_op(&self, op: BinOpST, type_id: TypeId) -> BinOp {
		let ty = &self.module.types[type_id];
		
		if ty.is_float() {
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
				BinOpST::Div => if ty.is_signed_int() { BinOp::SDiv } else { BinOp::UDiv },
				BinOpST::Mod => if ty.is_signed_int() { BinOp::SMod } else { BinOp::UMod },
			}
		}
	}
}

impl Module {
	fn find_or_add_type(&mut self, ty: Type) -> TypeId {
		for (i, t) in self.types.iter().enumerate() {
			if t == &ty {
				return i;
			}
		}

		let type_id = self.types.len();
		self.types.push(ty);
		type_id
	}
}

impl Type {
	pub fn is_signed_int(&self) -> bool {
		match self {
			Type::Builtin(BuiltinType::Int(_)) => true,
			_ => false,
		}
	}

	pub fn is_int(&self) -> bool {
		match self {
			Type::Builtin(BuiltinType::Int(_)) => true,
			Type::Builtin(BuiltinType::UInt(_)) => true,
			_ => false,
		}
	}

	pub fn is_float(&self) -> bool {
		match self {
			Type::Builtin(BuiltinType::Float(_)) => true,
			_ => false,
		}
	}
}