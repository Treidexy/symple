#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
	Eof,
	Unknown,

	LineComment,
	BlockComment,
	Whitespace,

	Name,

	// Literal
	/// no decimal point
	Int, 
	/// one decimal point
	Float,
	/// multiple decimal points
	Plex,
	String,
	Char,
	TrueKw,
	FalseKw,

	// Arithmetic
	Plus,
	Minus,
	Star,
	Slash,
	Percent,

	// Logic
	Carot,
	Pipe,
	Ampersand,
	Equal,
	Bang,

	// Comparison
	LChevronEqual,
	RChevronEqual,

	// Comparison + Grouping
	LChevron,
	RChevron,

	// Grouping
	LParen,
	RParen,
	LBrace,
	RBrace,
	LBracket,
	RBracket,

	// Punctuation
	Comma,
	Colon,
	ColonColon,
	Semicolon,
	Dot,

	// Notation
	PublicKw,
	MutKw,
	LetKw,
	ConstKw,
	FnKw,
	SysKw,

	// Control flow
	IfKw,
	ElseKw,
	LoopKw,
	ForKw,

	// Misc
	LArrow,
	RArrow,
	Dollar,

	// Contract
	TypeKw,
	TraitKw,

	// Implementation
	StructKw,
	EnumKw,
	ImplKw,
}