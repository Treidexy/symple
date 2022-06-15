pub type FileId = usize;

#[derive(Debug, Copy, Clone)]
pub struct Span {
	pub file_id: FileId,
	pub start: usize,
	pub end: usize,
}

impl Span {
	pub fn new(file_id: FileId) -> Self {
		Self { file_id, start: 0, end: 0 }
	}

	pub fn gap(&self, other: Span) -> Span {
		let start = self.end - 1;
		let end = other.start + 1;
		Span { file_id: self.file_id, start, end }
	}
}

#[derive(Debug, Copy, Clone)]
pub enum MessageKind {
	Error,
	Mistake,
	Warning,
	Note,
}

#[derive(Debug)]
pub struct Message {
	pub kind: MessageKind,
	pub span: Span,
	pub text: String,
}

impl MessageKind {
	pub fn color(self) -> &'static str {
		match self {
			MessageKind::Error => "31", // red
			MessageKind::Mistake => "91", // bright red
			MessageKind::Warning => "33", // yellow
			MessageKind::Note => "36", // cyan
		}
	}
}

impl std::fmt::Display for MessageKind {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			MessageKind::Error => write!(f, "error"),
			MessageKind::Mistake => write!(f, "mistake"),
			MessageKind::Warning => write!(f, "warning"),
			MessageKind::Note => write!(f, "note"),
		}
	}
}

impl Message {
	pub fn print<P: AsRef<std::path::Path> + std::fmt::Display>(&self, path: P, src: &String) {
		let mut col = 0;
		let mut line = 1;
		let mut line_start = 0;
		let mut line_end = src.len();
		let mut found = false;

		for (i, c) in src.chars().enumerate() {
			if c == '\n' {
				if found {
					line_end = i - 1;
					break;
				} else {
					line_start = i + 1;
					line += 1;
					col = 0;
				}
			}

			if i == self.span.start {
				found = true;
			}

			if !found {
				col += 1;
			}
		}

		let line_num_str = line.to_string();
		let line_str = &src[line_start..line_end].trim();
		let line_leading_space = &src[line_start..line_end-line_str.len()];
		let pre_pad = " ".repeat(line_num_str.len());
		println!("\u{001b}[{}m{}:\u{001b}[0m {}", self.kind.color(), self.kind, self.text);
		println!("{}\u{001b}[94m-->\u{001b}[0m {}:{}:{} + {}", pre_pad, path, line, col, self.span.end - self.span.start);
		println!("\u{001b}[94m{} |", pre_pad);
		println!("{} |\u{001b}[0m {}{}", line_num_str, line_leading_space, line_str);
		println!("\u{001b}[94m{} | {}{}\u{001b}[31m{}\u{001b}[0m", pre_pad, line_leading_space, " ".repeat(self.span.start - line_start), "^".repeat(self.span.end - self.span.start));
	}
}