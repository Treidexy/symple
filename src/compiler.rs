pub type FileId = usize;

#[derive(Debug, Copy, Clone)]
pub struct Span {
	pub file_id: FileId,
	pub start: usize,
	pub end: usize,
}

#[derive(Debug)]
pub struct Error(pub String, pub Span);

impl Error {
	pub fn print<P: AsRef<std::path::Path> + std::fmt::Display>(&self, path: P, src: &String) {
		let mut col = 0;
		let mut line = 0;
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

			if i == self.1.start {
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
		println!("\u{001b}[31merror:\u{001b}[0m {}", self.0);
		println!("{}\u{001b}[94m-->\u{001b}[0m {}:{}:{}", pre_pad, path, line, col);
		println!("\u{001b}[94m{} |", pre_pad);
		println!("{} |\u{001b}[0m {}{}", line_num_str, line_leading_space, line_str);
		println!("\u{001b}[94m{} | {}\u{001b}[31m{}\u{001b}[0m", pre_pad, line_leading_space, "^".repeat(self.1.end - self.1.start));
	}
}