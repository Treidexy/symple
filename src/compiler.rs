use crate::lex::{ Token, TokenKind, };

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
	pub fn print(&self, src: &String) {
		let mut line = 0;
		let line_spans = line_spans(src.as_bytes());

		while line < line_spans.len() {
			if line_spans[line].0 <= self.1.start && self.1.start <= line_spans[line].1 {
				break;
			}

			line += 1;
		}

		let line_num_str = line.to_string();
		let pre_pad = " ".repeat(line_num_str.len());
		println!("\u{001b}[31merror:\u{001b}[0m {}", self.0);
		println!("\u{001b}[94m{} |", pre_pad);
		println!("{} |\u{001b}[0m {}", line_num_str, &src[line_spans[line].0..line_spans[line].1]);
		println!("\u{001b}[94m{} | {}\u{001b}[31m{}\u{001b}[0m", pre_pad, " ".repeat(self.1.start - line_spans[line].0), "^".repeat(self.1.end - self.1.start));
	}
}

fn line_spans(contents: &[u8]) -> Vec<(usize, usize)> {
    let mut idx = 0;
    let mut output = vec![];

    let mut start = idx;
    while idx < contents.len() {
        if contents[idx] == b'\n' {
            output.push((start, idx));
            start = idx + 1;
        }
        idx += 1;
    }
    if start < idx {
        output.push((start, idx));
    }

    output
}