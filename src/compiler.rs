pub type FileId = usize;

#[derive(Debug, Copy, Clone)]
pub struct Span {
	pub file_id: FileId,
	pub start: usize,
	pub end: usize,
}