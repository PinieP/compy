use std::{fmt, ops::Add};

use crate::util::{IndexWrapper, IntegerWrapper};
pub struct SourceFile {
    pub name: String,
    pub text: String,
    line_map: Option<LineMap>,
}

impl SourceFile {
    pub fn new(name: &str, text: &str) -> Self {
        SourceFile {
            name: String::from(name),
            text: String::from(name),
            line_map: None,
        }
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct FileId(u32);

impl IntegerWrapper for FileId {
    type Wrapped = u32;

    fn index(self) -> Self::Wrapped {
        self.0
    }

    fn from_index(index: Self::Wrapped) -> Self {
        FileId(index)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytePos(u32);
impl Add for BytePos {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        BytePos(self.0 + rhs.0)
    }
}
impl IntegerWrapper for BytePos {
    type Wrapped = u32;

    fn index(self) -> Self::Wrapped {
        self.0
    }

    fn from_index(index: Self::Wrapped) -> Self {
        BytePos(index)
    }
}
impl IndexWrapper for BytePos {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FilePos(u32);

impl Add for FilePos {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        FilePos(self.0 + rhs.0)
    }
}
impl IntegerWrapper for FilePos {
    type Wrapped = u32;

    fn index(self) -> Self::Wrapped {
        self.0
    }

    fn from_index(index: Self::Wrapped) -> Self {
        FilePos(index)
    }
}
impl IndexWrapper for FilePos {}

pub struct SourceMap {
    pub files: Vec<SourceFile>,
}

impl SourceMap {
    pub fn new() -> Self {
        SourceMap { files: Vec::new() }
    }
    pub fn file_by_id(&self, id: FileId) -> &SourceFile {
        &self.files[id.as_usize()]
    }

    pub fn add_source_file(&mut self, file: SourceFile) {
        self.files.push(file);
    }

    pub fn byte_pos_to_file_pos(&self, byte_pos: BytePos) -> (FileId, FilePos) {
        let byte_index = byte_pos.index();
        let mut byte_offset = 0;
        for (id, file) in self.files.iter().enumerate() {
            let next_byte_offset = byte_offset + file.text.len() as u32;
            if byte_index >= byte_offset && byte_index < next_byte_offset {
                return (FileId(id as u32), FilePos(byte_index - byte_offset));
            }
            byte_offset = next_byte_offset;
        }
        panic!("byte pos: '{:?}' out of bounds", byte_pos)
    }
}

#[derive(Debug, Clone, Copy)]
struct LineInfo {
    line: usize,
    column: usize,
}

struct SourcePosition<'a> {
    source_file: &'a SourceFile,
    line: usize,
    column: usize,
}

struct LineMap {
    newline_positions: Vec<FilePos>,
}

impl LineMap {
    fn find_last_line_pos(&self, pos: FilePos) -> (usize, FilePos) {
        let mut begin = 0usize;
        let mut len = self.newline_positions.len();
        let mut end = len;
        while len != 1 {
            let half = begin + (len / 2);
            if self.newline_positions[half] > pos {
                end = half;
            } else {
                begin = half;
            };
            len = end - begin;
        }
        (begin, self.newline_positions[begin])
    }

    pub fn get_line_info(&self, pos: FilePos) -> LineInfo {
        let (index, line_pos) = self.find_last_line_pos(pos);
        LineInfo {
            line: index,
            column: pos.distance_abs(line_pos) as usize,
        }
    }
}

#[derive(Clone)]
pub struct Span {
    pub begin: BytePos,
    pub end: BytePos,
}
impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "span")
    }
}

impl SourceMap {
    fn get_source_position(&self, pos: BytePos) -> SourcePosition {
        let (file_id, file_pos) = self.byte_pos_to_file_pos(pos);
        let source_file = self.file_by_id(file_id);
        let LineInfo { line, column } = source_file
            .line_map
            .as_ref()
            .unwrap()
            .get_line_info(file_pos);
        SourcePosition {
            source_file,
            line,
            column,
        }
    }

    fn span_to_str(&self, span: Span) -> &str {
        let (file_id_begin, begin) = self.byte_pos_to_file_pos(span.begin);
        let (file_id_end, end) = self.byte_pos_to_file_pos(span.end);
        assert_eq!(file_id_begin, file_id_end, "span crosses file boundary");
        &self.file_by_id(file_id_begin).text[begin.as_usize()..end.as_usize()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn find_last_line_pos() {
        let newline_positions = [0, 6, 8, 15, 30, 45, 60, 187, 289, 299, 300, 310, 540]
            .map(|index| FilePos::from_index(index))
            .to_vec();
        let line_table = LineMap { newline_positions };
        let fpos = |i| FilePos::from_index(i);

        assert_eq!(line_table.find_last_line_pos(fpos(32)), (4, fpos(30)));
        assert_eq!(line_table.find_last_line_pos(fpos(0)), (0, fpos(0)));
        assert_eq!(line_table.find_last_line_pos(fpos(7)), (1, fpos(6)));
        assert_eq!(line_table.find_last_line_pos(fpos(29)), (3, fpos(15)));
        assert_eq!(line_table.find_last_line_pos(fpos(540)), (12, fpos(540)));
        assert_eq!(line_table.find_last_line_pos(fpos(580)), (12, fpos(540)));
    }
}
