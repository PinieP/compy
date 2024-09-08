use core::fmt;

pub struct SourceFile {
    pub name: String,
    pub text: String,
    line_map: Option<LineMap>,
}

pub struct CompileTarget {
    pub files: Vec<SourceFile>,
}

impl CompileTarget {
    fn get_file_by_id(&self, id: FileId) -> &SourceFile {
        &self.files[id.index as usize]
    }
}

#[derive(Copy, Clone, Debug, Default)]
pub struct FileId {
    index: u64,
}

impl FileId {
    pub fn from_index(index: u64) -> Self {
        FileId { index }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FilePos {
    pub index: u64,
}

#[derive(Debug, Clone, Copy)]
struct LineInfo {
    line: u64,
    column: u64,
}

struct SourcePosition<'a> {
    file_name: &'a str,
    line: u64,
    column: u64,
}

struct LineMap {
    newline_positions: Vec<FilePos>,
}

impl LineMap {
    fn find_last_line_pos(&self, pos: FilePos) -> (u64, FilePos) {
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
        (begin as u64, self.newline_positions[begin])
    }

    pub fn get_line_info(&self, pos: FilePos) -> LineInfo {
        let (index, line_pos) = self.find_last_line_pos(pos);
        LineInfo {
            line: index + 1,
            column: pos.index - line_pos.index,
        }
    }
}

#[derive(Clone)]
pub struct Span {
    pub file_id: FileId,
    pub begin: FilePos,
    pub end: FilePos,
}
impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "span")
    }
}

impl CompileTarget {
    fn get_line_info(&self, file_id: FileId, file_pos: FilePos) -> LineInfo {
        self.files[file_id.index as usize]
            .line_map
            .as_ref()
            .unwrap()
            .get_line_info(file_pos)
    }

    fn get_source_position(&self, file_id: FileId, file_pos: FilePos) -> SourcePosition {
        let line_info = self.get_line_info(file_id, file_pos);
        SourcePosition {
            file_name: &self.files[file_id.index as usize].name,
            line: line_info.line,
            column: line_info.column,
        }
    }
    fn get_slice(&self, span: Span) -> &str {
        &self.files[span.file_id.index as usize].text
            [span.begin.index as usize..span.end.index as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn find_last_line_pos() {
        let newline_positions = [0, 6, 8, 15, 30, 45, 60, 187, 289, 299, 300, 310, 540]
            .map(|index| FilePos { index })
            .to_vec();
        let line_table = LineMap { newline_positions };
        fn fpos(index: u64) -> FilePos {
            FilePos { index }
        }

        assert_eq!(line_table.find_last_line_pos(fpos(32)), (4, fpos(30)));
        assert_eq!(line_table.find_last_line_pos(fpos(0)), (0, fpos(0)));
        assert_eq!(line_table.find_last_line_pos(fpos(7)), (1, fpos(6)));
        assert_eq!(line_table.find_last_line_pos(fpos(29)), (3, fpos(15)));
        assert_eq!(line_table.find_last_line_pos(fpos(540)), (12, fpos(540)));
        assert_eq!(line_table.find_last_line_pos(fpos(580)), (12, fpos(540)));
    }
}
