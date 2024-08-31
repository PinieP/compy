pub struct SourceFile {
    pub name: String,
    pub text: String,
}

pub struct CompileTarget {
    files: Vec<SourceFile>,
}

impl CompileTarget {
    fn get_file_by_id(&self, id: FileId) -> &SourceFile {
        &self.files[id.index as usize]
    }
}

#[derive(Copy, Clone, Debug)]
pub struct FileId {
    index: u16,
}

impl FileId {
    pub fn from_index(index: u16) -> Self {
        FileId { index }
    }
}

#[derive(Clone, Debug)]
pub struct Span {
    pub file_id: FileId,
    pub begin: u64,
    pub end: u64,
}
