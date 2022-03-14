use std::convert::TryFrom;
use std::hash::Hash;
use std::io::Error;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use miette::{MietteError, MietteSpanContents, SourceCode};

use crate::context::LivingInTheDatabase;
use crate::interner::{Identifier, FieldName};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
    pub file: FileId,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Spanned<T: Clone + Hash + Eq> {
    pub inner: T,
    pub span: Span,
}

pub type IdSpan = Spanned<Identifier>;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct FieldSpan {
    pub id: FieldName,
    pub span: Span,
}

impl From<Span> for miette::SourceSpan {
    fn from(s: Span) -> Self {
        let length = miette::SourceOffset::from((s.hi - s.lo) as usize);
        let start = miette::SourceOffset::from(s.lo as usize);
        Self::new(start, length)
    }
}

impl Span {
    pub fn len(&self) -> usize {
        (self.hi - self.lo) as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct FileId(u32);

impl FileId {
    fn inc(self) -> Self {
        FileId(self.0.checked_add(1).expect("too many files"))
    }
    fn to_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FileData {
    pub id: FileId,
    pub path: Option<PathBuf>,
    pub content: Arc<String>,
}

impl SourceCode for FileData {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, MietteError> {
        let span_content =
            self.content
                .read_span(span, context_lines_before, context_lines_after)?;
        Ok(Box::new(match &self.path {
            Some(path) => MietteSpanContents::new_named(
                path.to_string_lossy().to_string(),
                span_content.data(),
                span_content.span().clone(),
                span_content.line(),
                span_content.column(),
                span_content.line_count(),
            ),
            None => MietteSpanContents::new(
                span_content.data(),
                span_content.span().clone(),
                span_content.line(),
                span_content.column(),
                span_content.line_count(),
            ),
        }))
    }
}

impl FileData {
    fn new(path: &Path, id: FileId) -> Result<Self, Error> {
        let content = Arc::new(std::fs::read_to_string(path)?);
        u32::try_from(content.len()).expect("File length bigger than 32 bits");
        Ok(FileData {
            id,
            path: Some(path.to_owned()),
            content,
        })
    }
    fn new_anon(s: &str, id: FileId) -> Self {
        FileData {
            id,
            path: None,
            content: Arc::new(s.to_string()),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct FileCollection {
    current_id: FileId,
    files: Vec<FileData>,
}

impl FileCollection {
    pub fn new() -> Self {
        FileCollection::default()
    }
    pub fn add(&mut self, new_file: &Path) -> Result<FileId, Error> {
        let new_f = FileData::new(new_file, self.current_id)?;
        let old_id = self.current_id;
        self.current_id = self.current_id.inc();
        self.files.push(new_f);
        Ok(old_id)
    }
    pub fn add_anon(&mut self, s: &str) -> FileId {
        let f = FileData::new_anon(s, self.current_id);
        let old_id = self.current_id;
        self.current_id = self.current_id.inc();
        self.files.push(f);
        old_id
    }
    pub fn file_data(&self, id: FileId) -> &FileData {
        &self.files[id.to_usize()]
    }
    pub fn content(&self, id: FileId) -> &str {
        &self.file_data(id).content
    }
    pub fn path(&self, id: FileId) -> Option<&Path> {
        self.file_data(id).path.as_deref()
    }
    pub fn span(&self, span: Span) -> &str {
        &self.content(span.file)[span.lo as usize..span.hi as usize]
    }
    pub fn insert_into_db(&self, db: &mut LivingInTheDatabase) {
        for (i, f) in self.files.iter().enumerate() {
            let fid = FileId(i as u32);
            db.set_input_file(fid, Arc::new(f.clone()));
        }
    }
}

#[salsa::query_group(FileDatabase)]
pub trait Files: salsa::Database {
    #[salsa::input]
    fn input_file(&self, id: FileId) -> Arc<FileData>;

    fn file_content(&self, id: FileId) -> Arc<String>;

    fn path(&self, id: FileId) -> Option<PathBuf>;
}

fn file_content(db: &dyn Files, id: FileId) -> Arc<String> {
    let fd = db.input_file(id);
    fd.content.clone()
}

fn path(db: &dyn Files, id: FileId) -> Option<PathBuf> {
    let fd = db.input_file(id);
    fd.path.clone()
}
