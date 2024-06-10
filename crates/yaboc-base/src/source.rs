use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::io::Error;
use std::num::NonZeroU32;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ariadne::{Cache, FnCache};
use dirs::data_dir;
use fxhash::FxHashMap;

use crate::databased_display::DatabasedDisplay;
use crate::dbformat;
use crate::error::diagnostic::{DiagnosticKind, Label};
use crate::error::{Report, SResult, SilencedError};
use crate::hash::StableHash;
use crate::interner::{DefId, FieldName, Identifier, IdentifierName, Interner};

#[salsa::query_group(FileDatabase)]
pub trait Files: salsa::Database {
    #[salsa::input]
    fn all_files(&self) -> Arc<Vec<FileId>>;

    #[salsa::input]
    fn input_file(&self, id: FileId) -> Arc<FileData>;

    #[salsa::input]
    fn std(&self) -> SResult<FileId>;

    #[salsa::input]
    fn core(&self) -> SResult<FileId>;

    fn file_content(&self, id: FileId) -> Arc<String>;

    fn path(&self, id: FileId) -> Option<String>;

    fn file_lines(&self, id: FileId) -> Arc<BTreeMap<usize, usize>>;

    fn file_offset_pos(&self, id: FileId, offset: (usize, usize)) -> (LineColumn, LineColumn);
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
    pub file: FileId,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Spanned<T: Clone + Hash + Eq> {
    pub inner: T,
    pub span: Span,
}

pub type IdSpan = Spanned<Identifier>;

pub type FieldSpan = Spanned<FieldName>;

impl ariadne::Span for Span {
    type SourceId = FileId;

    fn source(&self) -> &Self::SourceId {
        &self.file
    }

    fn start(&self) -> usize {
        self.lo as usize
    }

    fn end(&self) -> usize {
        self.hi as usize
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

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

impl FileId {
    fn inc(self) -> Self {
        FileId(self.0.checked_add(1).expect("too many files"))
    }
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl<DB: Files + ?Sized> StableHash<DB> for FileId {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        db.path(*self).update_hash(state, db)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FileData {
    pub id: FileId,
    pub path: Option<String>,
    pub content: Arc<String>,
}

impl FileData {
    fn new(path: &str, id: FileId) -> Result<Self, Error> {
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
    pub fn add(&mut self, new_file: &str) -> Result<FileId, Error> {
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
    pub fn path(&self, id: FileId) -> Option<&str> {
        self.file_data(id).path.as_deref()
    }
    pub fn span(&self, span: Span) -> &str {
        &self.content(span.file)[span.lo as usize..span.hi as usize]
    }
    pub fn insert_into_db(&self, db: &mut (impl ?Sized + Files)) {
        for (i, f) in self.files.iter().enumerate() {
            let fid = FileId(i as u32);
            db.set_input_file(fid, Arc::new(f.clone()));
        }
    }
}

pub struct FileResolver<'collection> {
    files: &'collection mut FileCollection,
    paths: FxHashMap<PathBuf, Option<FileId>>,
    absolute_mods: FxHashMap<Identifier, FileId>,
    relative_mods: FxHashMap<(PathBuf, Identifier), Option<FileId>>,
    lib_path: Option<PathBuf>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum LibKind {
    Core,
}

impl LibKind {
    fn name(&self) -> &'static str {
        match self {
            LibKind::Core => "core",
        }
    }

    fn env_var(&self) -> &'static str {
        match self {
            LibKind::Core => "YABO_CORE_PATH",
        }
    }

    fn set_db_path(&self, db: &mut (impl ?Sized + Files), path: SResult<FileId>) {
        match self {
            LibKind::Core => db.set_core(path),
        }
    }
}

impl std::fmt::Display for LibKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LibKind::Core => write!(f, "core"),
        }
    }
}

pub fn yabo_lib_path() -> Option<PathBuf> {
    match std::env::var_os("YABO_LIB_PATH") {
        Some(std_env_path) => Some(PathBuf::from(std_env_path)),
        None => {
            let mut yabo_dir = data_dir().unwrap();
            yabo_dir.push("yabo");
            yabo_dir.push("lib");
            yabo_dir.exists().then_some(yabo_dir)
        }
    }
}

impl<'collection> FileResolver<'collection> {
    pub fn new(files: &'collection mut FileCollection) -> Self {
        let mut res = FileResolver {
            files,
            paths: FxHashMap::default(),
            absolute_mods: FxHashMap::default(),
            relative_mods: FxHashMap::default(),
            lib_path: None,
        };
        res.init_paths();
        res
    }

    fn init_paths(&mut self) {
        self.lib_path = yabo_lib_path();
    }

    fn file_path(&self, dir: &Path, name: &str) -> Option<PathBuf> {
        let mut path = dir.to_path_buf();
        let name_with_suffix = format!("{name}.yb");
        path.push(name_with_suffix);
        if path.exists() {
            return Some(path);
        }
        path.pop();
        path.push(name);
        path.push("mod.yb");
        path.exists().then_some(path)
    }

    fn add_file(
        &mut self,
        db: &mut (impl Interner + ?Sized),
        path: PathBuf,
        origin: FileId,
        span: Span,
        name: Identifier,
    ) -> Result<FileId, FileLoadError> {
        if let Some(id) = self.paths.get(&path) {
            return id.ok_or(FileLoadError::Silenced);
        }
        let id =
            self.files
                .add(&path.to_string_lossy())
                .map_err(|error| FileLoadError::LoadError {
                    source: (origin, span),
                    name: name.to_owned(),
                    error,
                });
        self.paths.insert(path, id.as_ref().ok().cloned());
        if let Ok(id) = id {
            db.set_input_file(id, Arc::new(self.files.file_data(id).clone()));
        }
        id
    }

    pub fn resolve(
        &mut self,
        db: &mut (impl Interner + ?Sized),
        origin: FileId,
        name: Identifier,
        span: Span,
    ) -> Result<FileId, FileLoadError> {
        let name_str = db.lookup_intern_identifier(name).name;
        let Some(origin_path) = self.files.path(origin) else {
            return Err(FileLoadError::DoesNotExist {
                source: (origin, span),
                name,
            });
        };
        let path_dir = Path::new(&origin_path).parent().unwrap();
        // have to think more about what to do when canonicalization failed, as we don't want duplicate errors
        // or the same file as two different files
        let path_dir = path_dir
            .canonicalize()
            .unwrap_or_else(|_| path_dir.to_path_buf());
        if let Some(file_id) = self.relative_mods.get(&(path_dir.clone(), name)) {
            return file_id.ok_or(FileLoadError::Silenced);
        }

        let new_file_path = if let Some(path) = self.file_path(&path_dir, &name_str) {
            Ok(path)
        } else if let Some(f) = self.absolute_mods.get(&name) {
            return Ok(*f);
        } else {
            self.lib_path
                .clone()
                .and_then(|lib_path| self.file_path(&lib_path, &name_str))
                .ok_or(FileLoadError::DoesNotExist {
                    source: (origin, span),
                    name,
                })
        };
        let new_file_path = new_file_path.and_then(|x| self.add_file(db, x, origin, span, name));
        self.relative_mods
            .insert((path_dir, name), new_file_path.as_ref().ok().cloned());
        new_file_path
    }

    pub fn add_std(
        &mut self,
        db: &mut (impl Interner + ?Sized),
        kind: LibKind,
    ) -> Result<(), FileLoadError> {
        let std_ident = db.intern_identifier(IdentifierName::new(kind.name().into()));
        if let Some(std) = self.absolute_mods.get(&std_ident) {
            kind.set_db_path(db, Ok(*std));
            return Ok(());
        }
        let err = FileLoadError::NoStdLib {
            source: (FileId::default(), Span::default()),
            kind,
        };
        let std_dir = if let Some(std_env_path) = std::env::var_os(kind.env_var()) {
            PathBuf::from(std_env_path)
        } else {
            let Some(lib_path) = self.lib_path.clone() else {
                return Err(err);
            };
            lib_path
        };
        let Some(std_dir) = self.file_path(&std_dir, kind.name()) else {
            kind.set_db_path(db, Err(SilencedError::new()));
            return Err(err);
        };
        // note that we can not really give a good span here, as the std is not really loaded by any one file
        let file = self.add_file(db, std_dir, FileId::default(), Span::default(), std_ident)?;
        self.absolute_mods.insert(std_ident, file);
        kind.set_db_path(db, Ok(file));
        Ok(())
    }

    pub fn add_absolute_mod_path(
        &mut self,
        db: &mut (impl Interner + ?Sized),
        path: PathBuf,
        name: &str,
    ) -> Result<(), FileLoadError> {
        let name = db.intern_identifier(IdentifierName::new(name.into()));
        let file = self.add_file(db, path, FileId::default(), Span::default(), name)?;
        self.absolute_mods.insert(name, file);
        Ok(())
    }
}

type CacheFn<'a> = Box<dyn for<'b> Fn(&'b usize) -> Result<String, Box<dyn Debug>> + 'a>;

pub struct AriadneCache<'a, DB: ?Sized> {
    db: &'a DB,
    fncache: FnCache<usize, CacheFn<'a>>,
}

impl<'a, DB: ?Sized> AriadneCache<'a, DB> {
    pub fn new(db: &'a DB) -> AriadneCache<'a, DB>
    where
        DB: Files,
    {
        let fun = Box::new(move |id: &usize| Ok(db.file_content(FileId(*id as u32)).to_string()))
            as CacheFn<'a>;
        let fncache = FnCache::new(fun);
        AriadneCache { db, fncache }
    }
}

impl<'a, DB> Cache<FileId> for AriadneCache<'a, DB>
where
    DB: Files + ?Sized,
{
    fn fetch(&mut self, id: &FileId) -> Result<&ariadne::Source, Box<dyn std::fmt::Debug + '_>> {
        self.fncache.fetch(&id.to_usize())
    }

    fn display<'b>(&self, id: &'b FileId) -> Option<Box<dyn std::fmt::Display + 'b>> {
        Some(Box::new(id.to_db_string(self.db)))
    }
}

fn file_content(db: &dyn Files, id: FileId) -> Arc<String> {
    let fd = db.input_file(id);
    fd.content.clone()
}

fn path(db: &dyn Files, id: FileId) -> Option<String> {
    let fd = db.input_file(id);
    fd.path.clone()
}

fn file_lines(db: &dyn Files, id: FileId) -> Arc<BTreeMap<usize, usize>> {
    let content = db.file_content(id);
    let start_ptr = content.as_ptr();
    let offset = |s: &str| (s.as_ptr() as usize) - (start_ptr as usize);
    let mut lines = BTreeMap::new();
    for (i, line) in content.lines().enumerate() {
        lines.insert(offset(line), i);
    }
    Arc::new(lines)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl LineColumn {
    pub fn new(line: usize, column: usize) -> Self {
        LineColumn { line, column }
    }
    pub fn one_based_line(&self) -> usize {
        self.line + 1
    }
    pub fn one_based_column(&self) -> usize {
        self.column + 1
    }
}

// gets the column and line number of the given span
// lines are inclusive on both ends, columns are inclusive on the start and exclusive on the end
fn file_offset_pos(db: &dyn Files, id: FileId, span: (usize, usize)) -> (LineColumn, LineColumn) {
    let lines = db.file_lines(id);
    let (start_line_offset, start_line) = lines.range(..=span.0).next_back().unwrap();
    let start = LineColumn::new(*start_line, span.0 - start_line_offset);
    let (end_line_offset, end_line) = lines.range(..span.1).next_back().unwrap_or((&0, &0));
    let end = LineColumn::new(*end_line, span.1 - end_line_offset);
    (start, end)
}

impl<DB: Files + ?Sized> DatabasedDisplay<DB> for FileId {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        let fd = db.input_file(*self);
        if let Some(path) = &fd.path {
            write!(f, "{path}")
        } else {
            write!(f, "file[_]")
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct SpanIndex(NonZeroU32);
impl SpanIndex {
    pub fn add_span(spans: &RefCell<Vec<Span>>) -> impl Fn(&Span) -> Self + '_ {
        |span: &Span| {
            let mut borrow = spans.borrow_mut();
            borrow.push(*span);
            SpanIndex(NonZeroU32::new(u32::try_from(borrow.len()).unwrap()).unwrap())
        }
    }

    pub fn new(n: usize) -> Vec<Self> {
        (1..=n)
            .map(|i| {
                SpanIndex(
                    NonZeroU32::new(u32::try_from(i).expect("overflow adding spans")).unwrap(),
                )
            })
            .collect()
    }

    pub fn as_usize(self) -> usize {
        u32::from(self.0) as usize - 1
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct IndexSpanned<T> {
    pub atom: T,
    pub span: SpanIndex,
}

impl<T: Clone + Eq + Hash + Debug> IndexSpanned<T> {
    pub fn new(spanned: &Spanned<T>, add_span: &impl Fn(&Span) -> SpanIndex) -> Self {
        let span = add_span(&spanned.span); // span
        Self {
            span,
            atom: spanned.inner.clone(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IndirectSpan(pub DefId, pub Option<SpanIndex>);

impl IndirectSpan {
    pub fn new(id: DefId, span: SpanIndex) -> Self {
        Self(id, Some(span))
    }

    pub fn default_span(id: DefId) -> Self {
        Self(id, None)
    }
}

#[derive(Debug)]
pub enum FileLoadError {
    LoadError {
        source: (FileId, Span),
        name: Identifier,
        error: Error,
    },
    DoesNotExist {
        source: (FileId, Span),
        name: Identifier,
    },
    NoStdLib {
        source: (FileId, Span),
        kind: LibKind,
    },
    Silenced,
}

impl FileLoadError {
    pub fn into_report(self, db: &(impl ?Sized + Interner)) -> Option<Report> {
        match self {
            FileLoadError::LoadError {
                source,
                name,
                error,
            } => Some(
                Report::new(
                    DiagnosticKind::Error,
                    source.0,
                    &dbformat!(db, "Could not load {}: {}", &name, &error),
                )
                .with_code(150)
                .with_label(Label::new(source.1).with_message("imported here")),
            ),
            FileLoadError::DoesNotExist { source, name } => Some(
                Report::new(
                    DiagnosticKind::Error,
                    source.0,
                    &dbformat!(db, "Could not find {}", &name),
                )
                .with_code(151)
                .with_label(Label::new(source.1).with_message("imported here")),
            ),
            FileLoadError::NoStdLib { source, kind } => Some(
                Report::new(
                    DiagnosticKind::Error,
                    source.0,
                    &format!("No {} library found", kind),
                )
                .with_code(152),
            ),
            FileLoadError::Silenced => None,
        }
    }
}
