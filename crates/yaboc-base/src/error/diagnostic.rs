use crate::source::{FileId, Files, Span};
use serde::Serialize;

#[derive(Clone)]
pub struct Label {
    pub span: Span,
    pub message: Option<String>,
}

impl Label {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            message: None,
        }
    }
    pub fn with_message(mut self, message: &str) -> Self {
        self.message = Some(message.to_string());
        self
    }
    pub fn add_message(&mut self, message: &str) {
        self.message = Some(message.to_string());
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub enum DiagnosticKind {
    Error,
    Warning,
}

#[derive(Clone)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub file: FileId,
    pub code: Option<u32>,
    pub message: String,
    pub labels: Vec<Label>,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, file: FileId, message: &str) -> Self {
        Self {
            kind,
            file,
            code: None,
            message: message.to_string(),
            labels: Vec::new(),
        }
    }
    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }
    pub fn add_label(&mut self, label: Label) {
        self.labels.push(label);
    }
    pub fn with_code(mut self, code: u32) -> Self {
        self.code = Some(code);
        self
    }
    pub fn add_code(&mut self, code: u32) {
        self.code = Some(code);
    }
    pub fn into_ariadne(self) -> ariadne::Report<Span> {
        let mut diag = ariadne::Report::build(ariadne::ReportKind::Error, self.file, 0)
            .with_message(self.message);
        if let Some(code) = self.code {
            diag = diag.with_code(code);
        }
        for label in self.labels {
            let label = match label.message {
                Some(msg) => ariadne::Label::new(label.span).with_message(msg),
                None => ariadne::Label::new(label.span),
            };
            diag = diag.with_label(label);
        }
        diag.with_config(ariadne::Config::default().with_compact(true))
            .finish()
    }
    pub fn into_json(self, files: &(impl Files + ?Sized)) -> JsonDiagnostic {
        JsonDiagnostic::new(&self, files)
    }
}

#[derive(Serialize)]
struct JsonLabel {
    file_path: String,
    start: usize,
    end: usize,
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
    message: Option<String>,
    content: String,
}

#[derive(Serialize)]
pub struct JsonDiagnostic {
    kind: DiagnosticKind,
    file_path: String,
    code: Option<u32>,
    message: String,
    labels: Vec<JsonLabel>,
}

impl JsonDiagnostic {
    fn new(diag: &Diagnostic, files: &(impl Files + ?Sized)) -> Self {
        let file_path = files
            .path(diag.file)
            .unwrap_or_else(|| String::from("[anon]"));
        let labels = diag
            .labels
            .iter()
            .map(|label| {
                let (start_lc, end_lc) = files
                    .file_offset_pos(diag.file, (label.span.lo as usize, label.span.hi as usize));
                let start_line = start_lc.one_based_line();
                let start_col = start_lc.one_based_column();
                let end_line = end_lc.one_based_line();
                let end_col = end_lc.one_based_column();
                let start = label.span.lo as usize;
                let end = label.span.hi as usize;
                let file_content = files.file_content(diag.file);
                let content = String::from(&file_content[start..end]);
                JsonLabel {
                    file_path: file_path.clone(),
                    start,
                    end,
                    start_line,
                    start_col,
                    end_line,
                    end_col,
                    message: label.message.clone(),
                    content,
                }
            })
            .collect();
        JsonDiagnostic {
            kind: diag.kind,
            file_path,
            code: diag.code,
            message: diag.message.clone(),
            labels,
        }
    }
}
