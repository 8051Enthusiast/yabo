#include "sourceeditor.hpp"
#include "color.hpp"
#include "sourceeditor_highlights.hpp"
#include <QFontMetrics>
#include <QKeyEvent>
#include <QPainter>
#include <QRegularExpression>
#include <QTextBlock>
#include <QTextCursor>
#include <QTextDocument>
#include <QTextEdit>
#include <QTimer>
#include <algorithm>
#include <bit>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <stdexcept>
#include <string_view>
#include <tree_sitter/api.h>
#include <tree_sitter/tree-sitter-yabo.h>
#include <unistd.h>

namespace {

HighlightName highlight_for_capture(std::string_view name) {
  if (name.starts_with("comment")) {
    return HighlightName::Comment;
  }
  if (name.starts_with("constant.builtin")) {
    return HighlightName::ConstantLanguage;
  }
  if (name.starts_with("constant.numeric")) {
    return HighlightName::ConstantNumeric;
  }
  if (name.starts_with("constant.character")) {
    return HighlightName::StringQuotedSingle;
  }
  if (name.starts_with("constant.string")) {
    return HighlightName::StringQuotedDouble;
  }
  if (name.starts_with("string.regexp")) {
    return HighlightName::StringRegexp;
  }
  if (name.starts_with("keyword.operator")) {
    return HighlightName::KeywordOperator;
  }
  if (name.starts_with("keyword.control")) {
    return HighlightName::KeywordControl;
  }
  if (name.starts_with("keyword")) {
    return HighlightName::Keyword;
  }
  if (name.starts_with("operator")) {
    return HighlightName::Operator;
  }
  if (name.starts_with("punctuation")) {
    return HighlightName::Punctuation;
  }
  if (name.starts_with("type")) {
    return HighlightName::Type;
  }
  if (name.starts_with("function") || name.starts_with("namespace")) {
    return HighlightName::Entity;
  }
  if (name.starts_with("variable")) {
    return HighlightName::Variable;
  }
  return HighlightName::None;
}

static constexpr TSInputEncoding qstring_encoding =
    std::endian::little == std::endian::native ? TSInputEncodingUTF16LE
                                               : TSInputEncodingUTF16BE;

const char *qstring_read(void *payload, uint32_t byte_index, TSPoint position,
                         uint32_t *bytes_read) {
  const auto &string = *(const QString *)payload;
  size_t byte_size = string.size() * sizeof(QChar);
  auto *string_data = reinterpret_cast<const char *>(string.constData());
  auto actual_byte_index = std::min(static_cast<uint64_t>(byte_index),
                                    static_cast<uint64_t>(byte_size));
  *bytes_read =
      std::min(byte_size - actual_byte_index,
               static_cast<uint64_t>(std::numeric_limits<uint32_t>::max()));
  return string_data + actual_byte_index;
}

TSInput qstring_input(const QString &string) {
  return {
      .payload = (void *)(&string),
      .read = qstring_read,
      .encoding = qstring_encoding,
      .decode = nullptr,
  };
}

class ChangedRanges {
public:
  ChangedRanges(TSTree *old_tree, TSTree *new_tree) {
    uint32_t length = 0;
    auto *ranges = ts_tree_get_changed_ranges(old_tree, new_tree, &length);
    changed_ranges = std::span{ranges, length};
  }

  explicit ChangedRanges(TSRange range) {
    TSRange *alloc_range = (TSRange *)malloc(sizeof(TSRange));
    *alloc_range = range;
    changed_ranges = std::span(alloc_range, 1);
  }

  ChangedRanges(const ChangedRanges &) = delete;
  ChangedRanges &operator=(const ChangedRanges &) = delete;
  ChangedRanges(ChangedRanges &&new_ranges) {
    std::swap(this->changed_ranges, new_ranges.changed_ranges);
  }
  ChangedRanges &operator=(ChangedRanges &&new_ranges) {
    std::swap(this->changed_ranges, new_ranges.changed_ranges);
    return *this;
  }
  ChangedRanges() = default;

  ~ChangedRanges() {
    if (changed_ranges.data()) {
      free(changed_ranges.data());
    }
  }

  std::set<int> changed_blocks(const QTextDocument &doc) {
    std::set<int> res{};
    for (auto range : changed_ranges) {
      auto start = range.start_byte / 2;
      auto end = range.end_byte / 2;
      if (start == end) {
        continue;
      }

      auto start_block = doc.findBlock(start).blockNumber();
      auto end_block = doc.findBlock(end - 1).blockNumber();
      res.insert_range(std::views::iota(start_block, end_block + 1));
    }

    return res;
  }

private:
  std::span<TSRange> changed_ranges;
};

} // namespace

struct Highlighter::HighlightRange {
  uint32_t start;
  uint32_t end;
  HighlightName highlight;
};

class Highlighter::TreeSitterSyntax {
public:
  TreeSitterSyntax() {
    parser = ts_parser_new();
    if (!parser) {
      throw std::runtime_error("Could not create parser");
    }
    if (!ts_parser_set_language(parser, tree_sitter_yabo())) {
      throw std::runtime_error("Could not set language");
    }

    uint32_t error_offset = 0;
    TSQueryError error_type = TSQueryErrorNone;
    query = ts_query_new(tree_sitter_yabo(), YABO_HIGHLIGHTS_QUERY,
                         std::strlen(YABO_HIGHLIGHTS_QUERY), &error_offset,
                         &error_type);
    if (!query) {
      throw std::runtime_error("Could not parse query");
    }

    tree = nullptr;
  }

  ~TreeSitterSyntax() {
    ts_query_delete(query);
    ts_parser_delete(parser);
    if (tree) {
      ts_tree_delete(tree);
    }
  }

  std::pair<std::vector<HighlightRange>, ChangedRanges>
  parse(const QString &text) {
    struct QueryCursorDeleter {
      void operator()(TSQueryCursor *cursor) const {
        ts_query_cursor_delete(cursor);
      }
    };

    std::vector<HighlightRange> result{};

    auto new_tree = ts_parser_parse(parser, tree, qstring_input(text));
    if (!new_tree) {
      throw std::runtime_error("Could not parse");
    }
    ChangedRanges changed;
    if (tree) {
      changed = ChangedRanges(new_tree, tree);
    } else {
      changed = ChangedRanges(TSRange{
          .start_point = {.row = 1, .column = 1},
          .end_point = {.row = 1, .column = (uint32_t)(text.size() + 1)},
          .start_byte = 0,
          .end_byte = (uint32_t)(text.size() * 2),
      });
    }
    auto old_tree = tree;
    tree = new_tree;
    if (old_tree) {
      ts_tree_delete(old_tree);
    }

    std::unique_ptr<TSQueryCursor, QueryCursorDeleter> cursor(
        ts_query_cursor_new());
    ts_query_cursor_exec(cursor.get(), query, ts_tree_root_node(tree));

    TSQueryMatch match;
    uint32_t capture_index = 0;
    while (ts_query_cursor_next_capture(cursor.get(), &match, &capture_index)) {
      const TSQueryCapture &capture = match.captures[capture_index];
      uint32_t name_length = 0;
      const char *name_data =
          ts_query_capture_name_for_id(query, capture.index, &name_length);
      std::string_view name(name_data, name_length);
      HighlightName highlight = highlight_for_capture(name);
      if (highlight == HighlightName::None) {
        continue;
      }

      uint32_t start_byte = ts_node_start_byte(capture.node);
      uint32_t end_byte = ts_node_end_byte(capture.node);
      result.push_back({.start = start_byte / 2,
                        .end = end_byte / 2,
                        .highlight = highlight});
    }

    std::reverse(result.begin(), result.end());
    return {result, std::move(changed)};
  }

  void edit(uint32_t start, uint32_t old_end, uint32_t new_end) {
    if (tree) {
      TSInputEdit edit{.start_byte = start * 2,
                       .old_end_byte = old_end * 2,
                       .new_end_byte = new_end * 2,
                       // we don't particularly care about these, so just
                       // pretend everything is on one row. our read
                       // implementation ignores these too.
                       .start_point = {.row = 1, .column = start + 1},
                       .old_end_point =
                           {
                               .row = 1,
                               .column = old_end + 1,
                           },
                       .new_end_point = {
                           .row = 1,
                           .column = new_end + 1,
                       }};
      ts_tree_edit(tree, &edit);
    }
  }

private:
  TSParser *parser{};
  TSQuery *query{};
  TSTree *tree{};
};

Highlighter::Highlighter(QTextDocument *parent)
    : QSyntaxHighlighter(parent), syntax(std::make_unique<TreeSitterSyntax>()) {
  connect(document(), &QTextDocument::contentsChange, this,
          &Highlighter::schedule_rehighlight);
}

Highlighter::~Highlighter() = default;

void Highlighter::refresh_highlights() {
  auto &doc = *document();
  auto [new_highlights, changed] = syntax->parse(doc.toPlainText());
  highlights = std::move(new_highlights);
  auto changed_blocks = changed.changed_blocks(doc);
  changed_blocks.insert_range(dirty);
  dirty.clear();
  for (auto block_index : changed_blocks) {
    auto block = doc.findBlockByLineNumber(block_index);
    rehighlightBlock(block);
  }
}

void Highlighter::schedule_rehighlight(int position, int chars_removed,
                                       int chars_added) {
  syntax->edit((uint32_t)position, (uint32_t)(position + chars_removed),
               (uint32_t)(position + chars_added));
  rehighlight_status = RehighlightStatus::InProgress;
  refresh_highlights();
  rehighlight_status = RehighlightStatus::None;
}

void Highlighter::highlightBlock(const QString &text) {
  int block_start = currentBlock().position();
  int block_end = block_start + text.length();
  int block_num = currentBlock().blockNumber();
  if (rehighlight_status != RehighlightStatus::InProgress) {
    dirty.insert(block_num);
    return;
  }

  auto start = std::lower_bound(
      highlights.begin(), highlights.end(), block_end,
      [](const auto &range, int end) { return range.start > end; });
  for (const HighlightRange &highlight :
       std::ranges::subrange(start, highlights.end())) {
    if (highlight.end <= block_start) {
      break;
    }

    int start = std::max(highlight.start, static_cast<uint32_t>(block_start));
    int end = std::min(highlight.end, static_cast<uint32_t>(block_end));
    if (start < end) {
      setFormat(start - block_start, end - start,
                style_color(highlight.highlight));
    }
  }
}

LineNumberArea::LineNumberArea(SourceEditor *editor)
    : QWidget(editor), source_editor(editor) {}

QSize LineNumberArea::sizeHint() const {
  return QSize(source_editor->line_number_width(), 0);
}

void LineNumberArea::paintEvent(QPaintEvent *event) {
  source_editor->line_number_area_paint_event(event);
}

SourceEditor::SourceEditor(QWidget *parent) : QPlainTextEdit(parent) {
  setTabStopDistance(tab_width * QFontMetrics(font()).horizontalAdvance(' '));

  line_number_area = new LineNumberArea(this);

  connect(this, &QPlainTextEdit::blockCountChanged, this,
          &SourceEditor::update_line_number_area_width);
  connect(this, &QPlainTextEdit::updateRequest, this,
          &SourceEditor::update_line_number_area);
  connect(this, &QPlainTextEdit::cursorPositionChanged, this,
          [this]() { update_line_number_area(viewport()->rect(), 0); });

  line_number_area->setFont(font());
  highlighter = new Highlighter(document());

  auto bg = style_color(HighlightName::None);
  auto stylesheet =
      QString("QPlainTextEdit {background-color: %1;}").arg(bg.name());
  setStyleSheet(stylesheet);
  update_line_number_area_width(0);
}

void SourceEditor::keyPressEvent(QKeyEvent *event) {
  switch (event->key()) {
    CurrentLineInfo info;
  case Qt::Key_Return:
    info = current_line_info();
    insertPlainText("\n");
    insertPlainText(QString(info.indent, ' '));
    break;
  case Qt::Key_Tab:
    info = current_line_info();
    insertPlainText(QString(tab_width - (info.cursor_offset % tab_width), ' '));
    break;
  case Qt::Key_Backspace:
    info = current_line_info();
    if (info.cursor_offset <= info.indent && info.indent >= tab_width &&
        info.cursor_offset % tab_width == 0) {
      auto cursor = textCursor();
      cursor.movePosition(QTextCursor::Left, QTextCursor::KeepAnchor,
                          tab_width);
      cursor.removeSelectedText();
    } else {
      QPlainTextEdit::keyPressEvent(event);
    }
    break;
  default:
    QPlainTextEdit::keyPressEvent(event);
  }
}

static constexpr int left_line_number_pad = 4;
static constexpr int right_line_number_pad = 10;

int SourceEditor::line_number_width() {
  int digits = 1;
  int max = qMax(999, blockCount());
  while (max >= 10) {
    max /= 10;
    ++digits;
  }
  auto fnt = QFontMetrics(font());
  int space = left_line_number_pad + right_line_number_pad +
              fnt.horizontalAdvance(QChar('0')) * digits;

  return space;
}

void SourceEditor::update_line_number_area_width(int) {
  setViewportMargins(line_number_width(), 0, 0, 0);
}

void SourceEditor::resizeEvent(QResizeEvent *event) {
  QPlainTextEdit::resizeEvent(event);

  QRect cr = contentsRect();
  line_number_area->setGeometry(
      QRect(cr.left(), cr.top(), line_number_width(), cr.height()));
}

void SourceEditor::line_number_area_paint_event(QPaintEvent *event) {
  QPainter painter(line_number_area);

  QPalette palette = line_number_area->palette();
  QColor background_color = style_color(HighlightName::None);
  QColor text_color = palette.color(QPalette::ButtonText);
  QColor inactive_text_color = text_color;
  inactive_text_color.setAlphaF(0.5);

  painter.fillRect(event->rect(), background_color);

  QTextBlock block = firstVisibleBlock();
  int block_number = block.blockNumber();
  int top =
      qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
  int bottom = top + qRound(blockBoundingRect(block).height());
  int current_line = textCursor().blockNumber();

  while (block.isValid() && top <= event->rect().bottom()) {
    if (block.isVisible() && bottom >= event->rect().top()) {
      QString number = QString::number(block_number + 1);
      if (block_number == current_line) {
        painter.setPen(text_color);
      } else {
        painter.setPen(inactive_text_color);
      }
      painter.drawText(0, top,
                       line_number_area->width() - right_line_number_pad,
                       fontMetrics().height(), Qt::AlignRight, number);
    }

    block = block.next();
    top = bottom;
    bottom = top + static_cast<int>(blockBoundingRect(block).height());
    ++block_number;
  }
}

void SourceEditor::update_line_number_area(const QRect &rect, int dy) {
  if (dy)
    line_number_area->scroll(0, dy);
  else
    line_number_area->update(0, rect.y(), line_number_area->width(),
                             rect.height());

  if (rect.contains(viewport()->rect()))
    update_line_number_area_width(0);
}

const QRegularExpression SourceEditor::indent_re = QRegularExpression("^\\s*");

SourceEditor::CurrentLineInfo SourceEditor::current_line_info() const {
  auto cursor = textCursor();
  auto pos = cursor.positionInBlock();
  auto line = cursor.block().text();
  auto match = indent_re.match(line);
  return {pos, (int)match.captured().size()};
}

void SourceEditor::set_font(const QFont &font) {
  setFont(font);
  line_number_area->setFont(font);
  update_line_number_area(viewport()->rect(), 0);
}
