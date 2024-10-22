#include "sourceeditor.hpp"
#include <QFontMetrics>
#include <QKeyEvent>
#include <QPainter>
#include <QRegularExpression>
#include <QTextBlock>
#include <QTextCursor>
#include <QTextEdit>
#include <qregularexpression.h>

Lexer::Matcher::Matcher(const char *re, HighlightName highlight,
                        LexerState next_state, LexerState cond_state)
    : expr(std::make_unique<QRegularExpression>(re)), highlight(highlight),
      next_state(next_state), cond_state(cond_state){};

Lexer::Lexer() {
  Matcher matcher_list[] = {
      Matcher(R"((true|false|eof)\b)", HighlightName::ConstantLanguage,
              LexerState::NotAfterOperator),
      Matcher(R"((bit|int|char)\b)", HighlightName::Type,
              LexerState::NotAfterOperator),
      Matcher(R"((or|and|at|span)\b)", HighlightName::KeywordOperator,
              LexerState::AfterOperator),
      Matcher(R"(sizeof\b)", HighlightName::KeywordOperator,
              LexerState::NotAfterOperator),
      Matcher(R"((fun|def|static|let)\b)", HighlightName::Keyword,
              LexerState::None),
      Matcher(R"((if|else|then|import|export|return)\b)",
              HighlightName::Keyword, LexerState::AfterOperator),
      Matcher(R"(\(|\[|\{|\{\|)", HighlightName::Punctuation,
              LexerState::AfterOperator),
      Matcher(R"(\)|\]|\}|\|\})", HighlightName::Punctuation,
              LexerState::NotAfterOperator),
      Matcher(R"(,)", HighlightName::Punctuation, LexerState::AfterOperator),
      Matcher(R"(h?/([^/]|\\.)*/)", HighlightName::StringRegexp,
              LexerState::NotAfterOperator, LexerState::AfterOperator),
      Matcher(R"([A-Za-z_][A-Za-z_0-9]*)", HighlightName::Variable,
              LexerState::NotAfterOperator),
      Matcher(R"(-?(0x[0-9a-fA-F]+|0b[01]+|0o[0-7]+|[0-9]+))",
              HighlightName::ConstantNumeric, LexerState::NotAfterOperator),
      Matcher(R"('([^']|\\.)')", HighlightName::StringQuotedSingle,
              LexerState::NotAfterOperator),
      Matcher(
          R"(==|<=|>=|!=|>>|<<|[&]>|~>|->|<|>|~|!|\?|:|\.\.|[.]|[|]|=|[+]|-|[*]|/|%|\^|\&|\\)",
          HighlightName::Operator, LexerState::AfterOperator)};
  std::vector<Matcher> match{std::make_move_iterator(std::begin(matcher_list)),
                             std::make_move_iterator(std::end(matcher_list))};
  matchers = std::move(match);
}

HighlightName Lexer::match_next(const QString &str, size_t &offset,
                                LexerState &state) {
  while (offset < str.length() && str[offset].isSpace()) {
    offset++;
  }
  if (offset == str.length()) {
    return HighlightName::None;
  }

  if (str[offset] == '#') {
    // this is special case because we want to keep LexerState unchanged
    offset = str.length();
    return HighlightName::Comment;
  }

  int idx = 0;
  for (const auto &matcher : matchers) {
    if (matcher.cond_state != LexerState::None && matcher.cond_state != state &&
        state != LexerState::None) {
      continue;
    }

    auto match =
        matcher.expr->match(str, offset, QRegularExpression::NormalMatch,
                            QRegularExpression::AnchorAtOffsetMatchOption);
    if (match.hasMatch()) {
      offset += match.capturedLength();
      state = matcher.next_state;
      return matcher.highlight;
    }
    idx++;
  }

  offset += 1;
  state = LexerState::None;
  return HighlightName::Invalid;
};

void Highlighter::highlightBlock(const QString &text) {
  size_t offset = 0;
  LexerState state = LexerState(previousBlockState());
  while (offset != text.length()) {
    size_t start_offset = offset;
    HighlightName highlight = lexer.match_next(text, offset, state);
    if (highlight == HighlightName::None) {
      continue;
    }

    setFormat(start_offset, offset - start_offset, style_color(highlight));
  }
  setCurrentBlockState(static_cast<int>(state));
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
