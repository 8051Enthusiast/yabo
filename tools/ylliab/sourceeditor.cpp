#include "sourceeditor.hpp"
#include <QFontMetrics>
#include <QKeyEvent>
#include <QPainter>
#include <QTextBlock>
#include <QTextCursor>
#include <QTextEdit>

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

  int space = left_line_number_pad + right_line_number_pad +
              fontMetrics().horizontalAdvance(QChar('0')) * digits;

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
  QColor background_color = palette.color(QPalette::Base);
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