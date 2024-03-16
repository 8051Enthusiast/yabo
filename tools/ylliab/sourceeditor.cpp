#include "sourceeditor.hpp"
#include <QTextBlock>
#include <qnamespace.h>
#include <QKeyCombination>

SourceEditor::SourceEditor(QWidget *parent) : QPlainTextEdit(parent) {
  setTabStopDistance(tab_width * QFontMetrics(font()).horizontalAdvance(' '));
}

void SourceEditor::keyPressEvent(QKeyEvent *event) {
  switch (event->key()) {
    SourceEditor::CurrentLineInfo info;
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
      qDebug() << info.cursor_offset << info.indent;
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

const QRegularExpression SourceEditor::indent_re = QRegularExpression("^\\s*");

SourceEditor::CurrentLineInfo SourceEditor::current_line_info() const {
  auto cursor = textCursor();
  auto pos = cursor.positionInBlock();
  auto line = cursor.block().text();
  auto match = indent_re.match(line);
  return {pos, (int)match.captured().size()};
}
