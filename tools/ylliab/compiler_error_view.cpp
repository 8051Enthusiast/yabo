#include "compiler_error_view.hpp"
#include "color.hpp"

#include <QRegularExpressionMatchIterator>
#include <QTextCharFormat>
#include <QTextCursor>
#include <QTextDocument>

CompilerErrorView::CompilerErrorView(QWidget *parent) : QTextEdit(parent) {
  ansi_regex.setPattern("(^|\\x1b\\[([0-9;]*)m)([^\\x1b]*)");
  setReadOnly(true);
}

void CompilerErrorView::set_error(const QString &error) {
  clear();
  QTextCursor cursor = textCursor();
  auto matches = ansi_regex.globalMatch(error);
  auto default_format = cursor.charFormat();
  auto format = default_format;
  while (matches.hasNext()) {
    auto match = matches.next();
    auto text = match.captured(3);
    if (match.captured(1).isEmpty()) {
      cursor.insertText(text, format);
      continue;
    }
    auto codes = match.captured(2).split(";");
    if (codes.size() == 1) {
      auto num = codes[0].toInt();
      if (num >= 30 && num <= 37) {
        format.setForeground(ansi_color_16(num - 30));
      } else if (num >= 40 && num <= 47) {
        format.setBackground(ansi_color_16(num - 40));
      } else if (num == 1) {
        format.setFontWeight(QFont::Bold);
      } else if (num == 4) {
        format.setFontUnderline(true);
      } else if (num == 0) {
        format = default_format;
      }
    } else if (codes.size() == 3) {
      if (codes[0] == "38" && codes[1] == "5") {
        format.setForeground(ansi_color_256(codes[2].toInt()));
      }
    }
    cursor.insertText(text, format);
  }
  cursor.setCharFormat(default_format);
}
