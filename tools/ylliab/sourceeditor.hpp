#pragma once

#include <QObject>
#include <QPlainTextEdit>
#include <QRegularExpression>

class SourceEditor : public QPlainTextEdit {
  Q_OBJECT
public:
  SourceEditor(QWidget *parent = nullptr);
  virtual void keyPressEvent(QKeyEvent *event) override;

private:
  struct CurrentLineInfo {
    int cursor_offset;
    int indent;
  };
  CurrentLineInfo current_line_info() const;
  static constexpr int tab_width = 2;
  static const QRegularExpression indent_re;
};