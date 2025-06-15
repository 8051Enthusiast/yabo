#pragma once

#include "color.hpp"
#include <QObject>
#include <QPlainTextEdit>
#include <QRegularExpression>
#include <QSyntaxHighlighter>
#include <QWidget>

class SourceEditor;

enum class LexerState : int {
  None = -1,
  AfterOperator = 0,
  NotAfterOperator = 1,
};

class Lexer {
public:
  Lexer();
  HighlightName match_next(const QString &str, size_t &offset,
                           LexerState &state);

private:
  struct Matcher {
    Matcher(const char *re, HighlightName highlight, LexerState next_state,
            LexerState cond_state = LexerState::None);
    Matcher(Matcher &&other) = default;
    std::unique_ptr<QRegularExpression> expr;
    HighlightName highlight;
    LexerState next_state;
    LexerState cond_state;
  };

  std::vector<Matcher> matchers;
};

class Highlighter : QSyntaxHighlighter {
public:
  Highlighter(QTextDocument *parent)
      : QSyntaxHighlighter(parent), lexer(Lexer()) {}

protected:
  void highlightBlock(const QString &text) override;

private:
  Lexer lexer;
};

class LineNumberArea : public QWidget {
public:
  LineNumberArea(SourceEditor *editor);

  QSize sizeHint() const override;

protected:
  void paintEvent(QPaintEvent *event) override;

private:
  SourceEditor *source_editor;
};

class SourceEditor : public QPlainTextEdit {
  Q_OBJECT
public:
  SourceEditor(QWidget *parent = nullptr);
  void keyPressEvent(QKeyEvent *event) override;

  int line_number_width();
  void line_number_area_paint_event(QPaintEvent *event);
  void set_font(const QFont &font);

protected:
  void resizeEvent(QResizeEvent *event) override;

private slots:
  void update_line_number_area_width(int newBlockCount);
  void update_line_number_area(const QRect &rect, int dy);

private:
  struct CurrentLineInfo {
    int cursor_offset;
    int indent;
  };
  CurrentLineInfo current_line_info() const;

  LineNumberArea *line_number_area;
  Highlighter *highlighter;
  static constexpr int tab_width = 2;
  static const QRegularExpression indent_re;
};