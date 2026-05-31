#pragma once

#include <memory>
#include <QObject>
#include <QPlainTextEdit>
#include <QRegularExpression>
#include <QSyntaxHighlighter>
#include <QWidget>
#include <vector>

class SourceEditor;

class Highlighter : public QSyntaxHighlighter {
public:
  explicit Highlighter(QTextDocument *parent);
  ~Highlighter() override;

protected:
  void highlightBlock(const QString &text) override;


private:
  struct HighlightRange;
  class TreeSitterSyntax;
  enum class RehighlightStatus {
    None,
    InProgress,
  };

  void refresh_highlights();
  void schedule_rehighlight(int position, int chars_removed, int chars_added);

  std::unique_ptr<TreeSitterSyntax> syntax;
  std::vector<HighlightRange> highlights;
  RehighlightStatus rehighlight_status = RehighlightStatus::None;
  std::set<int> dirty{};
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
