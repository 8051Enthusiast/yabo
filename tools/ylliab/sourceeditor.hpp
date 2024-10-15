#pragma once

#include <QObject>
#include <QPlainTextEdit>
#include <QRegularExpression>
#include <QWidget>

class SourceEditor;

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
  static constexpr int tab_width = 2;
  static const QRegularExpression indent_re;
};