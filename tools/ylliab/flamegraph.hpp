#pragma once
#include <QFont>
#include <QWidget>
#include <memory>
#include <qevent.h>

enum class SquareKind : uint8_t {
  Root,
  Byte,
  Eof,
};

struct Square {
  std::vector<Square> children{};
  unsigned int row{};
  unsigned int start{};
  unsigned int width{};
  SquareKind kind{};
  uint8_t value{};
};

struct FileUpdate;

class FlameGraph : public QWidget {
  Q_OBJECT

public:
  explicit FlameGraph(QWidget *parent = nullptr);
  ~FlameGraph();

  bool hasHeightForWidth() const override;
  int heightForWidth(int) const override;

  void update_on_size_change();
  void set_font(const QFont &font);

signals:
  void jump_to_pos(size_t row);
public slots:
  void get_update(std::shared_ptr<FileUpdate>);

protected:
  void resizeEvent(QResizeEvent *event) override;
  void paintEvent(QPaintEvent *event) override;
  void mouseDoubleClickEvent(QMouseEvent *event) override;

private:
  std::pair<size_t, size_t> range_for_current_prefix() const;
  void update_for_size(unsigned int width);
  QRect box_for_node(const Square &node) const;
  void paint_square(QPainter &painter, const Square &square) const;
  void focus_on(QPoint pos);

  std::shared_ptr<FileUpdate> update;
  std::unique_ptr<Square> root;
  std::vector<uint8_t> chosen_prefix{};
  static constinit const int row_height = 20;

  QFont hex_font;
  int cached_line_height = 20;
  int cached_hex_digit_width = 10;
};