#pragma once
#include <QWidget>
#include <memory>

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

public slots:
  void get_update(std::shared_ptr<FileUpdate>);

protected:
  void resizeEvent(QResizeEvent *event) override;
  void paintEvent(QPaintEvent *event) override;

private:
  std::pair<size_t, size_t> range_for_current_prefix() const;
  void update_for_size(unsigned int width);
  QRect box_for_node(const Square &node) const;
  void paintSquare(QPainter &painter, const Square &square);

  std::shared_ptr<FileUpdate> update;
  std::unique_ptr<Square> root;
  std::vector<uint8_t> chosen_prefix{};
  static constinit const int row_height = 20;
};