#include "flamegraph.hpp"
#include "color.hpp"
#include "multifilemodel.hpp"
#include <QFontMetrics>
#include <QPaintEvent>
#include <QPainter>
#include <QResizeEvent>
#include <qwidget.h>

namespace {
bool can_reach(const Square &square, unsigned int row, unsigned int offset) {
  return row >= square.row && offset >= square.start &&
         offset < square.start + square.width;
}

bool get_prefix(const Square &parent, std::vector<uint8_t> &prefix,
                unsigned int row, unsigned int offset) {
  for (const auto &child : parent.children) {
    if (!can_reach(child, row, offset)) {
      continue;
    }

    if (child.kind == SquareKind::Byte) {
      prefix.push_back(child.value);
    }

    if (child.row == row) {
      return true;
    }

    [[clang::musttail]] return get_prefix(child, prefix, row, offset);
  }
  return false;
}

size_t overlap(ByteSpan lhs, ByteSpan rhs, size_t max_overlap) {
  size_t i = 0;
  while (i < max_overlap && i < lhs.size() && i < rhs.size() &&
         lhs[i] == rhs[i]) {
    i++;
  }
  if (lhs.size() == rhs.size() && i == lhs.size()) {
    return i + 1;
  }
  return i;
}

std::optional<uint8_t> span_get(ByteSpan span, size_t index) {
  if (index >= span.size()) {
    return {};
  }
  return span[index];
}

size_t pos_for_pixel_offset(size_t start_pos, size_t end_pos,
                            unsigned int width, unsigned int i) {
  if (width <= 1) {
    return start_pos;
  }
  if (i == width) {
    return end_pos - 1;
  }
  auto count = static_cast<uint64_t>(end_pos - start_pos);
  auto offset = count * static_cast<uint64_t>(i) / static_cast<uint64_t>(width);
  return start_pos + offset;
}

constinit size_t MAX_OVERLAP = 64;

size_t find_row_count(const FileUpdate &update, unsigned int width,
                      size_t start_pos, size_t end_pos) {

  size_t total_overlap = 0;
  size_t old_overlap = 0;
  ByteSpan old_span{};
  for (unsigned int i = 0; i < width; i++) {
    auto pos = pos_for_pixel_offset(start_pos, end_pos, width, i);
    auto index = update.order.ordered_to_index[pos];
    auto new_span = update.span_at_index(index);
    auto new_overlap = overlap(old_span, new_span, MAX_OVERLAP);
    total_overlap = std::max(new_overlap, total_overlap);
    old_overlap = new_overlap;
    old_span = new_span;
  }
  return total_overlap;
}

Square starting_square(unsigned int start, unsigned int row,
                       std::optional<uint8_t> mvalue) {
  auto kind = mvalue.has_value() ? SquareKind::Byte : SquareKind::Eof;
  auto value = mvalue.value_or(0);
  return Square{
      .children = {},
      .row = row,
      .start = start,
      .width = 1,
      .kind = kind,
      .value = value,
  };
}

Square find_squares(const FileUpdate &update, unsigned int width,
                    size_t start_pos, size_t end_pos) {
  std::vector<Square> common{Square{.kind = SquareKind::Root}};

  auto add = [&common](unsigned int i, std::optional<uint8_t> mvalue) {
    auto row = common.back().row + 1;
    common.emplace_back(starting_square(i, row, mvalue));
  };

  auto remove = [&common]() {
    const auto last = std::move(common.back());
    common.pop_back();
    common.back().children.emplace_back(std::move(last));
  };

  size_t old_overlap = 0;
  ByteSpan old_span{};
  for (unsigned int i = 0; i < width; i++) {
    auto pos = pos_for_pixel_offset(start_pos, end_pos, width, i);
    auto index = update.order.ordered_to_index[pos];
    auto new_span = update.span_at_index(index);
    auto new_overlap = overlap(old_span, new_span, MAX_OVERLAP);

    if (new_overlap > old_overlap) {
      for (size_t j = old_overlap; j < new_overlap; j++) {
        add(i - 1, span_get(new_span, j));
      }
    } else {
      for (size_t j = new_overlap; j < old_overlap; j++) {
        remove();
      }
    }

    for (auto &element : common) {
      element.width++;
    }

    old_overlap = new_overlap;
    old_span = new_span;
  }

  auto size = common.size();
  for (int i = 1; i < size; i++) {
    remove();
  }

  return std::move(common[0]);
}
} // namespace

FlameGraph::FlameGraph(QWidget *parent) : QWidget(parent) {
  setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);

  QFont hex_font("Monospace");
  hex_font.setStyleHint(QFont::TypeWriter);
  hex_font.setPointSize(10);
  set_font(hex_font);
}

FlameGraph::~FlameGraph() {}

void FlameGraph::update_on_size_change() {
  update_for_size(width());
  auto height = heightForWidth(width());
  setFixedHeight(height);
  updateGeometry();
}

void FlameGraph::set_font(const QFont &font) {
  hex_font = font;
  QFontMetrics fm(font);
  cached_line_height = fm.height();
  cached_hex_digit_width = fm.horizontalAdvance("00");

  update_on_size_change();
  QWidget::update();
}

void FlameGraph::resizeEvent(QResizeEvent *event) {
  if (event->size().width() != event->oldSize().width()) {
    update_on_size_change();
  }
  QWidget::resizeEvent(event);
}

std::pair<size_t, size_t> FlameGraph::range_for_current_prefix() const {
  auto start = update->lower_bound(chosen_prefix);
  auto end = update->upper_bound(chosen_prefix);
  return {start, end};
}

void FlameGraph::update_for_size(unsigned int width) {
  if (!update) {
    return;
  }
  auto [start, end] = range_for_current_prefix();
  if (width <= 1 || start == end) {
    root = nullptr;
    return;
  }
  root = std::make_unique<Square>(find_squares(*update, width, start, end));
}

bool FlameGraph::hasHeightForWidth() const { return true; }

int FlameGraph::heightForWidth(int width) const {
  if (!update) {
    return 0;
  }
  auto [start, end] = range_for_current_prefix();
  if (width <= 1 || start == end) {
    return 0;
  }
  auto rows = find_row_count(*update, width, start, end) + 1;
  return rows * cached_line_height;
}

QRect FlameGraph::box_for_node(const Square &node) const {
  auto row_start = (node.row) * cached_line_height;
  return QRect{(int)node.start, (int)row_start, (int)node.width - 1,
               cached_line_height - 1};
}

void FlameGraph::get_update(std::shared_ptr<FileUpdate> update) {
  this->update = update;
  update_for_size(width());

  auto height = heightForWidth(width());
  setFixedHeight(height);
  updateGeometry();
  QWidget::update();
}

void FlameGraph::paintEvent(QPaintEvent *event) {
  QPainter painter(this);
  painter.setRenderHint(QPainter::Antialiasing, false);

  if (!root) {
    return;
  }

  paint_square(painter, *root);
}

void FlameGraph::paint_square(QPainter &painter, const Square &square) const {
  QRect rect = box_for_node(square);

  if (square.kind == SquareKind::Root) {
    painter.setBrush(QColor(200, 200, 200));
    painter.setPen(QColor(100, 100, 100));
  } else if (square.kind == SquareKind::Eof) {
    painter.setBrush(QColor(255, 200, 200));
    painter.setPen(QColor(200, 100, 100));
  } else {
    painter.setBrush(byte_wheel_color(square.value));
    painter.setPen(byte_wheel_outline_color(square.value));
  }

  painter.drawRect(rect);

  painter.setFont(hex_font);
  QString text;

  if (square.kind == SquareKind::Root) {
    text = "^";
  } else if (square.kind == SquareKind::Eof) {
    text = "$";
  } else {
    text = QString("%1").arg(square.value, 2, 16, QChar('0')).toUpper();
  }

  if (cached_hex_digit_width <= rect.width()) {
    painter.setPen(Qt::black);
    painter.drawText(rect, Qt::AlignCenter, text);
  }

  for (const auto &child : square.children) {
    paint_square(painter, child);
  }
}

void FlameGraph::focus_on(QPoint pos) {
  auto row = pos.y() / cached_line_height;
  auto new_prefix = std::vector<uint8_t>{};

  if (row != 0) {
    auto res = get_prefix(*root, new_prefix, row, pos.x());
    if (!res) {
      return;
    }
  }

  chosen_prefix = std::move(new_prefix);
  update_on_size_change();
  QWidget::update();
}

void FlameGraph::mouseDoubleClickEvent(QMouseEvent *event) {
  auto pos = event->pos();
  focus_on(pos);
  auto row = range_for_current_prefix().first;
  emit jump_to_pos(row);
}
