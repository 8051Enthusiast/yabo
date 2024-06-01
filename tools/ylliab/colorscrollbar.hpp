#pragma once
#include <QScrollBar>
#include <QObject>
#include <QWidget>

class HexTableModel;

class ColorScrollBar : public QScrollBar
{
    Q_OBJECT
public:
    ColorScrollBar(HexTableModel *model, QWidget *parent = nullptr);

public slots:
    void minimap_change();

signals:
    void big_jump(size_t addr);

protected:
    void paintEvent(QPaintEvent *event) override;
    void mousePressEvent(QMouseEvent *event) override;
    void mouseMoveEvent(QMouseEvent *event) override;
    void mouseReleaseEvent(QMouseEvent *event) override;

private:
    int marker_offset() const;
    int offset_value(int offset) const;
    void set_val(int offset);
    void refresh_minimap();
    HexTableModel *model;
    QPixmap minimap;
    bool minimap_updated;
    bool dragging;
};
