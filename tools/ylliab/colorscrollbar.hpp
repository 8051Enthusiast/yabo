#pragma once
#include <QScrollBar>
#include <QObject>
#include <QWidget>

#include "hex.hpp"

class ColorScrollBar : public QScrollBar
{
    Q_OBJECT
public:
    ColorScrollBar(HexTableModel *model, QWidget *parent = nullptr);

public slots:
    void minimap_change();

protected:
    void paintEvent(QPaintEvent *event) override;
    void mousePressEvent(QMouseEvent *event) override;
    void mouseMoveEvent(QMouseEvent *event) override;

private:
    int marker_offset() const;
    int offset_value(int offset) const;
    void refresh_minimap();
    HexTableModel *model;
    QPixmap minimap;
    bool minimap_updated;
};
