#pragma once
#include <QWidget>
#include <QKeyEvent>

class MultiFileParser;
class MultiFileHexModel;

namespace Ui {
class MultiFileView;
}

class MultiFileView : public QWidget {
  Q_OBJECT

public:
  explicit MultiFileView(std::unique_ptr<MultiFileParser> parser,
                         QWidget *parent = nullptr);
  ~MultiFileView();

signals:
  void new_tab_requested(QWidget *widget, QString name);
  void ascii_mode_changed(bool ascii);

protected:
  void keyPressEvent(QKeyEvent *event) override;
  void keyReleaseEvent(QKeyEvent *event) override;

private slots:
  void on_actionCompile_triggered();
  void load_compiled_file(QString file);
  void on_error(QString error);
  void on_tableView_doubleClicked(const QModelIndex &index);

private:
  std::unique_ptr<MultiFileParser> parser;
  std::unique_ptr<MultiFileHexModel> model;
  QString current_lib_name;
  Ui::MultiFileView *ui;
};
