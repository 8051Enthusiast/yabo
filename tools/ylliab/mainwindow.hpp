#pragma once
#include <QMainWindow>

class ParserView;
class NewTab;

QT_BEGIN_NAMESPACE
namespace Ui {
class MainWindow;
}
QT_END_NAMESPACE

class MainWindow : public QMainWindow {
  Q_OBJECT

public:
  MainWindow(QWidget *parent = nullptr);
  ~MainWindow();

private slots:
  void on_actionNewTab_triggered();
  void on_tabWidget_tabCloseRequested(int index);
  void on_actionBack_triggered();
  void on_actionForth_triggered();

private:
  Ui::MainWindow *ui;
  NewTab *new_tab;
  ParserView *current_parser_view() const;
};
