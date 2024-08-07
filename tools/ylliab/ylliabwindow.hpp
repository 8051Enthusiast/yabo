#pragma once
#include <QMainWindow>

class ParserView;
class NewTab;
class AddressDialog;

QT_BEGIN_NAMESPACE
namespace Ui {
class YlliabWindow;
}
QT_END_NAMESPACE

class YlliabWindow : public QMainWindow {
  Q_OBJECT

public:
  YlliabWindow(QWidget *parent = nullptr);
  ~YlliabWindow();

private slots:
  void on_actionNewTab_triggered();
  void on_tabWidget_tabCloseRequested(int index);
  void on_actionBack_triggered();
  void on_actionForth_triggered();
  void on_actionGotoAddress_triggered();

  private:
  Ui::YlliabWindow *ui;
  NewTab *new_tab;
  AddressDialog *goto_address;
  ParserView *current_parser_view() const;
};
