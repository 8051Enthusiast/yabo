#pragma once
#include <QCommandLineParser>
#include <QMainWindow>

class ParserView;
class NewTab;
class AddressDialog;
class MultiFileDialog;
class MultiFileView;

const QCommandLineOption parser_opt(QStringList() << "p" << "parser",
                                    "file path to parser file", "path");
const QCommandLineOption input_opt(QStringList() << "i" << "input",
                                   "file path to input file", "path");
const QCommandLineOption kernkeule_opt(QStringList() << "k" << "kernkeule",
                                       "use kernkeule memory map");

QT_BEGIN_NAMESPACE
namespace Ui {
class YlliabWindow;
}
QT_END_NAMESPACE

class YlliabWindow : public QMainWindow {
  Q_OBJECT

public:
  YlliabWindow(const QCommandLineParser &options, QWidget *parent = nullptr);
  ~YlliabWindow();

public slots:
  void new_tab_requested(QWidget *widget, QString name);
private slots:
  void on_actionNewTab_triggered();
  void on_actionNewMultiFileTab_triggered();
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
