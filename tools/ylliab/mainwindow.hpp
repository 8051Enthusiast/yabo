#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private slots:
    void on_parserFileButton_clicked();

    void on_intputFileButton_clicked();

    void on_createTabBox_accepted();

private:
    Ui::MainWindow *ui;
};
#endif // MAINWINDOW_H
