#pragma once

#include <QWidget>
#include <QUrl>
#include <optional>

class SourceEditor;
class CompilerErrorView;

namespace Ui {
class CompileWidget;
}

class CompileWidget : public QWidget {
    Q_OBJECT

public:
    explicit CompileWidget(QWidget *parent = nullptr);
    ~CompileWidget();

    void set_source(const QString &source);
    QString get_source() const;
    void set_compile_url(const std::optional<QUrl> &url);
    void set_font(const QFont &font);
    void clear_error();
    void show_error(const QString &error);

public slots:
    void trigger_compile();

signals:
    void compile_success(const QString &file_path);
    void compile_error(const QString &error);

private slots:
    void on_compile_finished(QString file_path);
    void on_compile_error(QString error);

private:
    Ui::CompileWidget *ui;
    std::optional<QUrl> compile_url;
};