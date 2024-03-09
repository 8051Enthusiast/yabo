#include <QRegularExpression>
#include <QTextEdit>

class CompilerErrorView : public QTextEdit {
  Q_OBJECT
public:
  CompilerErrorView(QWidget *parent = nullptr);
  void set_error(const QString &error);

private:
  QRegularExpression ansi_regex;
};