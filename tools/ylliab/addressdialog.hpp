#pragma once
#include <QDialog>

namespace Ui {
class AddressDialog;
}

class AddressDialog : public QDialog {
  Q_OBJECT

public:
  explicit AddressDialog(QWidget *parent = nullptr);
  ~AddressDialog();
  size_t get_address() const;

protected:
  void accept() override;

private:
  size_t address;
  Ui::AddressDialog *ui;
};