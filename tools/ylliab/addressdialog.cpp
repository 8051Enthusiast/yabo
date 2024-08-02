#include "addressdialog.hpp"
#include "ui_addressdialog.h"

AddressDialog::AddressDialog(QWidget *parent)
    : QDialog(parent), ui(new Ui::AddressDialog) {
  ui->setupUi(this);
}

AddressDialog::~AddressDialog() { delete ui; }

size_t AddressDialog::get_address() const {
  return address;
}

void AddressDialog::accept() {
  bool ok;
  address = ui->comboBox->currentText().toULongLong(&ok, 16);
  if (!ok) {
    return;
  }
  QDialog::accept();
}
