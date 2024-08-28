#include "ylliabwindow.hpp"
#include "./ui_ylliabwindow.h"
#include "addressdialog.hpp"
#include "newtab.hpp"
#include "parserview.hpp"

#include <QFileDialog>
#include <QMessageBox>
#include <cstring>

static std::optional<FileRef> get_kernkeule_map(QString &err) {
  auto kernkeule_maps = std::getenv("KERNKEULE_MAPS");
  if (!kernkeule_maps || !std::strlen(kernkeule_maps)) {
    err = "KERNKEULE_MAPS variable not set, did you start ylliab using "
          "kernkeule?";
  }
  auto map_list = QString(kernkeule_maps).split(':');
  std::vector<std::pair<size_t, size_t>> maps;
  for (auto map_str : map_list) {
    auto map_components = map_str.split('+');
    if (map_components.size() != 2) {
      err = "invalid KERNKEULE_MAPS format";
      return {};
    }
    bool ok = true;
    size_t base = map_components[0].toLongLong(&ok, 16);
    if (!ok) {
      err = "invalid KERNKEULE_MAPS format";
      return {};
    }
    size_t size = map_components[1].toLongLong(&ok, 16);
    if (!ok) {
      err = "invalid KERNKEULE_MAPS format";
      return {};
    }
    maps.push_back({base, size});
  }
  return std::make_shared<FileContent>(maps);
}

YlliabWindow::YlliabWindow(const QCommandLineParser &options, QWidget *parent)
    : QMainWindow(parent), ui(new Ui::YlliabWindow) {
  ui->setupUi(this);
  new_tab = new NewTab(this);
  goto_address = new AddressDialog(this);
  if (options.isSet(parser_opt)) {
    new_tab->set_preset_parser_path(options.value(parser_opt));
  }
  if (options.isSet(input_opt)) {
    new_tab->set_preset_file_path(options.value(input_opt));
  } else if (options.isSet(kernkeule_opt)) {
    QString err;
    auto file_ref = get_kernkeule_map(err);
    if (!file_ref) {
      new_tab->show_error(err);
    } else {
      new_tab->set_preset_file(*file_ref);
    }
  }
  on_actionNewTab_triggered();
  auto rsp = std::getenv("KERNKEULE_SP");
  if (!rsp) {
    return;
  }
  bool ok;
  auto rsp_addr = QString(rsp).toLongLong(&ok, 16);
  if (!ok) {
    return;
  }
  auto widget = current_parser_view();
  if (!widget)
    return;
  widget->goto_address(rsp_addr);
}

YlliabWindow::~YlliabWindow() { delete ui; }
void YlliabWindow::on_actionNewTab_triggered() {
  new_tab->exec();
  if (new_tab->result() != QDialog::Accepted) {
    return;
  }
  auto fileRequester = new_tab->get_file_requester();
  auto view = new ParserView(nullptr, std::move(fileRequester));
  auto name = new_tab->get_input_file();
  ui->tabWidget->addTab(view, name);
  auto newTabIndex = ui->tabWidget->count() - 1;
  ui->tabWidget->setCurrentIndex(newTabIndex);
}

void YlliabWindow::on_tabWidget_tabCloseRequested(int index) {
  auto view = ui->tabWidget->widget(index);
  ui->tabWidget->removeTab(index);
  delete view;
}

void YlliabWindow::on_actionBack_triggered() {
  auto widget = current_parser_view();
  if (!widget)
    return;
  widget->back();
}

void YlliabWindow::on_actionForth_triggered() {
  auto widget = current_parser_view();
  if (!widget)
    return;
  widget->forth();
}

ParserView *YlliabWindow::current_parser_view() const {
  return dynamic_cast<ParserView *>(ui->tabWidget->currentWidget());
}

void YlliabWindow::on_actionGotoAddress_triggered() {
  if (!ui->tabWidget->count()) {
    return;
  }
  goto_address->exec();
  if (goto_address->result() != QDialog::Accepted) {
    return;
  }
  auto address = goto_address->get_address();
  auto widget = current_parser_view();
  if (!widget)
    return;
  widget->goto_address(address);
}
