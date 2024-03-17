#include "yphbtwindow.hpp"
#include "compile.hpp"
#include "filecontent.hpp"
#include "filerequester.hpp"
#include "hexview.hpp"
#include "init.hpp"
#include "ui_yphbtwindow.h"

#include <QFileDialog>
#include <QMessageBox>
#include <QProcess>
#include <QRunnable>
#include <QShortcut>
#include <QTemporaryFile>
#include <QThreadPool>
#include <QtGlobal>
#include <memory>
#include <qnamespace.h>
#include <qobjectdefs.h>
#include <qshortcut.h>

static constexpr uint8_t PNG_EXAMPLE[] = {
    0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x00, 0x00, 0x0d,
    0x49, 0x48, 0x44, 0x52, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
    0x01, 0x00, 0x00, 0x00, 0x00, 0x37, 0x6e, 0xf9, 0x24, 0x00, 0x00, 0x00,
    0x0a, 0x49, 0x44, 0x41, 0x54, 0x78, 0x01, 0x63, 0x60, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x01, 0x73, 0x75, 0x01, 0x18, 0x00, 0x00, 0x00, 0x00, 0x49,
    0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82};

YphbtWindow::YphbtWindow(QWidget *parent, std::optional<QString> source,
                         std::optional<QByteArray> input)
    : QMainWindow(parent), ui(new Ui::YphbtWindow),
      file(std::make_shared<FileContent>(std::vector<uint8_t>(
          PNG_EXAMPLE, PNG_EXAMPLE + sizeof(PNG_EXAMPLE)))),
      file_requester(nullptr), treeModel(nullptr), hexModel(nullptr) {
  ui->setupUi(this);
  ui->errorView->hide();
  current_font.setPointSize(default_font_size);
  set_font(current_font);
  auto compile_url_env = qEnvironmentVariable("YPHBT_COMPILE_URL");
  if (compile_url_env != "") {
    compile_url = QUrl(compile_url_env);
  }
  if (source) {
    ui->plainTextEdit->setPlainText(*source);
  }
  if (input) {
    file = std::make_shared<FileContent>(
        std::vector<uint8_t>(input->begin(), input->end()));
  }
  auto ensizen = new QShortcut(QKeySequence(Qt::CTRL | Qt::Key_Plus), this);
  connect(ensizen, &QShortcut::activated, this,
          &YphbtWindow::on_actionEnsizen_triggered);
  auto ensizen2 = new QShortcut(QKeySequence(Qt::CTRL | Qt::Key_Equal), this);
  connect(ensizen2, &QShortcut::activated, this,
          &YphbtWindow::on_actionEnsizen_triggered);
  auto desizen = new QShortcut(QKeySequence(Qt::CTRL | Qt::Key_Minus), this);
  connect(desizen, &QShortcut::activated, this,
          &YphbtWindow::on_actionDesizen_triggered);
  auto reset_font = new QShortcut(QKeySequence(Qt::CTRL | Qt::Key_0), this);
  connect(reset_font, &QShortcut::activated, this,
          &YphbtWindow::reset_font_size);
  // for some reason, qt on emscripten does not like when this is directly
  // executed (and debugging wasm is a pain)
  QMetaObject::invokeMethod(this, "after_init", Qt::QueuedConnection);
}

YphbtWindow::~YphbtWindow() { delete ui; }

void YphbtWindow::after_init() { on_actionCompile_triggered(); }

void YphbtWindow::on_actionCompile_triggered() {
  auto program = ui->plainTextEdit->toPlainText();
  if (compile_url) {
    start_remote_compile(*compile_url, program, this,
                         &YphbtWindow::load_compiled_file,
                         &YphbtWindow::compile_error);
  } else {
    start_local_compile(program, this, &YphbtWindow::load_compiled_file,
                        &YphbtWindow::compile_error);
  }
}

void YphbtWindow::on_actionLoadFile_triggered() {
  QFileDialog::getOpenFileContent(
      "*.*", [this](const QString &file_path, const QByteArray &content) {
        // this happens if you cancel on chrome
        if (file_path.isEmpty()) {
          return;
        }
        file = std::make_shared<FileContent>(
            std::vector<uint8_t>(content.begin(), content.end()));
        on_actionCompile_triggered();
      });
}

void YphbtWindow::load_compiled_file(QString file_path) {
  auto new_file_requester = FileRequesterFactory().create_file_requester(
      file_path, file, "main", false);
  auto error_message = new_file_requester->error_message();
  if (error_message != "") {
    compile_error(error_message);
    return;
  }
  // we do not need the .so file at file_path anymore, since it was copied
  // by the file requester
  std::filesystem::remove(file_path.toStdString());
  set_new_file_requester(std::move(new_file_requester));
  ui->errorView->hide();
  ui->errorView->clear();
}

void YphbtWindow::compile_error(QString error) {
  ui->errorView->set_error(error);
  ui->errorView->show();
}

void YphbtWindow::set_new_file_requester(
    std::unique_ptr<FileRequester> &&new_file_requester) {
  auto newTreeModel = std::make_unique<YaboTreeModel>(new_file_requester.get());
  ui->treeView->setModel(newTreeModel.get());
  treeModel = std::move(newTreeModel);
  auto newHexModel =
      std::make_unique<HexTableModel>(file, new_file_requester.get());
  ui->tableView->setModel(newHexModel.get());
  hexModel = std::move(newHexModel);
  file_requester = std::move(new_file_requester);
  init_hex_and_tree(ui->tableView, ui->treeView, hexModel.get(),
                    treeModel.get(), file_requester.get());
  file_requester->start_executor_thread();
}

void YphbtWindow::on_tableView_doubleClicked(const QModelIndex &index) {
  hexModel->handle_doubleclick(index);
}

void YphbtWindow::on_treeView_doubleClicked(const QModelIndex &index) {
  treeModel->handle_doubleclick(index);
}

void YphbtWindow::on_actionBack_triggered() { treeModel->undo(); }

void YphbtWindow::on_actionForth_triggered() { treeModel->redo(); }

void YphbtWindow::set_font(const QFont &font) {
  setFont(font);
  ui->tableView->set_font(font);
  ui->tableView->verticalHeader()->setFont(font);
  ui->treeView->setFont(font);
  ui->treeView->header()->setFont(font);
  ui->plainTextEdit->setFont(font);
  ui->errorView->setFont(font);
  ui->toolBar->setFont(font);
}

void YphbtWindow::on_actionEnsizen_triggered() {
  current_font.setPointSize(current_font.pointSize() + 1);
  set_font(current_font);
}

void YphbtWindow::on_actionDesizen_triggered() {
  if (current_font.pointSize() > minimum_font_size) {
    current_font.setPointSize(current_font.pointSize() - 1);
  }
  set_font(current_font);
}

void YphbtWindow::reset_font_size() {
  current_font.setPointSize(default_font_size);
  set_font(current_font);
}
