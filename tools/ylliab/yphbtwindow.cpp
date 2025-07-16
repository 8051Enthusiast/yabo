#include "yphbtwindow.hpp"
#include "compilewidget.hpp"
#include "filecontent.hpp"
#include "filerequester.hpp"
#include "hex.hpp"
#include "hexview.hpp"
#include "init.hpp"
#include "selectionstate.hpp"
#include "ui_yphbtwindow.h"
#include "valtreemodel.hpp"

#include <QClipboard>
#include <QFileDialog>
#include <QJsonDocument>
#include <QJsonObject>
#include <QMessageBox>
#include <QProcess>
#include <QRunnable>
#include <QSettings>
#include <QShortcut>
#include <QStringView>
#include <QTemporaryFile>
#include <QThreadPool>
#include <Qt>
#include <QtEnvironmentVariables>
#include <QtGlobal>
#include <memory>
#include <qobjectdefs.h>

static constexpr uint8_t PNG_EXAMPLE[] = {
    0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x00, 0x00, 0x0d,
    0x49, 0x48, 0x44, 0x52, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
    0x01, 0x00, 0x00, 0x00, 0x00, 0x37, 0x6e, 0xf9, 0x24, 0x00, 0x00, 0x00,
    0x0a, 0x49, 0x44, 0x41, 0x54, 0x78, 0x01, 0x63, 0x60, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x01, 0x73, 0x75, 0x01, 0x18, 0x00, 0x00, 0x00, 0x00, 0x49,
    0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82};

static constexpr const char *PNG_SOURCE = R"(import list

# a parser for the rough structure of a png

def ~chunk[T](ty: ~[u8], data_parser: ~T) = {
  length: u32b
  type: [4] |> ty?
  value: [length] |> data_parser
  crc: u32b
}

def ~head = {
  width: u32b
  height: u32b
  bit_depth: u8
  color_type: u8
  compression_method: u8
  filter_method: u8
  interlace_method: u8
}

def ~rgb = {
  red: u8
  green: u8
  blue: u8
}

export
def ~main = {
  h/89 50 4e 47 0d 0a 1a 0a/

  header: chunk(/IHDR/, head)!

  chunks: list.list({
    | palette: chunk(/PLTE/, rgb[..])?
    | data: chunk(/IDAT/, [..])?
    \ optional: chunk(/[a-z].{3}/, [..])?
  })
  end: chunk(/IEND/, nil)!
}
)";


ExampleLoader::ExampleLoader(QObject *parent)
    : QObject(parent), json_data(),
      network_manager(new QNetworkAccessManager(this)), source({0}),
      input({0}) {}

ExampleLoader::~ExampleLoader() {}

void ExampleLoader::load_files(size_t index) {
  if (index >= json_data.size()) {
    emit error("Invalid index");
    return;
  }

  QJsonObject item = json_data[index].toObject();
  auto source_str = item["source"];
  auto input_str = item["input"];

  if (!source_str.isString() || !input_str.isString()) {
    emit error("Invalid example data format");
    return;
  }

  input.url = QUrl(input_str.toString());
  source.url = QUrl(source_str.toString());
  input.completed = false;
  source.completed = false;

  start_download(source.url, true);
  start_download(input.url, false);
}

void ExampleLoader::start_download(const QUrl &url, bool is_source) {
  QNetworkRequest request(url);
  QNetworkReply *reply = network_manager->get(request);

  if (is_source) {
    connect(reply, &QNetworkReply::finished, this,
            &ExampleLoader::handle_source_finished);
  } else {
    connect(reply, &QNetworkReply::finished, this,
            &ExampleLoader::handle_input_finished);
  }

  connect(reply, &QNetworkReply::errorOccurred, this,
          &ExampleLoader::handle_network_error);
}

void ExampleLoader::handle_download_finished(QNetworkReply *reply,
                                             DownloadState &state) {
  if (!reply)
    return;

  reply->deleteLater();

  if (reply->error() != QNetworkReply::NoError) {
    return;
  }

  if (reply->url().fileName() != state.url.fileName()) {
    return;
  }

  state.content = reply->readAll();
  state.completed = true;

  if (source.completed && input.completed) {
    source.url = QUrl();
    input.url = QUrl();
    emit files_loaded(QString::fromUtf8(std::move(source.content)),
                      std::move(input.content));
  }
}

void ExampleLoader::handle_source_finished() {
  handle_download_finished(qobject_cast<QNetworkReply *>(sender()), source);
}

void ExampleLoader::handle_input_finished() {
  handle_download_finished(qobject_cast<QNetworkReply *>(sender()), input);
}

void ExampleLoader::handle_network_error(QNetworkReply::NetworkError error) {
  QNetworkReply *reply = qobject_cast<QNetworkReply *>(sender());
  if (!reply)
    return;

  emit this->error(
      QString("Network error: %1 - %2").arg(error).arg(reply->errorString()));
}

void ExampleLoader::load_examples(const QUrl &url) {
  QNetworkRequest request(url);
  QNetworkReply *reply = network_manager->get(request);

  connect(reply, &QNetworkReply::finished, this,
          &ExampleLoader::handle_examples_loaded);
}

void ExampleLoader::handle_examples_loaded() {
  QNetworkReply *reply = qobject_cast<QNetworkReply *>(sender());
  if (!reply) {
    return;
  }

  QByteArray data = reply->readAll();
  QJsonParseError parse_error;
  QJsonDocument doc = QJsonDocument::fromJson(data, &parse_error);

  if (parse_error.error != QJsonParseError::NoError) {
    qWarning() << "JSON parse error:" << parse_error.errorString();
    return;
  }

  if (!doc.isArray()) {
    qWarning() << "JSON file is not an array";
    return;
  }
  json_data = doc.array();
  emit examples_json_loaded(json_data);
}

YphbtWindow::YphbtWindow(QWidget *parent, std::optional<QString> source,
                         std::optional<QByteArray> input,
                         std::optional<QUrl> examples)
    : QMainWindow(parent), ui(new Ui::YphbtWindow),
      file(std::make_shared<FileContent>(std::vector<uint8_t>(
          PNG_EXAMPLE, PNG_EXAMPLE + sizeof(PNG_EXAMPLE)))) {
  ui->setupUi(this);
  set_font(current_font);
  auto compile_url_env = qEnvironmentVariable("YPHBT_COMPILE_URL");
  if (compile_url_env != "") {
    compile_url = QUrl(compile_url_env);
  }
  ui->compileWidget->set_compile_url(compile_url);

  connect(ui->compileWidget, &CompileWidget::compile_success,
          this, &YphbtWindow::load_compiled_file);

  if (source) {
    ui->compileWidget->set_source(*source);
  } else {
    ui->compileWidget->set_source(PNG_SOURCE);
  }
  if (input) {
    file = std::make_shared<FileContent>(
        std::vector<uint8_t>(input->begin(), input->end()));
  }
  if (examples) {
    init_example_loader(*examples);
  }
  // for some reason, qt on emscripten does not like when this is directly
  // executed (and debugging wasm is a pain)
  QMetaObject::invokeMethod(this, "after_init", Qt::QueuedConnection);
}

YphbtWindow::~YphbtWindow() { delete ui; }

void YphbtWindow::after_init() { on_actionCompile_triggered(); }

void YphbtWindow::load_example(QString source, QByteArray input) {
  file = std::make_shared<FileContent>(
      std::vector<uint8_t>(input.data(), input.data() + input.size()));
  ui->compileWidget->set_source(source);
  on_actionCompile_triggered();
}

void YphbtWindow::on_actionCompile_triggered() {
  auto program = ui->compileWidget->get_source();
  QSettings settings;
  settings.setValue("source", program);
  ui->compileWidget->trigger_compile();
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
  auto new_file_requester =
      FileRequesterFactory().create_file_requester(file_path, file, false);
  auto error_message = new_file_requester->error_message();
  if (error_message != "") {
    compile_error(error_message);
    return;
  }
  // we do not need the .so file at file_path anymore, since it was copied
  // by the file requester
  std::filesystem::remove(file_path.toStdString());
  set_new_file_requester(std::move(new_file_requester));
  ui->compileWidget->clear_error();
}

void YphbtWindow::compile_error(QString error) {
  ui->compileWidget->show_error(error);
}

void YphbtWindow::set_new_file_requester(
    std::unique_ptr<FileRequester> &&new_file_requester) {
  select = std::make_shared<SelectionState>();
  auto newTreeModel = new ValTreeModel(new_file_requester.get(), select);
  ui->treeView->setModel(newTreeModel);
  auto newHexModel = new HexTableModel(file, new_file_requester.get(),
                                       new_file_requester.get());
  ui->tableView->set_model(newHexModel, select);
  file_requester = std::move(new_file_requester);
  init_hex_and_tree(ui->tableView, ui->treeView, newHexModel, newTreeModel,
                    file_requester.get(), select);
  auto root = file_requester->request_parse("main", 0);
  select->set_root(root);
  file_requester->start_provider_thread();
}

void YphbtWindow::on_tableView_doubleClicked(const QModelIndex &index) {
  if (auto hex_model = dynamic_cast<HexTableModel *>(ui->tableView->model())) {
    hex_model->handle_doubleclick(index, select);
  }
}

void YphbtWindow::on_treeView_doubleClicked(const QModelIndex &index) {
  if (auto tree_model = dynamic_cast<ValTreeModel *>(ui->treeView->model())) {
    tree_model->handle_doubleclick(index);
  }
}

void YphbtWindow::on_actionBack_triggered() { select->undo(); }

void YphbtWindow::on_actionForth_triggered() { select->redo(); }

void YphbtWindow::on_actioncopyURL_triggered() {
  auto [start, end] = file->slice();
  QByteArray file_content(reinterpret_cast<const char *>(start), end - start);
  auto compressed_file =
      qCompress(file_content).toBase64(QByteArray::Base64UrlEncoding);

  QByteArray source = ui->compileWidget->get_source().toUtf8();
  auto compressed_source =
      qCompress(source).toBase64(QByteArray::Base64UrlEncoding);

  auto url = qEnvironmentVariable("YPHBT_URL");

  auto link = QString("%1?source=z!%2&input=z!%3")
                  .arg(url)
                  .arg(compressed_source)
                  .arg(compressed_file);

  auto clipboard = QGuiApplication::clipboard();
  clipboard->setText(link);
}

void YphbtWindow::set_font(const QFont &font) {
  setFont(font);
  ui->tableView->set_font(font);
  ui->tableView->verticalHeader()->setFont(font);
  ui->treeView->setFont(font);
  ui->treeView->header()->setFont(font);
  ui->compileWidget->set_font(font);
  ui->toolBar->setFont(font);
}

void YphbtWindow::init_examples(QJsonArray json_data) {
  auto file_menu = menuBar()->addMenu(tr("&Examples"));

  // Add an action for each item in the JSON array
  for (int i = 0; i < json_data.size(); ++i) {
    QJsonObject item = json_data[i].toObject();
    auto name = item["name"];
    if (!name.isString()) {
      emit compile_error("Could not load examples json");
      return;
    }

    QAction *action = new QAction(name.toString(), this);

    // Using a lambda to capture the index
    connect(action, &QAction::triggered, this,
            [this, i]() { emit loader->load_files(i); });

    file_menu->addAction(action);
  }
}

void YphbtWindow::init_example_loader(QUrl &url) {
  loader = std::make_unique<ExampleLoader>();
  connect(loader.get(), &ExampleLoader::error, this,
          &YphbtWindow::compile_error);
  connect(loader.get(), &ExampleLoader::files_loaded, this,
          &YphbtWindow::load_example);
  connect(loader.get(), &ExampleLoader::examples_json_loaded, this,
          &YphbtWindow::init_examples);
  loader->load_examples(url);
}
