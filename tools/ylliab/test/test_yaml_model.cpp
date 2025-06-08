#include <QCoreApplication>
#include <QFileInfo>
#include <QRegularExpression>
#include <QSignalSpy>
#include <QtTest/QtTest>
#include <chrono>
#include <memory>

#include "filecontent.hpp"
#include "filerequester.hpp"
#include "selectionstate.hpp"
#include "valtreemodel.hpp"
#include "yamldataprovider.hpp"

class SignalWaiter {
public:
  template <typename PointerToMemberFunction>
  SignalWaiter(
      const typename QtPrivate::FunctionPointer<PointerToMemberFunction>::Object
          *sender,
      PointerToMemberFunction signal)
      : spy(sender, signal), count(0) {}

  bool next() {
    using namespace std::chrono_literals;
    if (spy.count() > count) {
      count++;
      return true;
    }

    spy.wait(1s);

    if (spy.count() > count) {
      count = spy.count();
      return true;
    }
    qDebug() << "SignalWaiter timed out after waiting for signal";

    return false;
  }

  int totalCount() const { return spy.count(); }

  int currentCount() const { return count; }

private:
  QSignalSpy spy;
  int count;
};

class TestInfrastructure {
public:
  TestInfrastructure(const QString &yaml_path,
                     const QString &parser_name = "main") {
    auto file_content = std::make_shared<FileContent>(std::vector<uint8_t>());

    auto provider = std::make_unique<YamlDataProvider>(yaml_path);
    file_requester = std::make_unique<FileRequester>(std::move(provider),
                                                     file_content, false);

    selection_state = std::make_shared<SelectionState>();
    tree_model =
        std::make_unique<ValTreeModel>(file_requester.get(), selection_state);

    initializeAndWait(parser_name);
  }

  ~TestInfrastructure() {
    tree_model.reset();
    file_requester->stop_thread();
    file_requester.reset();
  }

  TestInfrastructure(const TestInfrastructure &) = delete;
  TestInfrastructure &operator=(const TestInfrastructure &) = delete;
  TestInfrastructure(TestInfrastructure &&) = delete;
  TestInfrastructure &operator=(TestInfrastructure &&) = delete;

  FileRequester *fileRequester() const { return file_requester.get(); }
  ValTreeModel *treeModel() const { return tree_model.get(); }
  std::shared_ptr<SelectionState> selectionState() const {
    return selection_state;
  }
  bool isReady() const { return ready; }

private:
  void initializeAndWait(const QString &parser_name) {
    SignalWaiter modelResetWaiter(tree_model.get(),
                                  &QAbstractItemModel::modelReset);
    SignalWaiter dataChangedWaiter(file_requester.get(),
                                   &FileRequester::tree_data_changed);

    file_requester->start_provider_thread();

    auto root = file_requester->request_parse(parser_name, 0);
    selection_state->set_root(root);

    bool gotModelReset = modelResetWaiter.next();
    bool gotDataChange = gotModelReset ? dataChangedWaiter.next() : false;

    ready = gotModelReset && gotDataChange;
  }

  std::unique_ptr<FileRequester> file_requester{};
  std::unique_ptr<ValTreeModel> tree_model{};
  std::shared_ptr<SelectionState> selection_state{};
  bool ready = false;
};

class TestYamlModel : public QObject {
  Q_OBJECT

private slots:
  void testSimpleYamlData();
  void testLargeArrayData();
  void testRecursiveYamlData();

private:
  void fetchEntireTree(ValTreeModel *model,
                       const QModelIndex &parent = QModelIndex()) {
    if (!model)
      return;

    SignalWaiter rowsInsertedWaiter(model, &QAbstractItemModel::rowsInserted);
    fetchEntireTreeRecursive(model, parent, rowsInsertedWaiter);
  }

  void fetchEntireTreeRecursive(ValTreeModel *model,
                               const QModelIndex &parent,
                               SignalWaiter &rowsInsertedWaiter) {
    if (!model)
      return;

    while (model->canFetchMore(parent)) {
      int beforeCount = model->rowCount(parent);
      model->fetchMore(parent);

      if (!rowsInsertedWaiter.next()) {
        break;
      }

      int afterCount = model->rowCount(parent);
    }

    int rowCount = model->rowCount(parent);
    for (int i = 0; i < rowCount; ++i) {
      QModelIndex child = model->index(i, 0, parent);
      if (child.isValid()) {
        fetchEntireTreeRecursive(model, child, rowsInsertedWaiter);
      }
    }
  }
};

namespace {
void verify_index_value(QModelIndex index, int row, const char* name, const char* value) {
  auto idx = index.model()->index(row, 0, index.parent());
  QVERIFY(idx.isValid());
  auto field_name = idx.model()->data(idx, Qt::DisplayRole);
  QCOMPARE(field_name.toString(), QString(name));

  auto field_value = idx.model()->data(idx.model()->index(row, 1, index.parent()), Qt::DisplayRole);
  QCOMPARE(field_value.toString(), QString(value));
}

void verify_index_rows(QModelIndex index, int row, const char* name, int row_count) {
  auto idx = index.model()->index(row, 0, index.parent());
  QVERIFY(idx.isValid());
  auto field_name = idx.model()->data(idx, Qt::DisplayRole);
  QCOMPARE(field_name.toString(), QString(name));

  int children_count = idx.model()->rowCount(idx);
  QCOMPARE(children_count, row_count);
}
}

void TestYamlModel::testSimpleYamlData() {
  auto yaml_path = "simple_test_data.yaml";
  TestInfrastructure infra(yaml_path, "main");
  auto tree_model = infra.treeModel();

  auto root_index = tree_model->index(0, 0);
  QVERIFY(root_index.isValid());

  auto root_field_name = tree_model->data(root_index, Qt::DisplayRole);
  QCOMPARE(root_field_name.toString(), QString("main"));

  fetchEntireTree(tree_model, root_index);

  int root_children = tree_model->rowCount(root_index);
  QCOMPARE(root_children, 3);

  verify_index_value(tree_model->index(0, 0, root_index), 0, "magic", "0x89");
  verify_index_value(tree_model->index(1, 0, root_index), 1, "version", "1");
  verify_index_rows(tree_model->index(2, 0, root_index), 2, "flags", 2);
  verify_index_value(tree_model->index(0, 0, tree_model->index(2, 0, root_index)), 0, "[0]", "0x01");
  verify_index_value(tree_model->index(1, 0, tree_model->index(2, 0, root_index)), 1, "[1]", "0x02");
}

void TestYamlModel::testLargeArrayData() {
  QString yaml_path = "large_test_data.yaml";

  TestInfrastructure infra(yaml_path, "large_array_test");
  QVERIFY2(infra.isReady(), "Failed to initialize test infrastructure");

  auto root_index = infra.treeModel()->index(0, 0);
  QVERIFY(root_index.isValid());
  fetchEntireTree(infra.treeModel(), root_index);

  auto root_field_name = infra.treeModel()->data(root_index, Qt::DisplayRole);
  QCOMPARE(root_field_name.toString(), QString("large_array_test"));

  int root_children = infra.treeModel()->rowCount(root_index);
  QCOMPARE(root_children, 2);

  verify_index_value(infra.treeModel()->index(0, 0, root_index), 0, "count", "5000");
  verify_index_rows(infra.treeModel()->index(1, 0, root_index), 1, "numbers", 5000);

  auto numbers_index = infra.treeModel()->index(1, 0, root_index);
  int final_count = infra.treeModel()->rowCount(numbers_index);
  QCOMPARE(final_count, 5000);

  for (int i = 0; i < final_count; ++i) {
    auto element_index = infra.treeModel()->index(i, 0, numbers_index);
    QVERIFY2(element_index.isValid(),
             QString("Element %1 index invalid").arg(i).toUtf8());

    auto element_name = infra.treeModel()->data(element_index, Qt::DisplayRole);
    QString expected_name = QString("[%1]").arg(i);
    QCOMPARE(element_name.toString(), expected_name);

    auto element_value_index = infra.treeModel()->index(i, 1, numbers_index);
    QVERIFY2(element_value_index.isValid(),
             QString("Element %1 value index invalid").arg(i).toUtf8());

    auto element_value =
        infra.treeModel()->data(element_value_index, Qt::DisplayRole);
    QCOMPARE(element_value.toInt(), i);
  }
}

void TestYamlModel::testRecursiveYamlData() {
  TestInfrastructure infra("recursive_test_data.yaml", "recursive_test");
  QVERIFY2(infra.isReady(), "Failed to initialize test infrastructure");

  auto root = infra.treeModel()->index(0, 0);
  QVERIFY(root.isValid());
  QCOMPARE(infra.treeModel()->data(root, Qt::DisplayRole).toString(), QString("recursive_test"));

  auto fetchChildren = [](ValTreeModel *model, const QModelIndex &parent) {
    if (!model || !parent.isValid()) return;
    SignalWaiter waiter(model, &QAbstractItemModel::rowsInserted);
    while (model->canFetchMore(parent)) {
      model->fetchMore(parent);
      if (!waiter.next()) break;
    }
  };

  fetchChildren(infra.treeModel(), root);
  QCOMPARE(infra.treeModel()->rowCount(root), 2);

  verify_index_rows(infra.treeModel()->index(0, 0, root), 0, "self_ref", 0);
  verify_index_value(infra.treeModel()->index(1, 0, root), 1, "value", "0x01");

  auto self_ref = infra.treeModel()->index(0, 0, root);
  fetchChildren(infra.treeModel(), self_ref);
  QCOMPARE(infra.treeModel()->rowCount(self_ref), 1);

  auto circular = infra.treeModel()->index(0, 0, self_ref);
  QVERIFY(circular.isValid());
  QCOMPARE(infra.treeModel()->data(circular, Qt::DisplayRole).toString(), QString("[0]"));

  fetchChildren(infra.treeModel(), circular);
  QCOMPARE(infra.treeModel()->rowCount(circular), 2);

  auto nested = infra.treeModel()->index(0, 0, circular);
  QVERIFY(nested.isValid());
  QVERIFY(!infra.treeModel()->data(nested, Qt::DisplayRole).toString().isEmpty());

  verify_index_value(infra.treeModel()->index(1, 0, circular), 1, "value", "0x01");

  fetchChildren(infra.treeModel(), nested);
  QVERIFY(infra.treeModel()->rowCount(nested) >= 0);
}

QTEST_MAIN(TestYamlModel)
#include "test_yaml_model.moc"