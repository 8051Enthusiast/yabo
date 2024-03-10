#include "init.hpp"
#include "graph.hpp"
#include "hexview.hpp"
#include "node.hpp"
#include "request.hpp"
#include "yabotreemodel.hpp"
#include <QHeaderView>
#include <QObject>
void init_meta_types() {
  qRegisterMetaType<Meta>();
  qRegisterMetaType<Request>();
  qRegisterMetaType<Response>();
  qRegisterMetaType<TreeIndex>();
  qRegisterMetaType<RootIndex>();
  qRegisterMetaType<NodeRange>();
  qRegisterMetaType<GraphUpdate>();
  qRegisterMetaType<PositionsUpdate>();
}
void init_hex_and_tree(HexTableView *hex_view, QTreeView *tree_view,
                       HexTableModel *hex_model, YaboTreeModel *tree_model,
                       FileRequester *file_requester) {
  QObject::connect(file_requester, &FileRequester::root_changed, hex_view,
                   &HexTableView::goto_node);
  QObject::connect(file_requester, &FileRequester::select_range, hex_view,
                   &HexTableView::select_addr_range);
  QObject::connect(file_requester, &FileRequester::new_node, hex_model,
                   &HexTableModel::add_range);
  QObject::connect(file_requester, &FileRequester::goto_addr, hex_view,
                   &HexTableView::goto_addr);
  QObject::connect(tree_model, &YaboTreeModel::expand, tree_view,
                   &QTreeView::expand);
  QObject::connect(tree_view->selectionModel(),
                   &QItemSelectionModel::currentChanged, tree_model,
                   &YaboTreeModel::change_selected);
  auto header_view = tree_view->header();
  header_view->setSectionResizeMode(0, QHeaderView::ResizeToContents);
  header_view->setStretchLastSection(false);
  header_view->setSectionResizeMode(YaboTreeModel::NUM_COLUMNS - 1,
                                    QHeaderView::Stretch);
  header_view->setMinimumSectionSize(100);
}