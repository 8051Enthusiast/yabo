#include "init.hpp"
#include "graph.hpp"
#include "hex.hpp"
#include "hexview.hpp"
#include "node.hpp"
#include "request.hpp"
#include "selectionstate.hpp"
#include "valtreemodel.hpp"
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
  qRegisterMetaType<PositionOverride>();
}
void init_hex_and_tree(HexTableView *hex_view, QTreeView *tree_view,
                       HexTableModel *hex_model, ValTreeModel *tree_model,
                       FileRequester *file_requester,
                       std::shared_ptr<SelectionState> &select) {
  QObject::connect(file_requester, &FileRequester::new_node, hex_model,
                   &HexTableModel::add_range);
  hex_view->set_parser_requester(file_requester);
}