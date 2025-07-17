#include "init.hpp"
#include "graph.hpp"
#include "hex.hpp"
#include "hexview.hpp"
#include "node.hpp"
#include "request.hpp"
#include "valtreemodel.hpp"
#include "yabo.hpp"
#include <QHeaderView>
#include <QObject>

void init_runtime() {
  qRegisterMetaType<Meta>();
  qRegisterMetaType<Request>();
  qRegisterMetaType<Response>();
  qRegisterMetaType<TreeIndex>();
  qRegisterMetaType<RootIndex>();
  qRegisterMetaType<NodeRange>();
  qRegisterMetaType<GraphUpdate>();
  qRegisterMetaType<PositionsUpdate>();
  qRegisterMetaType<PositionOverride>();
  init_segfault_handler();
}

void init_hex(HexTableView *hex_view, HexTableModel *hex_model,
                       FileRequester *file_requester) {
  QObject::connect(file_requester, &FileRequester::new_node, hex_model,
                   &HexTableModel::add_range);
  hex_view->set_parser_requester(file_requester);
}