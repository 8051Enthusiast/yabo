#include "filerequester.hpp"
#include "hexview.hpp"
#include "valtreemodel.hpp"
#include <QTreeView>
void init_meta_types();

void init_hex_and_tree(HexTableView *hex_view, QTreeView *tree_view,
                       HexTableModel *hex_model, ValTreeModel *tree_model,
                       FileRequester *file_requester,
                       std::shared_ptr<SelectionState> &select);