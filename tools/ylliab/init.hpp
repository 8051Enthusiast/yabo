#include "filerequester.hpp"
#include "hexview.hpp"
#include "yabotreemodel.hpp"
#include <QTreeView>
void init_meta_types();

void connect_hex_and_tree(HexTableView *hex_view, QTreeView *tree_view,
                          HexTableModel *hex_model, YaboTreeModel *tree_model,
                          FileRequester *file_requester);