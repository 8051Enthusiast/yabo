#include "filerequester.hpp"
#include "hexview.hpp"
#include "valtreemodel.hpp"
#include <QTreeView>
void init_runtime();

void init_hex(HexTableView *hex_view, HexTableModel *hex_model,
                       FileRequester *file_requester);