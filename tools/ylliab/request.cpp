#include "request.hpp"

void ParseRequester::request_parse(QString func_name, size_t pos) {
  last_parse = func_name;
  add_recently_used(func_name);
  run_parse(func_name, pos);
}

std::vector<QString> const &ParseRequester::recently_used_funcs() const {
  return recently_used;
}
void ParseRequester::add_recently_used(QString func_name) {
  auto it = std::find(recently_used.begin(), recently_used.end(), func_name);
  if (it == recently_used.end()) {
    recently_used.push_back(func_name);
    if (recently_used.size() > recently_used_list_size) {
      recently_used.erase(recently_used.begin());
    }
  } else {
    recently_used.erase(it);
    recently_used.push_back(func_name);
  }
}
