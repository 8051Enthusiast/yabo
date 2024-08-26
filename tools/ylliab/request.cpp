#include "request.hpp"
#include "yabo.hpp"

RootIndex ParseRequester::request_parse(QString func_name, size_t pos) {
  last_parse = func_name;
  add_recently_used(func_name);
  return run_parse(func_name, pos);
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

ValFlags::ValFlags(YaboVal val) noexcept { is_list = val.is_list_block(); }

SpannedHandle::SpannedHandle(SpannedVal val, const FileRef &file) noexcept
    : active(val.active), span(file->file_span(val.span)), kind(val.kind()),
      flags(val) {
  switch (kind) {
  case YaboValKind::YABONOM:
    name = QString::fromUtf8(
        reinterpret_cast<NominalVTable *>(val.val->vtable)->name);
  case YaboValKind::YABOARRAY:
  case YaboValKind::YABOBLOCK:
  case YaboValKind::YABOPARSER:
  case YaboValKind::YABOFUNARGS:
    handle = reinterpret_cast<int64_t>(val.val);
    break;
  case YaboValKind::YABOCHAR:
    handle = val.access_char();
    break;
  case YaboValKind::YABOINTEGER:
    handle = val.access_int();
    break;
  case YaboValKind::YABOU8:
    handle = file->get_addr(val.access_u8());
    break;
  case YaboValKind::YABOERROR:
    handle = val.access_error();
    break;
  case YaboValKind::YABOUNIT:
    handle = 0;
    break;
  case YaboValKind::YABOBIT:
    handle = val.access_bool();
    break;
  }
}
std::optional<ValHandle> SpannedHandle::access_val() const noexcept {
  switch (kind) {
  case YaboValKind::YABOARRAY:
  case YaboValKind::YABOBLOCK:
  case YaboValKind::YABOPARSER:
  case YaboValKind::YABOFUNARGS:
  case YaboValKind::YABONOM:
    return ValHandle(handle);
  case YaboValKind::YABOU8:
  case YaboValKind::YABOCHAR:
  case YaboValKind::YABOBIT:
  case YaboValKind::YABOINTEGER:
  case YaboValKind::YABOERROR:
  case YaboValKind::YABOUNIT:
    return {};
  }
  return {};
}
