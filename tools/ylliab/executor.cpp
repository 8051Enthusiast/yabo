#include "executor.hpp"
#include "request.hpp"

#include <dlfcn.h>
#ifdef __EMSCRIPTEN__
#include <emscripten.h>

// https://github.com/WebAssembly/binaryen/pull/2427
const char *YABO_GLOBAL_INIT = "orig$yabo_global_init";
#else
const char *YABO_GLOBAL_INIT = "yabo_global_init";
#endif

Executor::Executor(std::filesystem::path path, FileRef file_content)
    : file(file_content), lib(nullptr) {
  // we need to create a tmpfile copy of the library pointed to by
  // `path` which we then dlopen
  // this is because dlopen does not work well if the file changes, and
  // global symbols would get deduplicated which is also a bad idea
  try {
    // note: this is essentially tmpnam, but we are cool and totally
    // allowed to do this
    // (an attacker can just mess with the file after it was created and
    // before we dlopen it anyway, and we can only dlopen through
    // a file path)
    tmp_file = tmp_file_name("yabo_", ".so");

    std::filesystem::copy_file(
        path, tmp_file, std::filesystem::copy_options::overwrite_existing);
  } catch (std::filesystem::filesystem_error &e) {
    auto err = std::stringstream()
               << "Could not create temporary file: " << e.what();
    throw ExecutorError(err.str());
  }
}

int64_t Executor::init_lib() {
  if (lib) {
    return 0;
  }
  lib = dlopen(tmp_file.c_str(), RTLD_LAZY);
  if (!lib) {
    // error = dlerror();
    return -1;
  }

  auto size = reinterpret_cast<size_t *>(dlsym(lib, "yabo_max_buf_size"));
  if (!size) {
    // error = QString("File does not contain yabo_max_buf_size symbol: %1. Is "
    //                 "the file in the right format?")
    //             .arg(dlerror());
    return -1;
  }

  auto span = file->span();

  typedef InitFun init_fun;
  auto global_init = reinterpret_cast<init_fun>(dlsym(lib, YABO_GLOBAL_INIT));
  int64_t status = global_init(span.data(), span.data() + span.size());
  if (status) {
    // error = QString("Global init failed with status %1").arg(status);
    return -1;
  }
  vals = YaboValCreator(YaboValStorage(*size));
  return 0;
}

Executor::~Executor() {
  if (lib) {
    dlclose(lib);
  }
  std::filesystem::remove(tmp_file);
}

std::optional<Response> Executor::get_fields(Request &req) {
  std::vector<NamedYaboVal> fields;
  auto vtable = reinterpret_cast<BlockVTable *>(req.val.val->vtable);
  for (size_t i = 0; i < vtable->fields->number_fields; i++) {
    auto name = vtable->fields->fields[i];
    auto field_val = vals.access_field(req.val, name);
    if (field_val.has_value()) {
      auto new_val = normalize(field_val.value(), req.val.span);
      fields.push_back(NamedYaboVal{QString(name), new_val});
    }
  }
  std::sort(fields.begin(), fields.end());
  ValVecResponse ret{std::move(fields), {}};
  return Response(req.metadata, std::move(ret));
}

std::optional<Response> Executor::get_array_members(Request &req) {
  std::vector<NamedYaboVal> elements;
  uint64_t len = vals.array_len(req.val);
  uint64_t start = req.array_start_index;
  uint64_t end = std::min((uint64_t)array_fetch_size, len);
  for (uint64_t i = 0; i < end; i++) {
    auto idx_val = vals.index(req.val, i);
    if (idx_val.has_value()) {
      auto new_val = normalize(idx_val.value(), req.val.span);
      elements.push_back(NamedYaboVal{QString::number(start + i), new_val});
    } else {
      return {};
    }
  }
  std::optional<YaboVal> continuation = {};
  if (array_fetch_size < len) {
    continuation = vals.skip(req.val, array_fetch_size);
  }
  ValVecResponse ret{std::move(elements), continuation};
  return Response(req.metadata, std::move(ret));
}

std::optional<Response> Executor::get_list_members(Request &req) {
  std::vector<YaboVal> ret_vals;
  YaboVal current = req.val;
  uint64_t idx;
  for (idx = 0; idx < array_fetch_size; idx++) {
    if (!current.is_list_block()) {
      ret_vals.push_back(current);
      break;
    }
    auto head = vals.access_field(current, YaboVal::list_head);
    auto tail = vals.access_field(current, YaboVal::list_tail, 0);
    if (!head.has_value()) {
      if (tail.has_value()) {
        ret_vals.push_back(*tail);
      }
      break;
    }
    ret_vals.push_back(*head);
    if (!tail.has_value()) {
      break;
    }
    current = *tail;
  }
  std::vector<NamedYaboVal> elements;
  elements.reserve(ret_vals.size());
  for (size_t i = 0; i < ret_vals.size(); i++) {
    auto new_val = normalize(ret_vals[i], req.val.span);
    elements.push_back(
        NamedYaboVal{QString::number(i + req.array_start_index), new_val});
  }
  std::optional<YaboVal> continuation = {};
  if (idx == array_fetch_size) {
    continuation = current;
  }
  ValVecResponse ret{std::move(elements), continuation};
  return Response(req.metadata, std::move(ret));
}

std::optional<Response> Executor::execute_request(Request req) {
  switch (req.metadata.kind) {
  case MessageType::FIELDS: {
    return get_fields(req);
  }
  case MessageType::ARRAY_ELEMENTS: {
    if (req.val.kind() == YaboValKind::YABOBLOCK) {
      return get_list_members(req);
    }
    return get_array_members(req);
  }
  case MessageType::DEREF: {
    auto normalized = normalize(req.val, req.val.span);
    auto name = reinterpret_cast<NominalVTable *>(req.val->vtable)->name;
    return Response(req.metadata, {name, normalized});
  }
  case MessageType::PARSE:
  case MessageType::ERROR: {
    // parse requests come in via the execute_parser slot
    return {};
  }
  default:
    return {};
  }
}

std::optional<Response>
Executor::execute_parser(Meta meta, char const *func_name, size_t pos) {
  auto parser_ptr = reinterpret_cast<ParseFun const *>(dlsym(lib, func_name));
  if (!parser_ptr) {
    return {};
  }
  auto parser = *parser_ptr;
  auto span = file->span();
  if (pos >= span.size()) {
    return {};
  }
  span = span.subspan(pos, span.size() - pos);
  auto ret = vals.parse(parser, span);
  if (ret.has_value()) {
    auto normalized = normalize(ret.value(), ret->span);
    return Response(meta, {func_name, normalized});
  }
  return {};
}

Executor::DerefInfo Executor::deref(YaboVal val) {
  auto it = deref_cache.find(val);
  if (it != deref_cache.end()) {
    return it->second;
  }
  auto ret = vals.deref(val);
  std::optional<FileSpan> span;
  if (ret.has_value()) {
    span = vals.extent(val);
  }
  auto info = DerefInfo{ret, span};
  deref_cache.insert({val, info});
  return info;
}

SpannedVal Executor::normalize(YaboVal val, FileSpan parent_span) {
  std::optional<SpannedVal> first_outside;
  bool active = false;
  while (true) {
    if (val.kind() == YaboValKind::YABOU8) {
      const uint8_t *start = val.access_u8();
      return SpannedVal(val, FileSpan(start, 1), true);
    }
    auto deref_info = deref(val);
    if (!deref_info.val.has_value()) {
      break;
    }
    if (deref_info.span.has_value()) {
      active = true;
      if (!span_contains(parent_span, deref_info.span.value())) {
        first_outside = SpannedVal(val, deref_info.span.value(), true);
      }
      parent_span = deref_info.span.value();
    }
    val = deref_info.val.value();
  }
  if (val.kind() == YaboValKind::YABOARRAY) {
    auto extent = vals.extent(val);
    if (extent.has_value()) {
      parent_span = extent.value();
      active = true;
    }
  }
  if (first_outside.has_value() && (val.kind() == YaboValKind::YABOARRAY ||
                                    val.kind() == YaboValKind::YABOBLOCK)) {
    return first_outside.value();
  }
  return SpannedVal(val, parent_span, active);
}