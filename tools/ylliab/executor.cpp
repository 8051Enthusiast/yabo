#include "executor.hpp"
#include "request.hpp"
#include "yabo.hpp"
#include "yabo/dynamic.h"
#include "yabo/parse_export_call.h"
#include "yabo/vtable.h"

#include <dlfcn.h>
#ifdef __linux__
#include <csignal>
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

int64_t Executor::thread_init() {
  if (lib) {
    return 0;
  }
#ifdef __linux__
  altstack = std::make_unique<char[]>(MINSIGSTKSZ);
  stack_t ss;
  ss.ss_sp = altstack.get();
  ss.ss_flags = 0;
  ss.ss_size = MINSIGSTKSZ;
  if (sigaltstack(&ss, nullptr)) {
    return -1;
  }
#endif
  lib = dlopen(tmp_file.c_str(), RTLD_LAZY);
  if (!lib) {
    return -1;
  }

  try {
    vals = init_vals_from_lib(lib, file->slice());
  } catch (std::runtime_error &) {
    dlclose(lib);
    lib = nullptr;
    return -1;
  }
  return 0;
}

Executor::~Executor() {
  if (lib) {
    dlclose(lib);
  }
#ifdef __linux__
  if (altstack) {
    stack_t ss;
    ss.ss_flags = SS_DISABLE;
    sigaltstack(&ss, nullptr);
  }
#endif
  std::filesystem::remove(tmp_file);
}

std::optional<Response> Executor::get_fields(Request &req) {
  std::vector<NamedYaboVal> fields;
  auto val = from_spanned_handle(req.val);
  const auto vtable = reinterpret_cast<BlockVTable *>(val->vtable);
  const auto fields_table = YABO_ACCESS_VPTR(vtable, fields);
  auto field_count = dyn_block_field_count(val.val);
  for (size_t i = 0; i < fields_table->number_fields; i++) {
    auto name = YABO_ACCESS_VPTR(fields_table, fields[i]);
    auto field_val = vals.access_field(val, name);
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
  auto val = from_spanned_handle(req.val);
  uint64_t len = vals.array_len(val);
  uint64_t start = req.array_start_index;
  uint64_t end = std::min((uint64_t)array_fetch_size, len);
  for (uint64_t i = 0; i < end; i++) {
    auto idx_val = vals.index(val, i);
    if (idx_val.has_value()) {
      auto new_val = normalize(idx_val.value(), req.val.span);
      elements.push_back(NamedYaboVal{QString::number(start + i), new_val});
    } else {
      return {};
    }
  }
  std::optional<ValHandle> continuation = {};
  if (array_fetch_size < len) {
    auto cont = vals.skip(val, array_fetch_size);
    continuation = ValHandle(*cont);
  }
  ValVecResponse ret{std::move(elements), continuation};
  return Response(req.metadata, std::move(ret));
}

std::optional<Response> Executor::get_list_members(Request &req) {
  std::vector<YaboVal> ret_vals;
  YaboVal current = from_spanned_handle(req.val);
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
    if (req.val.kind == YaboValKind::YABOBLOCK) {
      return get_list_members(req);
    }
    return get_array_members(req);
  }
  case MessageType::DEREF: {
    auto val = from_spanned_handle(req.val);
    auto normalized = normalize(val, req.val.span);
    const auto vtable = reinterpret_cast<NominalVTable *>(val->vtable);
    const auto name = YABO_ACCESS_VPTR(vtable, name);
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

namespace {
const ParserExport *get_export(void *lib, char const *func_name) {
  size_t end = yabo_export_identifier_end(func_name);
  auto id_part = std::string_view(func_name, end);
  auto part = std::string(id_part);
  return reinterpret_cast<const ParserExport *>(dlsym(lib, part.c_str()));
}

std::optional<std::vector<char>> get_args(const ParserExport *parser,
                                          char const *func_name) {
  func_name += yabo_export_identifier_end(func_name);
  auto size = yabo_export_args_size(parser);
  if (size == -1) {
    return {};
  }
  std::vector<char> args(size, 0);
  auto err = yabo_export_parse_arg(func_name, parser, args.data());
  if (err) {
    return {};
  }
  return args;
}
} // namespace

std::optional<Response>
Executor::execute_parser(Meta meta, char const *func_name, size_t pos) {
  auto parser_ptr = get_export(lib, func_name);
  if (!parser_ptr) {
    return {};
  }
  auto parser = YABO_ACCESS_VPTR(parser_ptr, parser);
  auto span = file->segment_from(pos);
  if (!span.data()) {
    return {};
  }
  auto args = get_args(parser_ptr, func_name);
  if (!args) {
    return {};
  }
  auto ret = vals.parse(parser, args->data(), span);
  if (ret.has_value()) {
    auto normalized = normalize(ret.value(), ret->span);
    auto handle = SpannedHandle(normalized, file);
    return Response(meta, {func_name, handle});
  }
  return {};
}

Executor::DerefInfo Executor::deref(YaboVal val) {
  auto it = deref_cache.find(val);
  if (it != deref_cache.end()) {
    return it->second;
  }
  auto ret = vals.deref(val);
  std::optional<ByteSpan> span;
  if (ret.has_value()) {
    span = vals.extent(val);
  }
  auto info = DerefInfo{ret, span};
  deref_cache.insert({val, info});
  return info;
}

SpannedVal Executor::normalize(YaboVal val, ByteSpan parent_span) {
  std::optional<SpannedVal> first_outside;
  bool active = false;
  while (true) {
    if (val.kind() == YaboValKind::YABOU8) {
      const uint8_t *start = val.access_u8();
      return SpannedVal(val, ByteSpan(start, 1), true);
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

SpannedHandle Executor::normalize(YaboVal val, FileSpan parent_span) {
  auto span = file->byte_span(parent_span);
  SpannedVal new_val = normalize(val, span);
  return SpannedHandle(new_val, file);
}

YaboVal Executor::from_handle(ValHandle handle) const noexcept {
  return YaboVal(reinterpret_cast<DynValue *>(handle.handle));
}

SpannedVal Executor::from_spanned_handle(SpannedHandle handle) const noexcept {
  auto val = from_handle(ValHandle(handle.handle));
  return SpannedVal(val, file->byte_span(handle.span), handle.active);
}
