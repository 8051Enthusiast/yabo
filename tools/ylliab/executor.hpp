#pragma once
#include <QObject>
#include <filesystem>

#include "dataprovider.hpp"
#include "filecontent.hpp"
#include "request.hpp"
#include "yabo.hpp"

struct ExecutorError : public std::exception {
  ExecutorError(std::string msg) : message(msg) {}
  std::string what() { return message; }
  std::string message;
};

constexpr size_t array_fetch_size = 4096;

class Executor : public DataProvider {
  Q_OBJECT
public:
  Executor(std::filesystem::path path, FileRef file);
  ~Executor();
  // we need to delete the copy constructor and assignment operator because
  // we don't want to copy the library
  Executor(const Executor &) = delete;
  Executor &operator=(const Executor &) = delete;
  Executor(Executor &&) = delete;
  Executor &operator=(Executor &&) = delete;

  std::optional<Response> execute_request(Request req);
  std::optional<Response> execute_parser(Meta meta, char const *func_name,
                                         size_t pos);
public slots:
  void execute_request_slot(Request req) override {
    if (thread_init()) {
      emit response(Response(req.metadata));
      return;
    }
    auto resp = execute_request(req);
    if (resp.has_value()) {
      emit response(std::move(resp.value()));
    } else {
      emit response(Response(req.metadata));
    }
  }
  void execute_parser_slot(Meta meta, QString func_name, size_t pos) override {
    if (thread_init()) {
      emit response(Response(meta));
      return;
    }
    auto s = func_name.toStdString();
    auto resp = execute_parser(meta, s.c_str(), pos);
    if (resp.has_value()) {
      emit response(std::move(resp.value()));
    } else {
      emit response(Response(meta));
    }
  }
signals:
  void response(Response resp);

private:
  int64_t thread_init();
  std::optional<Response> get_fields(Request &req);
  std::optional<Response> get_array_members(Request &req);
  std::optional<Response> get_list_members(Request &req);
  YaboVal from_handle(ValHandle handle) const noexcept;
  SpannedVal from_spanned_handle(SpannedHandle handle) const noexcept;
  SpannedVal normalize(YaboVal val, ByteSpan parent_span);
  SpannedHandle normalize(YaboVal val, FileSpan parent_span);
  struct DerefInfo {
    std::optional<YaboVal> val;
    std::optional<ByteSpan> span;
  };
  std::unordered_map<YaboVal, DerefInfo> deref_cache;
  DerefInfo deref(YaboVal val);
  YaboValCreator vals;
  void *lib;
  std::unique_ptr<char[]> altstack = nullptr;
  std::filesystem::path tmp_file;
  FileRef file;
};