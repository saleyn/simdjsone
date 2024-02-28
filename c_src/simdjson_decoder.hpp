#pragma once

#include <erl_nif.h>
#include <utility>
#include <unordered_set>
#include "simdjson.h"
#include "simdjson_atoms.hpp"
#include "simdjson_bigint.hpp"

#if __cplusplus < 202101L
namespace std {
  [[noreturn]] inline void unreachable()
  {
      // Uses compiler specific extensions if possible.
      // Even if no extension is used, undefined behavior is still raised by
      // an empty function body and the noreturn attribute.
  #if defined(_MSC_VER) && !defined(__clang__) // MSVC
    __assume(false);
  #else // GCC, Clang
    __builtin_unreachable();
  #endif
  }
}
#endif

namespace simdjsone {

struct DecodeOpts {
  DecodeOpts()
  : return_maps(true)
  , null_term(am_null)
  , dedupe_keys(false)
  {}

  bool          return_maps;
  ERL_NIF_TERM  null_term;
  bool          dedupe_keys;
};

using namespace simdjson;

struct OnDemandDecoder {
  OnDemandDecoder(ErlNifEnv* env, const DecodeOpts& opts);
  inline ERL_NIF_TERM to_json(ErlNifBinary const& bin);
private:
  template <typename T>
  std::pair<bool, ERL_NIF_TERM> decode_number(T& doc) noexcept;
  std::pair<bool, ERL_NIF_TERM> decode_number(ondemand::number num) noexcept;
  inline ERL_NIF_TERM unescape_string(ondemand::raw_json_string rjs, size_t size,
                                      std::unordered_set<std::string>* seen, ERL_NIF_TERM& err);
  inline std::pair<bool, ERL_NIF_TERM> recursive_processor(ondemand::value element);
  void release_binaries(std::vector<ERL_NIF_TERM>& items);
  inline ERL_NIF_TERM raise_error(ERL_NIF_TERM, const char* err);

  ondemand::parser m_parser;
  ErlNifEnv*       m_env;
  DecodeOpts       m_opts;
  padded_string    m_buff;
};

OnDemandDecoder::OnDemandDecoder(ErlNifEnv* env, const DecodeOpts& opts)
  : m_env(env)
  , m_opts(opts)
{}

ERL_NIF_TERM OnDemandDecoder::to_json(ErlNifBinary const& bin) {
  padded_string json(reinterpret_cast<const char*>(bin.data), bin.size);
  auto buf = padded_string(bin.size);
  m_buff.swap(buf);

  ondemand::document doc = m_parser.iterate(json);
  ERL_NIF_TERM res, errcode = 0;
  if (doc.is_scalar()) {
    // we have a special case where the JSON document is a single document...
    switch (doc.type()) {
      case ondemand::json_type::number:
        res = decode_number(doc).second;
        break;
      case ondemand::json_type::string:
        res = unescape_string(doc.get_raw_json_string(), doc.raw_json_token().value_unsafe().size(), nullptr, errcode);
        if (errcode) [[unlikely]] return raise_error(errcode, "Failed to decode string");
        assert(res);
        break;
      case ondemand::json_type::boolean:
        res = doc.get_bool() ? AM_TRUE : AM_FALSE;
        break;
      case ondemand::json_type::null:
        // We check that the value is indeed null
        // otherwise: an error is thrown.
        res = doc.is_null() ? m_opts.null_term : raise_error(AM_ERROR, "NULL expected");
        break;
      case ondemand::json_type::array:
      case ondemand::json_type::object:
      default:
        // This is impossible
        res = raise_error(AM_ERROR, "Unreachable code");
    }
  } else {
    ondemand::value val = doc;
    res = recursive_processor(val).second;
  }

  assert(res);

  if (!doc.at_end())
    return raise_error(AM_BADARG, "Unexpectedly tokens after the end of the json");

  return res;
}

std::pair<bool, ERL_NIF_TERM>
OnDemandDecoder::recursive_processor(ondemand::value element)
{
  switch (element.type()) {
    case ondemand::json_type::array: {
      std::vector<ERL_NIF_TERM> items;
      for (auto child : element.get_array()) {
        auto res = recursive_processor(child.value());
        if (!res.first) [[unlikely]] {
          release_binaries(items);
          return res;
        }
        items.push_back(res.second);
      }
      return std::make_pair(true, enif_make_list_from_array(m_env, items.data(), items.size()));
    }
    case ondemand::json_type::object: {
      std::vector<ERL_NIF_TERM> keys;
      std::vector<ERL_NIF_TERM> vals;
      std::unordered_set<std::string> seen;
      ERL_NIF_TERM errcode;
      auto pseen   = m_opts.dedupe_keys ? &seen : nullptr;
      for (auto field : element.get_object()) {
        // TODO: add error checking for field.key?
        auto k = unescape_string(field.key(), field.key_raw_json_token().value_unsafe().size(), pseen, errcode);
        if (errcode) [[unlikely]]
          goto ERR;
        if (!k) [[unlikely]]
          continue;
        keys.push_back(k);
        auto res = recursive_processor(field.value());
        if (!res.first) [[unlikely]] {
          release_binaries(keys);
          release_binaries(vals);
          return res;
        }
        vals.push_back(res.second);
      }
      ERL_NIF_TERM m;
      if (m_opts.return_maps) {
        if (!enif_make_map_from_arrays(m_env, keys.data(), vals.data(), keys.size(), &m)) [[unlikely]] {
          errcode = AM_DUP_KEYS_FOUND;
          goto ERR;
        }
      } else {
        std::vector<ERL_NIF_TERM> v;
        v.resize(keys.size());
        for (size_t i=0; i < v.size(); ++i)
          v[i] = enif_make_tuple2(m_env, keys[i], vals[i]);
        m = enif_make_list_from_array(m_env, v.data(), v.size());
        m = enif_make_tuple1(m_env, m);
      }
      return std::make_pair(true, m);
    ERR:
      release_binaries(keys);
      release_binaries(vals);
      return std::make_pair(false,
        raise_error(errcode, "Dup keys found in JSON object"));
    }
    case ondemand::json_type::number:
      return decode_number(element);

    case ondemand::json_type::string: {
      ERL_NIF_TERM err = 0;
      auto s = unescape_string(element.get_raw_json_string(), element.raw_json_token().size(), nullptr, err);
      if (err) [[unlikely]] return std::make_pair(false, raise_error(AM_ENOMEM, "Not enough memory"));
      assert(s);
      return std::make_pair(true, s);
    }
    case ondemand::json_type::boolean: {
      bool val = false;
      auto err = element.get(val);
      return std::make_pair(err == SUCCESS, val ? AM_TRUE : AM_FALSE);
    }
    case ondemand::json_type::null:
      // We check that the value is indeed null
      // otherwise: an error is thrown.
      return element.is_null()
          ? std::make_pair(true,  m_opts.null_term)
          : std::make_pair(false, raise_error(AM_NULL, "Invalid NULL"));
    default:
      return std::make_pair(false, raise_error(AM_ERROR, "Unhandled switch clause"));
  }
}

template <typename T>
std::pair<bool, ERL_NIF_TERM>
OnDemandDecoder::decode_number(T& val) noexcept {
  auto res = val.get_number();
  if (res.error() != SUCCESS) [[unlikely]] {
    std::string_view str = val.raw_json();
    auto val = BigInt::decode(m_env, str.begin(), str.end());
    return std::make_pair(true, val);
  }
  return decode_number(res.value_unsafe());
}

std::pair<bool, ERL_NIF_TERM>
OnDemandDecoder::decode_number(ondemand::number num) noexcept {
  using ondemand::number_type;

  switch (num.get_number_type()) {
    case number_type::floating_point_number:
      return std::make_pair(true, enif_make_double(m_env, num.get_double()));
    case number_type::signed_integer:
      return std::make_pair(true, enif_make_int64(m_env, num.get_int64()));
    case number_type::unsigned_integer:
      return std::make_pair(true, enif_make_uint64(m_env, num.get_uint64()));
    case number_type::big_integer:
      // This should be handled by the "if" clause in the beginning of this function
      return std::make_pair(false, raise_error(AM_ERROR, "Unexpected number type"));
    default:
      std::unreachable();
  }
}

inline ERL_NIF_TERM OnDemandDecoder::
unescape_string(ondemand::raw_json_string in, size_t size, std::unordered_set<std::string>* seen, ERL_NIF_TERM& err) {
  auto dst = reinterpret_cast<uint8_t*>(m_buff.data());
  std::string_view v = m_parser.unescape(in, dst);
  if (seen) {
    auto [it, ok] = seen->insert(std::string(v));
    if (!ok) [[unlikely]]
      return err = 0;
  }
  err = 0;
  return make_binary(m_env, v);
}

ERL_NIF_TERM OnDemandDecoder::raise_error(ERL_NIF_TERM reason, const char* err)
{
  return ::raise_error(m_env, reason, err);
}

void OnDemandDecoder::release_binaries(std::vector<ERL_NIF_TERM>& items)
{
  for (auto t : items)
    if (enif_is_binary(m_env, t)) {
      ErlNifBinary bin;
      enif_inspect_binary(m_env, t, &bin);
      enif_release_binary(&bin);
    }
}

} // namespace simdjsone