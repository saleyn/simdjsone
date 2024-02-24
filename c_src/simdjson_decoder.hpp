#pragma once

#include <erl_nif.h>
#include <utility>
#include "simdjson.h"
#include "simdjson_atoms.hpp"
#include "simdjson_bigint.hpp"

#define SIMDJSON_GCC_COMPILER ((__GNUC__) && !(__clang__) && !(__INTEL_COMPILER))

namespace simdjsone {

#if SIMDJSON_GCC_COMPILER
  // the GCC compiler does well with by-value passing.
  // GCC has superior recursive inlining:
  // https://stackoverflow.com/questions/29186186/why-does-gcc-generate-a-faster-program-than-clang-in-this-recursive-fibonacci-co
  // https://godbolt.org/z/TeK4doE51
  using ValueType = simdjson::SIMDJSON_BUILTIN_IMPLEMENTATION::ondemand::value;
#else
  using ValueType = simdjson::SIMDJSON_BUILTIN_IMPLEMENTATION::ondemand::value&;
#endif

using namespace simdjson;

struct OnDemandDecoder {
  struct Writer {
    Writer(ErlNifEnv* env) : m_env(env), m_res(0), m_err(nullptr) {}

    void append_s64(int64_t val) {
      if (!enif_make_int64(m_env, val)) [[unlikely]]
        m_err = "Failed to decode int64 value";
    }

    void append_u64(uint64_t val) {
      if (!enif_make_uint64(m_env, val)) [[unlikely]]
        m_err = "Failed to decode uint64 value";
    }

    void append_double(double val) {
      if (!enif_make_double(m_env, val)) [[unlikely]]
        m_err = "Failed to decode double value";
    }

    ERL_NIF_TERM value() const { return m_res; }
    const char*  error() const { return m_err; }
  private:
    ErlNifEnv*   m_env;
    ERL_NIF_TERM m_res;
    const char*  m_err;
  };

  OnDemandDecoder(ErlNifEnv* env);
  inline ERL_NIF_TERM to_json(ErlNifBinary const& bin);
private:
  inline ERL_NIF_TERM decode_number(simdjson::ondemand::raw_json_string d) noexcept;
  inline ERL_NIF_TERM decode_raw_string(simdjson::ondemand::raw_json_string rjs);
  inline void recursive_processor(ValueType element);

  simdjson::ondemand::parser m_parser;
  ErlNifEnv*                 m_env;
};

ERL_NIF_TERM OnDemandDecoder::to_json(ErlNifBinary const& bin) {
  simdjson::padded_string json(reinterpret_cast<const char*>(bin.data), bin.size);

  ondemand::document doc = m_parser.iterate(json);
  ERL_NIF_TERM res;
  if (doc.is_scalar()) {
    // we have a special case where the JSON document is a single document...
    switch (doc.type()) {
      case simdjson::ondemand::json_type::number:
        res = decode_number(doc.get_raw_json_string());
        break;
      case simdjson::ondemand::json_type::string:
        res = decode_raw_string(doc.get_raw_json_string());
        break;
      case simdjson::ondemand::json_type::boolean:
        res = doc.get_bool() ? AM_TRUE : AM_FALSE;
        break;
      case simdjson::ondemand::json_type::null:
        // We check that the value is indeed null
        // otherwise: an error is thrown.
        if (doc.is_null()) res = am_null;
        break;
      case simdjson::ondemand::json_type::array:
      case simdjson::ondemand::json_type::object:
      default:
        res = AM_UNDEFINED;
        std::unreachable(); // impossible
    }
  } else {
    simdjson::ondemand::value val = doc;
    res = recursive_processor(val);
  }

  if (!doc.at_end())
    return raise_error(m_env,AM_BADARG, "Unexpectedly tokens after the end of the json");

  return res;
}

ERL_NIF_TERM OnDemandDecoder::decode_number(simdjson::ondemand::raw_json_string in) noexcept {
  using namespace simdjson::SIMDJSON_IMPLEMENTATION::numberparsing;

  bool   is_bigint;
  size_t digit_count;
  Writer writer(m_env);

  auto   str = reinterpret_cast<const uint8_t*>(in.raw());
  auto   err = number_parsing::parse_number(str, writer, is_bigint, digit_count);
  if    (err) [[unlikely]] {
    if (err == NUMBER_ERROR && is_bigint) {
      auto res = BigInt.decode(m_env, str, str + digit_count);
      if (!res)
        return raise_error(m_env, AM_BADARG, "Failed to decode big number");
    }
    return raise_error(m_env, err);
  }
  if (writer.error()) [[unlikely]]
    return raise_error(m_env, AM_ERROR, writer.error());

  return writer.value();
}

ERL_NIF_TERM OnDemandDecoder::decode_raw_string(ondemand::raw_json_string in) {
  std::string_view v = m_parser.unescape(in, false);
  return make_binary(m_env, v);
}


std::pair<bool, ERL_NIF_TERM>
OnDemandDecoder::recursive_processor
(
  ErlNifEnv* env,
  ValueType  element
)
{
  switch (element.type()) {
    case simdjson::ondemand::json_type::array: {
      std::vector<ERL_NIF_TERM> items;
      for (auto child : element.get_array()) {
        auto res = recursive_processor(env, child.value());
        if (!res.first) [[unlikely]] {
          release_binaries(items);
          return res;
        }
        items.push_back(res.second);
      }
      return std::make_pair(true, enif_make_list_from_array(env, items.data(), items.size()));
    }
    case simdjson::ondemand::json_type::object: {
      std::vector<ERL_NIF_TERM> keys;
      std::vector<ERL_NIF_TERM> vals;
      for (auto field : element.get_object()) {
        keys.push_back(make_binary(env, field.key()));
        auto res = recursive_processor(env, field.value());
        if (!res.first) [[unlikely]] {
          release_binaries(keys);
          release_binaries(vals);
          return res;
        }
        vals.push_back(res.second);
      }
      ERL_NIF_TERM m;
      if (!enif_make_map_from_arrays(env, keys.data(), vals.data(), keys.size(), &m)) [[unlikely]] {
        release_binaries(keys);
        release_binaries(vals);
        return std::make_pair(false,
          enif_raise_exception(env,
            enif_make_tuple2(env, AM_DUP_KEYS_FOUND, make_binary("Dup keys found in JSON object"))));
      }
      return std::make_pair(true, m);
    }
    case simdjson::ondemand::json_type::number: {
      double v;
      auto err = element.get(v);
      if (err != SUCCESS) [[unlikely]]
        return std::make_pair(false, enif_raise_exception(env, error_reason(m_env, err.error())));
      return std::make_pair(true, enif_make_double(env, v));
    }
    case simdjson::ondemand::json_type::string: {
      auto err = element.get_raw_json_string();
      if (err.error() != SUCCESS) [[unlikely]]
        return std::make_pair(false, enif_raise_exception(m_env, error_reason(m_env, err.error())));
      return std::make_pair(true, make_binary(m_env, err.value()));
    }
    case simdjson::ondemand::json_type::boolean: {
      bool val = false;
      auto err = element.get(val);
      return std::make_pair(err == SUCCESS, val ? AM_TRUE : AM_FALSE);
    }
    case simdjson::ondemand::json_type::null:
      // We check that the value is indeed null
      // otherwise: an error is thrown.
      return element.is_null()
          ? std::make_pair(true, am_null)
          : enif_raise_exception(env, make_tuple2(m_env, AM_NULL, "Invalid NULL"));
    default:
      std::unreachable();
  }
}

struct simdjson_ondemand {
  using StringType = std::string_view;

  simdjson2msgpack parser{};

  bool run(simdjson::padded_string &json, char *buffer, std::string_view &result) {
    result = parser.to_msgpack(json, reinterpret_cast<uint8_t *>(buffer));
    return true;
  }
};

} // namespace simdjsone