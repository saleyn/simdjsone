#pragma once

#include <erl_nif.h>
#include "simdjson.h"

#define SIMDJSON_GCC_COMPILER ((__GNUC__) && !(__clang__) && !(__INTEL_COMPILER))

namespace simdjsone {

#if SIMDJSON_GCC_COMPILER
  // the GCC compiler does well with by-value passing.
  // GCC has superior recursive inlining:
  // https://stackoverflow.com/questions/29186186/why-does-gcc-generate-a-faster-program-than-clang-in-this-recursive-fibonacci-co
  // https://godbolt.org/z/TeK4doE51
  use ValueType = simdjson::ondemand::value;
#else
  use ValueType = simdjson::ondemand::value&;
#endif

using namespace simdjson;

struct OnDemandDecoder {
  OnDemandDecoder(ErlNifEnv* env);
  inline ERL_NIF_TERM to_json(const simdjson::padded_string &json,
                                     uint8_t *buf);

private:
  simdjson_inline void write_double(const double d) noexcept;
  simdjson_inline void write_byte(const uint8_t b) noexcept;
  simdjson_inline void write_uint32(const uint32_t w) noexcept;
  simdjson_inline void
  write_raw_string(simdjson::ondemand::raw_json_string rjs);
  inline void recursive_processor(ValueType element);

  simdjson::ondemand::parser m_parser;
  ErlNifEnv*                 m_env;
};

ERL_NIF_TERM OnDemandDecoder::to_json(ErlNifBinary const& bin) {
  simdjson::padded_string json(bin.data, bin.size);

  ondemand::document doc = parser.iterate(json);
  if (doc.is_scalar()) {
    // we have a special case where the JSON document is a single document...
    switch (doc.type()) {
      case simdjson::ondemand::json_type::number:
        write_double(doc.get_double());
        break;
      case simdjson::ondemand::json_type::string:
        write_raw_string(doc.get_raw_json_string());
        break;
      case simdjson::ondemand::json_type::boolean:
        write_byte(0xc2 + doc.get_bool());
        break;
      case simdjson::ondemand::json_type::null:
        // We check that the value is indeed null
        // otherwise: an error is thrown.
        if(doc.is_null()) {
          write_byte(0xc0);
        }
        break;
      case simdjson::ondemand::json_type::array:
      case simdjson::ondemand::json_type::object:
      default:
        std::unreachable(); // impossible
    }
  } else {
    simdjson::ondemand::value val = doc;
    recursive_processor(val);
  }
  if (!doc.at_end()) {
     throw "There are unexpectedly tokens after the end of the json in the json2msgpack sample data";
  }
  return std::string_view(reinterpret_cast<char *>(buf), size_t(buff - buf));
}

ERL_NIF_TERM OnDemandDecoder::write_double(const double d) noexcept {
  *buff++ = 0xcb;
  ::memcpy(buff, &d, sizeof(d));
  buff += sizeof(d);
}

ERL_NIF_TERM OnDemandDecoder::write_byte(const uint8_t b) noexcept {
  return enif_make_int(m_env, b);
}

ERL_NIF_TERM OnDemandDecoder::write_uint32(const uint32_t w) noexcept {
  return enif_make_uint(m_env, w);
}

ERL_NIF_TERM OnDemandDecoder::write_raw_string(
    simdjson::ondemand::raw_json_string in) {
  std::string_view v = parser.unescape(in, false);
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

void simdjson2msgpack::recursive_processor_ref(simdjson::ondemand::value& element) {
  switch (element.type()) {
  case simdjson::ondemand::json_type::array: {
    uint32_t counter = 0;
    write_byte(0xdd);
    uint8_t *location = skip_uint32();
    for (auto child : element.get_array()) {
      counter++;
      simdjson::ondemand::value v = child.value();
      recursive_processor_ref(v);
    }
    write_uint32_at(counter, location);
  } break;
  case simdjson::ondemand::json_type::object: {
    uint32_t counter = 0;
    write_byte(0xdf);
    uint8_t *location = skip_uint32();
    for (auto field : element.get_object()) {
      counter++;
      write_raw_string(field.key());
      simdjson::ondemand::value v = field.value();
      recursive_processor_ref(v);
    }
    write_uint32_at(counter, location);
  } break;
  case simdjson::ondemand::json_type::number:
    write_double(element.get_double());
    break;
  case simdjson::ondemand::json_type::string:
    write_raw_string(element.get_raw_json_string());
    break;
  case simdjson::ondemand::json_type::boolean:
    write_byte(0xc2 + element.get_bool());
    break;
  case simdjson::ondemand::json_type::null:
    // We check that the value is indeed null
    // otherwise: an error is thrown.
    if(element.is_null()) {
      write_byte(0xc0);
    }
    break;
  default:
    SIMDJSON_UNREACHABLE();
  }
}

struct simdjson_ondemand {
  using StringType = std::string_view;

  simdjson2msgpack parser{};

  bool run(simdjson::padded_string &json, char *buffer,
           std::string_view &result) {
    result = parser.to_msgpack(json, reinterpret_cast<uint8_t *>(buffer));
    return true;
  }
};

BENCHMARK_TEMPLATE(json2msgpack, simdjson_ondemand)->UseManualTime();

} // namespace simdjsone