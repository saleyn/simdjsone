//------------------------------------------------------------------------------
// Author: Serge Aleynikov <saleyn at gmail dot com>
//------------------------------------------------------------------------------
// Copied from https://github.com/saleyn/fix/c_src/util.hpp
//------------------------------------------------------------------------------
#pragma once

#include <vector>
#include <cmath>
#include <cstdint>
#include <erl_nif.h>
#include "simdjson.h"

namespace simdjsone {

using namespace simdjson;

// Unfortunately there's no NIF support for big integers.  We use the following
// workaround - encode the integer from a string into the binary term format.
// Then use the 'enif_binary_to_term' to convert that into an Erlang term, which
// represents a big number.  This approach works well for integers that have
// fewer than 256 digits.

struct BigInt {
  // Creates a small BigInteger (< 256 digits) Erlang term that corresponds to
  // a number stored as a string.  E.g. "12345678901234567890123"
  // will be returned as a BigInt 12345678901234567890123.
  // The function returns 0 if the decoding fails.
  static ERL_NIF_TERM
  decode(ErlNifEnv* env, const char* begin, const char* end)
  {
    auto neg = false;

    if (begin != end && *begin == '-') {
      ++begin;
      neg = true;
    }

    std::vector<unsigned char> result;
    auto len    = approx_digits(end - begin);
    auto offset = 3;
    constexpr const size_t add_size = 4 + 3 /* maybe Large Big integer */; 
    result.reserve(len + add_size);
    result.push_back(131);
    result.push_back(int('o'));           // Large BIG integer ERL_LARGE_BIG_EXT (111)
    result.push_back(0);
    result.push_back(131);                // Version byte
    result.push_back(int('n'));           // Small BIG integer ERL_SMALL_BIG_EXT (110)
    result.push_back(0);                  // Length placeholder
    result.push_back(neg);                // Sign byte
    convert_to_base256(result, result.begin() + add_size, begin, end);
    auto size = result.size() - add_size; // Get byte length

    if (size <= 255) [[likely]]
      result[3+2] = size;                 // Update byte length
    else {
      put32be(result.data() + 2, size);   // For Large BIG integers the length is 4 bytes
      offset = 0;
    }

    ERL_NIF_TERM out;
    if (!enif_binary_to_term(env, result.data()+offset, result.size()-offset, &out, 0)) [[unlikely]]
      return 0;
    return out;
  }

private:
  static inline void convert_to_base256(
    std::vector<unsigned char>& result,
    std::vector<unsigned char>::iterator it,
    const char* begin, const char* end)
  {
    result.push_back(*begin++ - '0');

    for(; begin != end; ++begin) {
      mul10(result, it);           // multiply result by 10
      add(result, it, *begin-'0'); // add current digit
    }
  }

  static inline size_t approx_digits(size_t decimal_dig_count) {
    const auto factor = std::log(10) / std::log(256);
    return std::ceil(factor * decimal_dig_count);
  }

  static inline void put32be(uint8_t* s, uint32_t n) {
    *s++ = (n >> 24) & 0xff;
    *s++ = (n >> 16) & 0xff;
    *s++ = (n >>  8) & 0xff;
    *s   = n         & 0xff;
  }

  static inline void add
  (
    std::vector<unsigned char>&          num,
    std::vector<unsigned char>::iterator start,
    int                                  digit
  ) {
    for (auto end=num.end(); digit && start != end; ++start) {
      int x  = *start + digit;
      *start = x & 255;
      digit  = x >> 8;
    }
    if (digit)
      num.push_back(digit);
  }

  static inline void mul10(
    std::vector<unsigned char>& num,
    std::vector<unsigned char>::iterator start
  ) {
    auto carry = 0;
    for (auto end=num.end(); start != end; ++start) {
      carry   = 10 * *start + carry;
      *start  = static_cast<uint8_t>(carry);
      carry >>= 8;
    }
    if (carry)
      num.push_back(carry);
  }
};

} // namespace simdjsone
