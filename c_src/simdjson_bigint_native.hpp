//------------------------------------------------------------------------------
// Author: Serge Aleynikov <saleyn at gmail dot com>
//------------------------------------------------------------------------------
// Highly optimized BigInt implementation that uses efficient algorithms
// similar to what the Erlang VM uses internally
//------------------------------------------------------------------------------
#pragma once

#include <string>
#include <string_view>
#include <cstring>
#include <algorithm>
#include <erl_nif.h>
#include "simdjson.h"

namespace simdjsone {

using namespace simdjson;

// Fast BigInt implementation optimized for performance
struct BigIntFast {

  static ERL_NIF_TERM
  decode(ErlNifEnv* env, const char* begin, const char* end)
  {
    if (begin >= end) [[unlikely]]
      return 0;

    // Fast path validation and conversion
    return parse_decimal_string_fast(env, begin, end);
  }

  static std::string
  encode(ErlNifEnv* env, ERL_NIF_TERM term)
  {
    // Fast path for regular integers
    ErlNifSInt64 small_val;
    if (enif_get_int64(env, term, &small_val)) [[likely]] {
      return int_to_string_fast(small_val);
    }

    // Handle big integers efficiently
    return bigint_to_string_fast(env, term);
  }

private:

  // Ultra-fast integer to string conversion (faster than std::to_string)
  static std::string int_to_string_fast(ErlNifSInt64 value) {
    if (value == 0) return "0";

    bool negative = value < 0;
    if (negative) value = -value;

    char buffer[32];
    char* p = buffer + sizeof(buffer);
    *--p = '\0';

    // Convert digits
    do {
      *--p = '0' + (value % 10);
      value /= 10;
    } while (value);

    if (negative) *--p = '-';

    return std::string(p);
  }

  // Fast decimal string parsing with early integer detection
  static ERL_NIF_TERM parse_decimal_string_fast(ErlNifEnv* env, const char* begin, const char* end) {
    size_t len = end - begin;
    bool negative = false;

    // Handle sign
    if (*begin == '-') {
      negative = true;
      ++begin;
      --len;
    } else if (*begin == '+') {
      ++begin;
      --len;
    }

    if (len == 0) [[unlikely]] return 0;

    // Skip leading zeros
    while (len > 1 && *begin == '0') {
      ++begin;
      --len;
    }

    // Fast path: try to parse as regular 64-bit integer first
    if (len <= 18) { // 64-bit signed max is 19 digits, be conservative
      ErlNifSInt64 result = 0;
      bool overflow = false;

      for (size_t i = 0; i < len; ++i) {
        char c = begin[i];
        if (c < '0' || c > '9') [[unlikely]] return 0;

        int digit = c - '0';

        // Check for overflow before multiplication
        if (result > (LLONG_MAX / 10)) {
          overflow = true;
          break;
        }

        result = result * 10 + digit;
      }

      if (!overflow) {
        return negative ? enif_make_int64(env, -result) : enif_make_int64(env, result);
      }
    }

    // For larger numbers, use optimized bigint creation
    return create_bigint_fast(env, begin, len, negative);
  }

  // Optimized bigint creation using more efficient algorithm
  static ERL_NIF_TERM create_bigint_fast(ErlNifEnv* env, const char* digits, size_t len, bool negative) {
    // Use more efficient base conversion algorithm
    std::vector<uint32_t> result;
    result.reserve(len / 9 + 2); // Each uint32 holds ~9 decimal digits

    // Process in chunks of 9 digits for efficiency
    size_t remaining = len;
    const char* ptr = digits;

    // Handle full chunks of 9 digits
    while (remaining >= 9) {
      // Parse 9 digits at once
      uint32_t chunk = 0;
      for (int i = 0; i < 9; ++i) {
        chunk = chunk * 10 + (ptr[i] - '0');
      }

      // Multiply existing result by 10^9 and add chunk
      multiply_by_billion(result);
      add_uint32(result, chunk);

      ptr += 9;
      remaining -= 9;
    }

    // Handle remaining digits
    if (remaining > 0) {
      uint32_t chunk = 0;
      uint32_t multiplier = 1;

      for (size_t i = 0; i < remaining; ++i) {
        chunk = chunk * 10 + (ptr[i] - '0');
        multiplier *= 10;
      }

      multiply_by(result, multiplier);
      add_uint32(result, chunk);
    }

    // Convert to Erlang binary format
    return convert_to_erlang_bigint(env, result, negative);
  }

  // Multiply bigint by 10^9 (billion) - optimized operation
  static void multiply_by_billion(std::vector<uint32_t>& number) {
    constexpr uint32_t billion = 1000000000u;
    uint64_t carry = 0;

    for (auto& digit : number) {
      uint64_t product = static_cast<uint64_t>(digit) * billion + carry;
      digit = static_cast<uint32_t>(product);
      carry = product >> 32;
    }

    if (carry) {
      number.push_back(static_cast<uint32_t>(carry));
    }
  }

  // Multiply by arbitrary value
  static void multiply_by(std::vector<uint32_t>& number, uint32_t multiplier) {
    uint64_t carry = 0;

    for (auto& digit : number) {
      uint64_t product = static_cast<uint64_t>(digit) * multiplier + carry;
      digit = static_cast<uint32_t>(product);
      carry = product >> 32;
    }

    while (carry) {
      number.push_back(static_cast<uint32_t>(carry));
      carry >>= 32;
    }
  }

  // Add 32-bit value to bigint
  static void add_uint32(std::vector<uint32_t>& number, uint32_t value) {
    if (number.empty()) {
      if (value != 0) number.push_back(value);
      return;
    }

    uint64_t carry = value;
    for (auto& digit : number) {
      carry += digit;
      digit = static_cast<uint32_t>(carry);
      carry >>= 32;
      if (carry == 0) break;
    }

    if (carry) {
      number.push_back(static_cast<uint32_t>(carry));
    }
  }

  // Convert internal representation to Erlang bigint format
  static ERL_NIF_TERM convert_to_erlang_bigint(ErlNifEnv* env, const std::vector<uint32_t>& number, bool negative) {
    if (number.empty()) {
      return enif_make_int(env, 0);
    }

    // Convert uint32 array to byte array (little endian)
    std::vector<uint8_t> bytes;
    bytes.reserve(number.size() * 4);

    for (uint32_t word : number) {
      bytes.push_back(word & 0xFF);
      bytes.push_back((word >> 8) & 0xFF);
      bytes.push_back((word >> 16) & 0xFF);
      bytes.push_back((word >> 24) & 0xFF);
    }

    // Remove trailing zeros
    while (bytes.size() > 1 && bytes.back() == 0) {
      bytes.pop_back();
    }

    // Create Erlang binary term format
    std::vector<uint8_t> term_data;
    term_data.reserve(bytes.size() + 8);

    term_data.push_back(131); // Version tag

    if (bytes.size() <= 255) {
      // Small bignum
      term_data.push_back('n'); // SMALL_BIG_EXT
      term_data.push_back(static_cast<uint8_t>(bytes.size()));
      term_data.push_back(negative ? 1 : 0);
    } else {
      // Large bignum
      term_data.push_back('o'); // LARGE_BIG_EXT
      uint32_t size = bytes.size();
      term_data.push_back(static_cast<uint8_t>(size >> 24));
      term_data.push_back(static_cast<uint8_t>(size >> 16));
      term_data.push_back(static_cast<uint8_t>(size >> 8));
      term_data.push_back(static_cast<uint8_t>(size));
      term_data.push_back(negative ? 1 : 0);
    }

    term_data.insert(term_data.end(), bytes.begin(), bytes.end());

    ERL_NIF_TERM result;
    if (!enif_binary_to_term(env, term_data.data(), term_data.size(), &result, 0)) {
      return 0;
    }

    return result;
  }

  // Fast bigint to string conversion
  static std::string bigint_to_string_fast(ErlNifEnv* env, ERL_NIF_TERM term) {
    ErlNifBinary bin;
    if (!enif_term_to_binary(env, term, &bin)) {
      return {};
    }

    std::string result = parse_bigint_binary_fast(bin.data, bin.size);
    enif_release_binary(&bin);
    return result;
  }

  // Fast parsing of Erlang bigint binary format
  static std::string parse_bigint_binary_fast(const uint8_t* data, size_t size) {
    if (size < 2 || data[0] != 131) return {};

    size_t pos = 1;
    uint8_t tag = data[pos++];

    bool negative = false;
    std::vector<uint8_t> bytes;

    if (tag == 'n') { // Small bignum
      if (pos + 2 > size) return {};
      size_t byte_len = data[pos++];
      negative = data[pos++] != 0;

      if (pos + byte_len > size) return {};
      bytes.assign(data + pos, data + pos + byte_len);

    } else if (tag == 'o') { // Large bignum
      if (pos + 5 > size) return {};

      size_t byte_len = (static_cast<size_t>(data[pos]) << 24) |
                       (static_cast<size_t>(data[pos+1]) << 16) |
                       (static_cast<size_t>(data[pos+2]) << 8) |
                       static_cast<size_t>(data[pos+3]);
      pos += 4;
      negative = data[pos++] != 0;

      if (pos + byte_len > size) return {};
      bytes.assign(data + pos, data + pos + byte_len);

    } else {
      return {}; // Not a bignum
    }

    return convert_bytes_to_decimal_fast(bytes, negative);
  }

  // Ultra-fast base256 to decimal conversion using optimized algorithm
  static std::string convert_bytes_to_decimal_fast(const std::vector<uint8_t>& bytes, bool negative) {
    if (bytes.empty()) return "0";

    // Convert byte array to uint32 array for faster processing
    std::vector<uint32_t> words;
    words.reserve((bytes.size() + 3) / 4);

    for (size_t i = 0; i < bytes.size(); i += 4) {
      uint32_t word = 0;
      for (int j = 0; j < 4 && i + j < bytes.size(); ++j) {
        word |= static_cast<uint32_t>(bytes[i + j]) << (j * 8);
      }
      words.push_back(word);
    }

    // Convert to decimal using optimized division
    std::vector<char> digits;
    digits.reserve(bytes.size() * 3); // Conservative estimate

    // Repeatedly divide by 10^9 for efficiency
    while (!is_zero(words)) {
      uint32_t remainder = divide_by_billion(words);

      // Convert remainder to 9 decimal digits
      for (int i = 0; i < 9; ++i) {
        digits.push_back('0' + (remainder % 10));
        remainder /= 10;
        if (remainder == 0 && is_zero(words)) break;
      }
    }

    if (digits.empty()) return "0";

    // Build result string
    std::string result;
    if (negative) result += '-';

    // Remove leading zeros and reverse
    while (!digits.empty() && digits.back() == '0') {
      digits.pop_back();
    }

    std::reverse(digits.begin(), digits.end());
    result.append(digits.begin(), digits.end());

    return result;
  }

  // Check if bigint is zero
  static bool is_zero(const std::vector<uint32_t>& words) {
    for (uint32_t word : words) {
      if (word != 0) return false;
    }
    return true;
  }

  // Divide bigint by 10^9, return remainder
  static uint32_t divide_by_billion(std::vector<uint32_t>& words) {
    constexpr uint32_t billion = 1000000000u;
    uint64_t remainder = 0;

    // Process from most significant to least significant
    for (auto it = words.rbegin(); it != words.rend(); ++it) {
      uint64_t dividend = (remainder << 32) | *it;
      *it = static_cast<uint32_t>(dividend / billion);
      remainder = dividend % billion;
    }

    // Remove leading zeros
    while (!words.empty() && words.back() == 0) {
      words.pop_back();
    }

    return static_cast<uint32_t>(remainder);
  }
};

} // namespace simdjsone