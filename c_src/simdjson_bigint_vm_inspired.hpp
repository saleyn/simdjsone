//------------------------------------------------------------------------------
// Author: Serge Aleynikov <saleyn at gmail dot com>
//------------------------------------------------------------------------------
// VM-Inspired BigInt implementation using NIF-safe functions
// This mimics the VM's algorithms but uses only safe NIF API functions
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

// Implementation inspired by VM's big.c algorithms but NIF-safe
struct BigIntVMInspired {

  static ERL_NIF_TERM decode(ErlNifEnv* env, const char* begin, const char* end) {
    if (begin >= end) [[unlikely]]
      return 0;

    return decode_decimal_vm_style(env, begin, end);
  }

  static std::string encode(ErlNifEnv* env, ERL_NIF_TERM term) {
    // Fast path for regular integers
    ErlNifSInt64 small_val;
    if (enif_get_int64(env, term, &small_val)) [[likely]] {
      return int64_to_string_vm_style(small_val);
    }

    // For bignums, use VM-style algorithms
    return bigint_to_string_vm_style(env, term);
  }

private:

  // VM-style decimal string parsing (based on chars_to_integer from big.c)
  static ERL_NIF_TERM decode_decimal_vm_style(ErlNifEnv* env, const char* begin, const char* end) {
    size_t len = end - begin;
    bool negative = false;

    // Handle sign (exactly like VM does)
    if (*begin == '-') {
      negative = true;
      ++begin;
      --len;
    } else if (*begin == '+') {
      ++begin;
      --len;
    }

    if (len == 0) [[unlikely]] return 0;

    // Skip leading zeros (VM optimization)
    while (len > 1 && *begin == '0') {
      ++begin;
      --len;
    }

    // VM uses a threshold to decide between small int and bigint
    // For base 10, this is typically around 18-19 digits for 64-bit
    if (len <= 18) {
      return try_decode_as_small_int_vm_style(env, begin, len, negative);
    }

    // For large numbers, use VM-style bigint creation
    return decode_as_bigint_vm_style(env, begin, len, negative);
  }

  // Try to decode as small integer using VM's approach
  static ERL_NIF_TERM try_decode_as_small_int_vm_style(ErlNifEnv* env,
                                                       const char* digits,
                                                       size_t len,
                                                       bool negative) {
    // Use VM's algorithm for small integer conversion
    ErlNifUInt64 result = 0;
    ErlNifUInt64 base = 10;

    for (size_t i = 0; i < len; ++i) {
      char c = digits[i];
      if (c < '0' || c > '9') [[unlikely]] return 0;

      ErlNifUInt64 digit = c - '0';

      // Check overflow using VM's technique
      if (result > (UINT64_MAX / base)) {
        // Overflow - need bigint
        return decode_as_bigint_vm_style(env, digits - (negative ? 1 : 0),
                                        len + (negative ? 1 : 0), false);
      }

      ErlNifUInt64 new_result = result * base;
      if (new_result > (UINT64_MAX - digit)) {
        // Overflow - need bigint
        return decode_as_bigint_vm_style(env, digits - (negative ? 1 : 0),
                                        len + (negative ? 1 : 0), false);
      }

      result = new_result + digit;
    }

    // Check if result fits in signed 64-bit
    if (result > (negative ? (1ULL << 63) : ((1ULL << 63) - 1))) {
      return decode_as_bigint_vm_style(env, digits - (negative ? 1 : 0),
                                      len + (negative ? 1 : 0), false);
    }

    return negative ? enif_make_int64(env, -static_cast<ErlNifSInt64>(result))
                   : enif_make_int64(env, static_cast<ErlNifSInt64>(result));
  }

  // Decode as bigint using VM-inspired algorithm
  static ERL_NIF_TERM decode_as_bigint_vm_style(ErlNifEnv* env,
                                                const char* digits,
                                                size_t len,
                                                bool negative) {
    // Use the VM's approach: process digits in chunks for efficiency

    // The VM processes decimal digits in chunks to reduce multiplications
    // We'll use 9-digit chunks (10^9 fits in 32-bit)
    constexpr uint32_t chunk_base = 1000000000u; // 10^9
    constexpr size_t chunk_size = 9;

    std::vector<uint32_t> result;

    size_t remaining = len;
    const char* ptr = digits;

    // Skip sign if present
    if (remaining > 0 && (*ptr == '+' || *ptr == '-')) {
      if (*ptr == '-') negative = true;
      ++ptr;
      --remaining;
    }

    // Process full 9-digit chunks from right to left (VM approach)
    while (remaining >= chunk_size) {
      // Extract 9 digits from the end
      size_t start_pos = remaining - chunk_size;
      uint32_t chunk = 0;

      for (size_t i = 0; i < chunk_size; ++i) {
        char c = ptr[start_pos + i];
        if (c < '0' || c > '9') [[unlikely]] return 0;
        chunk = chunk * 10 + (c - '0');
      }

      // Multiply existing result by 10^9 and add chunk (VM algorithm)
      multiply_and_add_vm_style(result, chunk_base, chunk);

      remaining -= chunk_size;
    }

    // Handle remaining digits
    if (remaining > 0) {
      uint32_t chunk = 0;
      uint32_t multiplier = 1;

      for (size_t i = 0; i < remaining; ++i) {
        char c = ptr[i];
        if (c < '0' || c > '9') [[unlikely]] return 0;
        chunk = chunk * 10 + (c - '0');
        multiplier *= 10;
      }

      multiply_and_add_vm_style(result, multiplier, chunk);
    }

    // Convert to Erlang bigint using VM-compatible format
    return create_erlang_bigint_vm_style(env, result, negative);
  }

  // VM-style multiply and add operation
  static void multiply_and_add_vm_style(std::vector<uint32_t>& number,
                                       uint32_t multiplier,
                                       uint32_t addend) {
    if (number.empty()) {
      if (addend != 0) number.push_back(addend);
      return;
    }

    // Multiply existing number by multiplier
    uint64_t carry = 0;
    for (auto& digit : number) {
      uint64_t product = static_cast<uint64_t>(digit) * multiplier + carry;
      digit = static_cast<uint32_t>(product & 0xFFFFFFFF);
      carry = product >> 32;
    }

    while (carry) {
      number.push_back(static_cast<uint32_t>(carry & 0xFFFFFFFF));
      carry >>= 32;
    }

    // Add the addend
    carry = addend;
    for (auto& digit : number) {
      uint64_t sum = static_cast<uint64_t>(digit) + carry;
      digit = static_cast<uint32_t>(sum & 0xFFFFFFFF);
      carry = sum >> 32;
      if (carry == 0) break;
    }

    while (carry) {
      number.push_back(static_cast<uint32_t>(carry & 0xFFFFFFFF));
      carry >>= 32;
    }
  }

  // Create Erlang bigint using VM-compatible binary format
  static ERL_NIF_TERM create_erlang_bigint_vm_style(ErlNifEnv* env,
                                                    const std::vector<uint32_t>& digits,
                                                    bool negative) {
    if (digits.empty()) return enif_make_int(env, 0);

    // Convert to VM's little-endian byte format
    std::vector<uint8_t> bytes;
    for (uint32_t word : digits) {
      bytes.push_back(word & 0xFF);
      bytes.push_back((word >> 8) & 0xFF);
      bytes.push_back((word >> 16) & 0xFF);
      bytes.push_back((word >> 24) & 0xFF);
    }

    // Remove trailing zeros (VM optimization)
    while (bytes.size() > 1 && bytes.back() == 0) {
      bytes.pop_back();
    }

    // Create binary term using VM's external format
    std::vector<uint8_t> term_data;
    term_data.push_back(131); // Version

    if (bytes.size() <= 255) {
      term_data.push_back('n'); // SMALL_BIG_EXT
      term_data.push_back(static_cast<uint8_t>(bytes.size()));
    } else {
      term_data.push_back('o'); // LARGE_BIG_EXT
      uint32_t size = bytes.size();
      term_data.push_back(static_cast<uint8_t>(size >> 24));
      term_data.push_back(static_cast<uint8_t>(size >> 16));
      term_data.push_back(static_cast<uint8_t>(size >> 8));
      term_data.push_back(static_cast<uint8_t>(size));
    }

    term_data.push_back(negative ? 1 : 0);
    term_data.insert(term_data.end(), bytes.begin(), bytes.end());

    ERL_NIF_TERM result;
    if (!enif_binary_to_term(env, term_data.data(), term_data.size(), &result, 0)) {
      return 0;
    }

    return result;
  }

  // VM-style integer to string conversion (optimized like VM's lltoa)
  static std::string int64_to_string_vm_style(ErlNifSInt64 value) {
    if (value == 0) return "0";

    bool negative = value < 0;
    if (negative) value = -value;

    // Use VM's approach: build string backwards
    char buffer[32];
    char* p = buffer + sizeof(buffer);
    *--p = '\0';

    // Convert digits (VM uses optimized division)
    do {
      *--p = '0' + (value % 10);
      value /= 10;
    } while (value);

    if (negative) *--p = '-';

    return std::string(p);
  }

  // VM-style bigint to string conversion
  static std::string bigint_to_string_vm_style(ErlNifEnv* env, ERL_NIF_TERM term) {
    // Extract binary representation
    ErlNifBinary bin;
    if (!enif_term_to_binary(env, term, &bin)) return {};

    // Parse using VM-style algorithm
    std::string result = parse_bigint_binary_vm_style(bin.data, bin.size);
    enif_release_binary(&bin);
    return result;
  }

  // Parse bigint binary using VM-inspired algorithm
  static std::string parse_bigint_binary_vm_style(const uint8_t* data, size_t size) {
    if (size < 2 || data[0] != 131) return {};

    size_t pos = 1;
    uint8_t tag = data[pos++];

    bool negative;
    std::vector<uint8_t> bytes;

    // Parse binary format (VM's external format)
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
      return {};
    }

    // Convert using VM-style division algorithm
    return convert_bytes_to_decimal_vm_style(bytes, negative);
  }

  // VM-style base256 to decimal conversion
  static std::string convert_bytes_to_decimal_vm_style(const std::vector<uint8_t>& bytes,
                                                      bool negative) {
    if (bytes.empty()) return "0";

    // Convert to 32-bit words (VM's internal format)
    std::vector<uint32_t> words;
    for (size_t i = 0; i < bytes.size(); i += 4) {
      uint32_t word = 0;
      for (size_t j = 0; j < 4 && i + j < bytes.size(); ++j) {
        word |= static_cast<uint32_t>(bytes[i + j]) << (j * 8);
      }
      words.push_back(word);
    }

    // VM uses repeated division by large powers of 10 for efficiency
    std::vector<char> digits;
    constexpr uint32_t billion = 1000000000u; // 10^9

    while (!is_zero_vm_style(words)) {
      uint32_t remainder = divide_by_vm_style(words, billion);

      // Convert remainder to 9 decimal digits
      for (int i = 0; i < 9; ++i) {
        digits.push_back('0' + (remainder % 10));
        remainder /= 10;
        if (remainder == 0 && is_zero_vm_style(words)) break;
      }
    }

    if (digits.empty()) return "0";

    // Build result string (VM builds backwards then reverses)
    std::string result;
    if (negative) result += '-';

    // Remove leading zeros
    while (!digits.empty() && digits.back() == '0') {
      digits.pop_back();
    }

    std::reverse(digits.begin(), digits.end());
    result.append(digits.begin(), digits.end());

    return result;
  }

  // VM-style zero check
  static bool is_zero_vm_style(const std::vector<uint32_t>& words) {
    for (uint32_t word : words) {
      if (word != 0) return false;
    }
    return true;
  }

  // VM-style division by constant
  static uint32_t divide_by_vm_style(std::vector<uint32_t>& words, uint32_t divisor) {
    uint64_t remainder = 0;

    // Divide from most significant to least significant (VM approach)
    for (auto it = words.rbegin(); it != words.rend(); ++it) {
      uint64_t dividend = (remainder << 32) | *it;
      *it = static_cast<uint32_t>(dividend / divisor);
      remainder = dividend % divisor;
    }

    // Remove leading zeros (VM optimization)
    while (!words.empty() && words.back() == 0) {
      words.pop_back();
    }

    return static_cast<uint32_t>(remainder);
  }
};

} // namespace simdjsone