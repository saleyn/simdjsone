//------------------------------------------------------------------------------
// Author: Serge Aleynikov <saleyn at gmail dot com>
//------------------------------------------------------------------------------
// Highly optimized BigInt implementation inspired by Erlang VM's native functions
//------------------------------------------------------------------------------
#pragma once

#include "simdjson_bigint_native.hpp"

namespace simdjsone {

using namespace simdjson;

// Use the fast implementation as the default BigInt
using BigInt = BigIntFast;

} // namespace simdjsone
