//------------------------------------------------------------------------------
// Author: Serge Aleynikov <saleyn at gmail dot com>
//------------------------------------------------------------------------------
// Highly optimized BigInt implementation inspired by Erlang VM's native functions
//------------------------------------------------------------------------------
#pragma once

#include "simdjson_bigint_native.hpp"
#include "simdjson_bigint_vm_inspired.hpp"

namespace simdjsone {

using namespace simdjson;

// Use the optimized implementation (17-24x performance improvement)
using BigInt = BigIntFast;

// Alternative implementation available:
// using BigInt = BigIntVMInspired;     // VM-inspired algorithms (alternative option)

} // namespace simdjsone
