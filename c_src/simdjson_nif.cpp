// vim:ts=2:tw=2:et
//-----------------------------------------------------------------------------
// Erlang NIF binding to the simdjson C++ JSON parser
//-----------------------------------------------------------------------------
#include <cstring>
#include <vector>
#include <cassert>
#include <string.h>
#include <erl_nif.h>

#include "simdjson.h"
using namespace simdjson;

static constexpr const size_t BYTES_PER_REDUCTION = 20;
static constexpr const size_t ERL_REDUCTION_COUNT = 2000;
static constexpr const size_t TIMESLICE_BYTES     = ERL_REDUCTION_COUNT * BYTES_PER_REDUCTION / 2;

static ErlNifResourceType* JSON_RESOURCE;

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_NULL;
static ERL_NIF_TERM ATOM_NIL;
static ERL_NIF_TERM ATOM_ENOMEM;
static ERL_NIF_TERM ATOM_ENOPROCESS;
static ERL_NIF_TERM ATOM_ENOCALLBACK;
static ERL_NIF_TERM ATOM_DUP_KEYS_FOUND;

static ERL_NIF_TERM am_null;

struct DeadProcError : public std::exception {};

static std::tuple<ERL_NIF_TERM, unsigned char*>
make_binary(ErlNifEnv* env, size_t size)
{
  ERL_NIF_TERM term;
  auto   p = enif_make_new_binary(env, size, &term);
  return std::make_tuple(term, p);
}

static ERL_NIF_TERM make_binary(ErlNifEnv* env, std::string_view const& str)
{
  auto [term, p] = make_binary(env, str.length());
  memcpy(p, str.data(), str.length());
  return term;
}

static ERL_NIF_TERM make_term(ErlNifEnv* env, const dom::element& elm)
{
  if (!enif_is_current_process_alive(env)) [[unlikely]]
    throw DeadProcError();

  switch(elm.type()) {
    case dom::element_type::OBJECT: {
      auto obj = dom::object(elm);
      int  i   = 0;
      std::vector<ERL_NIF_TERM> ks(obj.size());
      std::vector<ERL_NIF_TERM> vs(obj.size());
      for(auto field : obj) {
        ks.at(i)   = make_binary(env, field.key);
        vs.at(i++) = make_term(env, field.value);
      }
      ERL_NIF_TERM m;
      return enif_make_map_from_arrays(env, ks.data(), vs.data(), ks.size(), &m)
           ? m : enif_raise_exception(env, ATOM_DUP_KEYS_FOUND);
    }
    case dom::element_type::ARRAY: {
      auto array = dom::array(elm);
      int  i     = 0;
      std::vector<ERL_NIF_TERM> cs(array.size());
      for(dom::element c : array)
        cs.at(i++) = make_term(env, c);
      return enif_make_list_from_array(env, cs.data(), cs.size());
    }
    case dom::element_type::STRING: {
      ErlNifBinary bin;
      std::string_view str = elm;
      enif_alloc_binary(str.length(), &bin);
      memcpy(bin.data, str.data(), str.length());
      return enif_make_binary(env, &bin);
    }
    case dom::element_type::INT64:      return enif_make_long(env, int64_t(elm));
    case dom::element_type::UINT64:     return enif_make_ulong(env, uint64_t(elm));
    case dom::element_type::DOUBLE:     return enif_make_double(env, elm);
    case dom::element_type::BOOL:       return elm.get<bool>() ? ATOM_TRUE : ATOM_FALSE;
    case dom::element_type::NULL_VALUE:
    default:                            return am_null;
  }
}

static ERL_NIF_TERM error_reason(ErlNifEnv* env, error_code err)
{
  switch (err) {
    case CAPACITY:                   return make_binary(env, "This parser can't support a document that big");
    case MEMALLOC:                   return make_binary(env, "Error allocating memory, most likely out of memory");
    case TAPE_ERROR:                 return make_binary(env, "Something went wrong, this is a generic error");
    case DEPTH_ERROR:                return make_binary(env, "Your document exceeds the user-specified depth limitation");
    case STRING_ERROR:               return make_binary(env, "Problem while parsing a string");
    case T_ATOM_ERROR:               return make_binary(env, "Problem while parsing an atom starting with 't'");
    case F_ATOM_ERROR:               return make_binary(env, "Problem while parsing an atom starting with 'f'");
    case N_ATOM_ERROR:               return make_binary(env, "Problem while parsing an atom starting with 'n'");
    case NUMBER_ERROR:               return make_binary(env, "Problem while parsing a number");
    case UTF8_ERROR:                 return make_binary(env, "The input is not valid UTF-8");
    case UNINITIALIZED:              return make_binary(env, "Uninitialized document");
    case EMPTY:                      return make_binary(env, "No structural element found");
    case UNESCAPED_CHARS:            return make_binary(env, "Found unescaped characters in a string");
    case UNCLOSED_STRING:            return make_binary(env, "Missing quote at the end");
    case UNSUPPORTED_ARCHITECTURE:   return make_binary(env, "Unsupported architecture");
    case INCORRECT_TYPE:             return make_binary(env, "Element has a different type than user expected");
    case NUMBER_OUT_OF_RANGE:        return make_binary(env, "Number does not fit in 64 bits");
    case INDEX_OUT_OF_BOUNDS:        return make_binary(env, "Array index too large");
    case NO_SUCH_FIELD:              return make_binary(env, "Field not found in object");
    case IO_ERROR:                   return make_binary(env, "Error reading a file");
    case INVALID_JSON_POINTER:       return make_binary(env, "Invalid JSON pointer reference");
    case INVALID_URI_FRAGMENT:       return make_binary(env, "Invalid URI fragment");
    case UNEXPECTED_ERROR:           return make_binary(env, "Indicative of a bug in simdjson");
    case PARSER_IN_USE:              return make_binary(env, "Parser is already in use");
    case OUT_OF_ORDER_ITERATION:     return make_binary(env, "Tried to iterate an array or object out of order");
    case INSUFFICIENT_PADDING:       return make_binary(env, "Not enough padding for simdjson to safely parse it");
    case INCOMPLETE_ARRAY_OR_OBJECT: return make_binary(env, "The document ends early");
    case SCALAR_DOCUMENT_AS_VALUE:   return make_binary(env, "A scalar document is treated as a value");
    case OUT_OF_BOUNDS:              return make_binary(env, "Attempted to access location outside of document");
    case TRAILING_CONTENT:           return make_binary(env, "Unexpected trailing content");
    default:                         return make_binary(env, "Unknown error code " + std::to_string(int(err)));
  }
}

static ERL_NIF_TERM decode(ErlNifEnv* env, const ErlNifBinary& bin)
{
  try {
    dom::parser parser;
    dom::element elm = parser.parse(reinterpret_cast<const char*>(bin.data), bin.size);
    return make_term(env, elm);
  } catch (simdjson_error const& error) {
    auto msg = enif_make_string(env, error.what(), ERL_NIF_LATIN1);
    return enif_raise_exception(env, msg);
  } catch (DeadProcError const&) {
    return enif_raise_exception(env, ATOM_ENOPROCESS);
  }
}

static ERL_NIF_TERM decode_dirty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;

  [[maybe_unused]] auto res = enif_inspect_binary(env, argv[0], &bin);

  assert(res);

  return decode(env, bin);
}

/*
static uint64_t get_time() {
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return static_cast<uint64_t>(t.tv_sec)*1000000000l + start_time.tv_nsec;
}

static bool consume_timeslice(ErlNifEnv* env, uint64_t start_time = 0) {
  auto now = get_time();

  // Figure out how much time elapsed
  auto elapsed = t - start_time;

  // Convert that to a percentage of a timeslice
  int slice_percent = (elapsed * 100) / TIMESLICE_NANOSECONDS;
  if (slice_percent < 0)
    slice_percent = 0;
  else if (slice_percent > 100)
    slice_percent = 100;

  // If the result is 1, then we have consumed the entire slice and should
  // yield.
  return enif_consume_timeslice(env, slice_percent) == 1;
}
*/

static ERL_NIF_TERM decode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM args[1];

  if (argc != 1) [[unlikely]]
    return enif_make_badarg(env);

  if (enif_inspect_binary(env, argv[0], &bin)) [[likely]] {
    if (bin.size < TIMESLICE_BYTES)
      return decode(env, bin);

    args[0] = argv[0];
  }
  else if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)) [[unlikely]]
    return enif_make_badarg(env);
  else if (bin.size < TIMESLICE_BYTES)
    return decode(env, bin);
  else
    args[0] = enif_make_binary(env, &bin);

  return enif_schedule_nif(env, "simdjson_decode",
                           ERL_NIF_DIRTY_JOB_CPU_BOUND, decode_dirty, argc, args);
}

static ERL_NIF_TERM parse_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[0], &bin) &&
      !enif_inspect_iolist_as_binary(env, argv[0], &bin))
    return enif_make_badarg(env);

  ErlNifPid pid;
  enif_self(env, &pid);

  auto p = static_cast<dom::document*>(
    enif_alloc_resource(JSON_RESOURCE, sizeof(dom::document)));
  if (!p)
    return enif_raise_exception(env, ATOM_ENOMEM);
  //fprintf(stderr, "--> Allocated resource %p by pid %p\r\n", p, &pid);
  new (p) dom::document();

  dom::parser parser;
  auto res  = parser.parse_into_document(*p, (const char*)bin.data, bin.size);

  if (res.error()) [[unlikely]]
    return enif_make_string(env, error_message(res.error()), ERL_NIF_LATIN1);

  ErlNifMonitor mon;

  auto result = enif_monitor_process(env, p, &pid, &mon);

  if (result != 0) [[unlikely]] {
    if (result > 0) {
      // Process no longer alive
      enif_release_resource(p);
      return enif_raise_exception(env, ATOM_ENOPROCESS);
    } else {
      assert(result < 0);
      // mon callback is not specified
      enif_release_resource(p);
      return enif_raise_exception(env, ATOM_ENOCALLBACK);
    }
  }

  assert(result == 0);
  ERL_NIF_TERM resource = enif_make_resource(env, p);
  enif_release_resource(p);

  return resource;
}

static ERL_NIF_TERM get_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  dom::document* doc;
  if (argc != 2 || !enif_get_resource(env, argv[0], JSON_RESOURCE, (void**)&doc)) [[unlikely]]
    return enif_make_badarg(env);

  //fprintf(stderr, "--> AT %p\r\n", root);

  ErlNifBinary res;
  if(!enif_inspect_binary(env, argv[1], &res) &&
     !enif_inspect_iolist_as_binary(env, argv[1], &res))
    return enif_make_badarg(env);

  auto path = std::string_view((char *)res.data, res.size);
  auto elm  = doc->root().at_pointer(path);

  if  (elm.error()) [[unlikely]]
    return enif_make_string(env, error_message(elm.error()), ERL_NIF_LATIN1);

  try {
    return make_term(env, elm.value_unsafe());
  } catch (DeadProcError const&) {
    return enif_raise_exception(env, ATOM_ENOPROCESS);
  }
}

static ERL_NIF_TERM minify(ErlNifEnv* env, const ErlNifBinary& bin)
{
  std::unique_ptr<char[]> buffer{new char[bin.size]};
  size_t size{};
  auto error = simdjson::minify((const char*)bin.data, bin.size, buffer.get(), size);
  if (error != simdjson::SUCCESS) [[unlikely]]
    return enif_make_tuple2(env, ATOM_ERROR, error_reason(env, error));

  return enif_make_tuple2(env, ATOM_OK, make_binary(env, std::string_view(buffer.get(), size)));
}

static ERL_NIF_TERM minify_dirty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;

  [[maybe_unused]] auto res = enif_inspect_binary(env, argv[0], &bin);

  assert(res);

  return minify(env, bin);
}

static ERL_NIF_TERM minify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM args[1];

  if (argc != 1) [[unlikely]]
    return enif_make_badarg(env);

  if (enif_inspect_binary(env, argv[0], &bin)) [[likely]] {
    if (bin.size < TIMESLICE_BYTES)
      return decode(env, bin);

    args[0] = argv[0];
  }
  else if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)) [[unlikely]]
    return enif_make_badarg(env);
  else if (bin.size < TIMESLICE_BYTES)
    return minify(env, bin);
  else
    args[0] = enif_make_binary(env, &bin);

  return enif_schedule_nif(env, "simdjson_minify",
                           ERL_NIF_DIRTY_JOB_CPU_BOUND, minify_dirty, argc, args);
}

static void resource_dtor(ErlNifEnv* env, void* arg)
{
  assert(arg);
  //fprintf(stderr, "--> Releasing resource %p\r\n", arg);
  static_cast<dom::document*>(arg)->dom::document::~document();
}

static void resource_down(ErlNifEnv* env, void* obj, ErlNifPid*, ErlNifMonitor*)
{
  //fprintf(stderr, "--> Decrement resource ref %p\r\n", obj);
  enif_release_resource(obj);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  if (!enif_is_list(env, load_info)) {
    fprintf(stderr, "Arguments passed to the NIF loader must be list!\r\n");
    return -1;
  }

  auto flags          = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  ErlNifResourceTypeInit rti{0};
  rti.dtor            = &resource_dtor;
  rti.down            = &resource_down;
  JSON_RESOURCE       = enif_open_resource_type_x(env, "simjson_resource",
                                                  &rti, flags, nullptr);
  ATOM_OK             = enif_make_atom(env, "ok");
  ATOM_ERROR          = enif_make_atom(env, "error");
  ATOM_TRUE           = enif_make_atom(env, "true");
  ATOM_FALSE          = enif_make_atom(env, "false");
  ATOM_NIL            = enif_make_atom(env, "nil");
  ATOM_NULL           = enif_make_atom(env, "null");
  am_null             = ATOM_NULL;
  ATOM_ENOMEM         = enif_make_atom(env, "enomem");
  ATOM_ENOPROCESS     = enif_make_atom(env, "enoprocess");
  ATOM_ENOCALLBACK    = enif_make_atom(env, "enocallback");
  ATOM_DUP_KEYS_FOUND = enif_make_atom(env, "dup_keys_found");

  int   arity;
  const ERL_NIF_TERM* tagval;
  ERL_NIF_TERM head, list = load_info;

  while (enif_get_list_cell(env, list, &head, &list)) {
    if (!enif_get_tuple(env, head, &arity, &tagval) || arity != 2) [[unlikely]]
      return enif_make_badarg(env);

    if (!enif_is_identical(tagval[0], ATOM_NULL) || !enif_is_atom(env, tagval[1]))
      return enif_make_badarg(env);
    else
      am_null = tagval[1];
  }

  return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
  if (old_priv_data)
    enif_release_resource(old_priv_data);
  return 0;
}

static ErlNifFunc funcs[] = {
  {"decode", 1, decode_nif},
  {"parse",  1, parse_nif},
  {"get",    2, get_nif},
  {"minify", 1, minify_nif},
};

ERL_NIF_INIT(simdjson, funcs, load, nullptr, upgrade, nullptr);
