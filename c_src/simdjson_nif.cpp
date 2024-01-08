// vim:ts=2:tw=2:et
//-----------------------------------------------------------------------------
// Erlang NIF binding to the simdjson C++ JSON parser
//-----------------------------------------------------------------------------
#include <cstring>
#include <vector>
#include <cassert>
#include <set>
#include <string>
#include <string.h>

#include "simdjson_atoms.hpp"
#include "simdjson_encoder.hpp"

using namespace simdjson;

static constexpr const size_t BYTES_PER_REDUCTION = 20;
static constexpr const size_t ERL_REDUCTION_COUNT = 2000;
static constexpr const size_t TIMESLICE_BYTES     = ERL_REDUCTION_COUNT * BYTES_PER_REDUCTION / 2;

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

static ERL_NIF_TERM make_term(ErlNifEnv* env, const dom::element& elm, const DecodeOpts& opts)
{
  if (!enif_is_current_process_alive(env)) [[unlikely]]
    throw DeadProcError();

  switch(elm.type()) {
    case dom::element_type::OBJECT: {
      auto   obj = dom::object(elm);
      size_t i   = 0;
      if (opts.return_maps) {
        std::vector<ERL_NIF_TERM> ks(obj.size());
        std::vector<ERL_NIF_TERM> vs(obj.size());

        if (opts.dedupe_keys) {
          std::set<std::string_view> seen;
          for(auto field : obj) {
            auto [_, inserted] = seen.emplace(field.key);
            if (inserted) [[likely]] {
              ks.at(i)   = make_binary(env, field.key);
              vs.at(i++) = make_term(env, field.value, opts);
            }
          }
          if (i != obj.size()) [[unlikely]] {
            ks.resize(i);
            vs.resize(i);
          }
        } else {
          for(auto field : obj) {
            ks.at(i)   = make_binary(env, field.key);
            vs.at(i++) = make_term(env, field.value, opts);
          }
        }

        ERL_NIF_TERM m;
        return enif_make_map_from_arrays(env, ks.data(), vs.data(), ks.size(), &m)
            ? m : enif_raise_exception(env, ATOM_DUP_KEYS_FOUND);
      } else {
        std::vector<ERL_NIF_TERM> items(obj.size());
        if (opts.dedupe_keys) {
          std::set<std::string_view> seen;
          for(auto field : obj) {
            auto [_, inserted] = seen.emplace(field.key);
            if (inserted) [[likely]]
              items.at(i++) = enif_make_tuple2(env, make_binary(env, field.key), make_term(env, field.value, opts));
          }
          if (i != obj.size()) [[unlikely]]
            items.resize(i);
        } else {
          for(auto field : obj)
            items.at(i++) = enif_make_tuple2(env, make_binary(env, field.key), make_term(env, field.value, opts));
        }
        return enif_make_tuple1(env, enif_make_list_from_array(env, items.data(), items.size()));
      }
    }
    case dom::element_type::ARRAY: {
      auto array = dom::array(elm);
      int  i     = 0;
      std::vector<ERL_NIF_TERM> cs(array.size());
      for(dom::element c : array)
        cs.at(i++) = make_term(env, c, opts);
      return enif_make_list_from_array(env, cs.data(), cs.size());
    }
    case dom::element_type::STRING: {
      ErlNifBinary bin;
      std::string_view str = elm;
      enif_alloc_binary(str.length(), &bin);
      memcpy(bin.data, str.data(), str.length());
      return enif_make_binary(env, &bin);
    }
    case dom::element_type::INT64:      return enif_make_long(env,  int64_t(elm));
    case dom::element_type::UINT64:     return enif_make_ulong(env, uint64_t(elm));
    case dom::element_type::DOUBLE:     return enif_make_double(env, elm);
    case dom::element_type::BOOL:       return elm.get<bool>() ? ATOM_TRUE : ATOM_FALSE;
    case dom::element_type::NULL_VALUE:
    default:                            return opts.null_term;
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

static ERL_NIF_TERM parse_opts(ErlNifEnv* env, ERL_NIF_TERM options, DecodeOpts& opts)
{
  ERL_NIF_TERM head = options, null_term = am_null;
  const ERL_NIF_TERM* array;
  int arity;

  while (enif_get_list_cell(env, options, &head, &options)) {
    if (enif_is_identical(head, ATOM_RETURN_MAPS))
      opts.return_maps = true;
    else if (enif_is_identical(head, ATOM_OBJECT_AS_TUPLE))
      opts.return_maps = false;
    else if (enif_is_identical(head, ATOM_USE_NIL))
      null_term = ATOM_NIL;
    else if (enif_is_identical(head, ATOM_DEDUPE_KEYS))
      opts.dedupe_keys = true;
    else if (!enif_get_tuple(env, head, &arity, &array) || arity != 2)
      return enif_raise_exception(env, enif_make_tuple2(env, ATOM_BADARG, head));
    else if (enif_is_identical(array[0], ATOM_NULL_TERM))
      null_term = array[1];
    else
      return enif_raise_exception(env, enif_make_tuple2(env, ATOM_BADARG, head));
  }

  opts.null_term = null_term;

  return ATOM_OK;
}

static ERL_NIF_TERM decode(ErlNifEnv* env, const ErlNifBinary& bin, const DecodeOpts& opts)
{
  try {
    dom::parser parser;
    dom::element elm = parser.parse(reinterpret_cast<const char*>(bin.data), bin.size);
    return make_term(env, elm, opts);
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

  DecodeOpts opts;
  auto   r =  argc > 1 ? parse_opts(env, argv[1], opts) : ATOM_OK;
  return r == ATOM_OK  ? decode(env, bin, opts)         : r;
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
  ERL_NIF_TERM args[2];
  static ERL_NIF_TERM s_nil_list = enif_make_list(env, 0);

  assert(argc >= 1 && argc <= 2);

  if (enif_inspect_binary(env, argv[0], &bin)) [[likely]] {
    if (bin.size < TIMESLICE_BYTES)
      goto CALL_DECODE;

    args[0] = argv[0];
    args[1] = argc > 1 ? argv[1] : s_nil_list;
  }
  else if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)) [[unlikely]]
    return enif_make_badarg(env);
  else if (bin.size < TIMESLICE_BYTES)
    goto CALL_DECODE;
  else {
    args[0] = enif_make_binary(env, &bin);
    args[1] = argc > 1 ? argv[1] : s_nil_list;
  }

  return enif_schedule_nif(env, "simdjson_decode",
                           ERL_NIF_DIRTY_JOB_CPU_BOUND, decode_dirty, argc, args);
CALL_DECODE:
  DecodeOpts opts;
  auto res = parse_opts(env, argv[1], opts);
  return res == ATOM_OK ? decode(env, bin, opts) : res;
}

static ERL_NIF_TERM parse_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[0], &bin) &&
      !enif_inspect_iolist_as_binary(env, argv[0], &bin)) [[unlikely]]
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
  assert(argc == 2 || argc == 3);

  if (!enif_get_resource(env, argv[0], JSON_RESOURCE, (void**)&doc)) [[unlikely]]
    return enif_make_badarg(env);

  //fprintf(stderr, "--> AT %p\r\n", root);

  ErlNifBinary res;
  if(!enif_inspect_binary(env, argv[1], &res) &&
     !enif_inspect_iolist_as_binary(env, argv[1], &res)) [[unlikely]]
    return enif_make_badarg(env);

  DecodeOpts opts;
  if (argc == 3) {
    auto res  = parse_opts(env, argv[2], opts);
    if  (res != ATOM_OK) [[unlikely]]
      return res;
  }

  auto path = std::string_view((char *)res.data, res.size);
  auto elm  = doc->root().at_pointer(path);

  if  (elm.error()) [[unlikely]]
    return enif_make_string(env, error_message(elm.error()), ERL_NIF_LATIN1);

  try {
    return make_term(env, elm.value_unsafe(), opts);
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
  assert(argc == 1);

  ErlNifBinary bin;
  ERL_NIF_TERM args[1];

  if (enif_inspect_binary(env, argv[0], &bin)) [[likely]] {
    if (bin.size < TIMESLICE_BYTES)
      return minify(env, bin);

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

static ERL_NIF_TERM int_to_bin_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 1);
  int64_t n;

  if (!enif_get_long(env, argv[0], &n))
    return enif_make_badarg(env);

  char buf[64];
  auto end = util::lltoa(buf, n);

  return make_binary(env, std::string_view(buf, end - buf));
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

  auto flags             = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  ErlNifResourceTypeInit rti{0};
  rti.dtor               = &resource_dtor;
  rti.down               = &resource_down;
  JSON_RESOURCE          = enif_open_resource_type_x(env, "simjson_resource",
                                                     &rti, flags, nullptr);
  ATOM_OK                = enif_make_atom(env, "ok");
  ATOM_ERROR             = enif_make_atom(env, "error");
  ATOM_TRUE              = enif_make_atom(env, "true");
  ATOM_FALSE             = enif_make_atom(env, "false");
  ATOM_BADARG            = enif_make_atom(env, "badarg");
  ATOM_NIL               = enif_make_atom(env, "nil");
  ATOM_NULL              = enif_make_atom(env, "null");
  am_null                = ATOM_NULL;
  ATOM_ENOMEM            = enif_make_atom(env, "enomem");
  ATOM_ENOPROCESS        = enif_make_atom(env, "enoprocess");
  ATOM_ENOCALLBACK       = enif_make_atom(env, "enocallback");
  ATOM_DUP_KEYS_FOUND    = enif_make_atom(env, "dup_keys_found");

  ATOM_RETURN_MAPS       = enif_make_atom(env, "return_maps");
  ATOM_OBJECT_AS_TUPLE   = enif_make_atom(env, "object_as_tuple");
  ATOM_USE_NIL           = enif_make_atom(env, "use_nil");
  ATOM_NULL_TERM         = enif_make_atom(env, "null_term");
  ATOM_DEDUPE_KEYS       = enif_make_atom(env, "dedupe_keys");

  ATOM_UESCAPE           = enif_make_atom(env, "uescape");
  ATOM_PRETTY            = enif_make_atom(env, "pretty");
  ATOM_ESCAPE_FWD_SLASH  = enif_make_atom(env, "escape_fwd_slash");
  ATOM_FORCE_UTF8        = enif_make_atom(env, "force_utf8");
  ATOM_BYTES_PER_RED     = enif_make_atom(env, "bytes_per_red");
  ATOM_ITER              = enif_make_atom(env, "iter");
  ATOM_PARTIAL           = enif_make_atom(env, "partial");

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

  jiffy_st* st = reinterpret_cast<jiffy_st*>(enif_alloc(sizeof(jiffy_st)));
  if(st == NULL) [[unlikely]] {
    fprintf(stderr, "Failed to allocate %lu bytes\r\n", sizeof(jiffy_st));
    return 1;
  }

  // Markers used in encoding
  st->ref_object = make_atom(env, "$object_ref$");
  st->ref_array  = make_atom(env, "$array_ref$");

  st->res_enc = enif_open_resource_type(
    env,
    NULL,
    "encoder",
    enc_destroy,
    ErlNifResourceFlags(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER),
    NULL
  );

  *priv_data = static_cast<void*>(st);

  return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
  if (old_priv_data)
    enif_release_resource(old_priv_data);
  return 0;
}

static ErlNifFunc funcs[] = {
  {"decode",      1, decode_nif},
  {"decode",      2, decode_nif},
  {"parse",       1, parse_nif},
  {"get",         2, get_nif},
  {"get",         3, get_nif},
  {"minify",      1, minify_nif},
  {"encode_init", 2, encode_init},
  {"encode_iter", 3, encode_iter},
  {"int_to_bin",  1, int_to_bin_nif},
};

ERL_NIF_INIT(simdjson, funcs, load, nullptr, upgrade, nullptr);
