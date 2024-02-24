// vim:ts=2:tw=2:et

#pragma once

#include <erl_nif.h>
#include "simdjson.h"

static ErlNifResourceType* JSON_RESOURCE;

static ERL_NIF_TERM AM_OK;
static ERL_NIF_TERM AM_ERROR;
static ERL_NIF_TERM AM_TRUE;
static ERL_NIF_TERM AM_FALSE;
static ERL_NIF_TERM AM_BADARG;
static ERL_NIF_TERM AM_NULL;
static ERL_NIF_TERM AM_NIL;
static ERL_NIF_TERM AM_ENOMEM;
static ERL_NIF_TERM AM_ENOPROCESS;
static ERL_NIF_TERM AM_ENOCALLBACK;
static ERL_NIF_TERM AM_OTHER;

static ERL_NIF_TERM AM_DUP_KEYS_FOUND;
static ERL_NIF_TERM AM_RETURN_MAPS;
static ERL_NIF_TERM AM_OBJECT_AS_TUPLE;
static ERL_NIF_TERM AM_USE_NIL;
static ERL_NIF_TERM AM_NULL_TERM;
static ERL_NIF_TERM AM_DEDUPE_KEYS;

static ERL_NIF_TERM AM_UESCAPE;
static ERL_NIF_TERM AM_PRETTY;
static ERL_NIF_TERM AM_ESCAPE_FWD_SLASH;
static ERL_NIF_TERM AM_FORCE_UTF8;
static ERL_NIF_TERM AM_BYTES_PER_RED;
static ERL_NIF_TERM AM_ITER;
static ERL_NIF_TERM AM_PARTIAL;

static ERL_NIF_TERM AM_CAPACITY;
static ERL_NIF_TERM AM_MEMALLOC;
static ERL_NIF_TERM AM_TAPE_ERROR;
static ERL_NIF_TERM AM_DEPTH_ERROR;
static ERL_NIF_TERM AM_STRING_ERROR;
static ERL_NIF_TERM AM_T_ATOM_ERROR;
static ERL_NIF_TERM AM_F_ATOM_ERROR;
static ERL_NIF_TERM AM_N_ATOM_ERROR;
static ERL_NIF_TERM AM_NUMBER_ERROR;
static ERL_NIF_TERM AM_UTF8_ERROR;
static ERL_NIF_TERM AM_UNINITIALIZED;
static ERL_NIF_TERM AM_EMPTY;
static ERL_NIF_TERM AM_UNESCAPED_CHARS;
static ERL_NIF_TERM AM_UNCLOSED_STRING;
static ERL_NIF_TERM AM_UNSUPPORTED_ARCHITECTURE;
static ERL_NIF_TERM AM_INCORRECT_TYPE;
static ERL_NIF_TERM AM_NUMBER_OUT_OF_RANGE;
static ERL_NIF_TERM AM_INDEX_OUT_OF_BOUNDS;
static ERL_NIF_TERM AM_NO_SUCH_FIELD;
static ERL_NIF_TERM AM_IO_ERROR;
static ERL_NIF_TERM AM_INVALID_JSON_POINTER;
static ERL_NIF_TERM AM_INVALID_URI_FRAGMENT;
static ERL_NIF_TERM AM_UNEXPECTED_ERROR;
static ERL_NIF_TERM AM_PARSER_IN_USE;
static ERL_NIF_TERM AM_OUT_OF_ORDER_ITERATION;
static ERL_NIF_TERM AM_INSUFFICIENT_PADDING;
static ERL_NIF_TERM AM_INCOMPLETE_ARRAY_OR_OBJECT;
static ERL_NIF_TERM AM_SCALAR_DOCUMENT_AS_VALUE;
static ERL_NIF_TERM AM_OUT_OF_BOUNDS;
static ERL_NIF_TERM AM_TRAILING_CONTENT;
static ERL_NIF_TERM AM_UNDEFINED;

static ERL_NIF_TERM am_null;

struct DeadProcError : public std::exception {};

inline std::tuple<ERL_NIF_TERM, unsigned char*>
make_binary(ErlNifEnv* env, size_t size)
{
  ERL_NIF_TERM term;
  auto   p = enif_make_new_binary(env, size, &term);
  return std::make_tuple(term, p);
}

inline ERL_NIF_TERM make_binary(ErlNifEnv* env, std::string_view const& str)
{
  auto [term, p] = make_binary(env, str.length());
  memcpy(p, str.data(), str.length());
  return term;
}

inline ERL_NIF_TERM make_binary(ErlNifEnv* env, const char* str)
{
  size_t n = strlen(str);
  auto [term, p] = make_binary(env, n);
  memcpy(p, str, n);
  return term;
}

ERL_NIF_TERM error_reason(ErlNifEnv* env, simdjson::error_code err);

inline ERL_NIF_TERM raise_error(ErlNifEnv* env, ERL_NIF_TERM reason, const char* err)
{
  return enif_raise_exception(env, enif_make_tuple2(env, reason, make_binary(env, err)));
}

inline ERL_NIF_TERM raise_error(ErlNifEnv* env, simdjson::error_code err)
{
  return enif_raise_exception(env, error_reason(env, err))
}

