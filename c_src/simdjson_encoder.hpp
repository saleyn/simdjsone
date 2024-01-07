// vim:ts=2:tw=2:et
#pragma once

// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <erl_nif.h>

#include "simdjson_atoms.hpp"

const constexpr int BIN_INC_SIZE = 2048;

const constexpr int DEFAULT_BYTES_PER_REDUCTION    = 20;
const constexpr int DEFAULT_ERLANG_REDUCTION_COUNT = 2000;

#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))

#define MAYBE_PRETTY(e)             \
do {                                \
    if(e->pretty) {                 \
        if(!enc_shift(e))           \
            return 0;               \
    }                               \
} while(0)

#define MAP_TYPE_PRESENT            \
    ((ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION >= 6) \
    || (ERL_NIF_MAJOR_VERSION > 2))
#define CONSUME_TIMESLICE_PRESENT   \
    ((ERL_NIF_MAJOR_VERSION >= 2 && ERL_NIF_MINOR_VERSION >= 4))
#define SCHEDULE_NIF_PRESENT        \
    ((ERL_NIF_MAJOR_VERSION >= 2 && ERL_NIF_MINOR_VERSION >= 7))

#if WINDOWS || WIN32
#define inline __inline
#define snprintf  _snprintf
#endif

struct jiffy_st {
    ERL_NIF_TERM        ref_object;
    ERL_NIF_TERM        ref_array;
    ErlNifResourceType* res_enc;
};

//----- TermStack -----

#define SMALL_TERMSTACK_SIZE 16

typedef struct {
    ERL_NIF_TERM* elements;
    size_t size;
    size_t top;

    ERL_NIF_TERM __default_elements[SMALL_TERMSTACK_SIZE];
} TermStack;


ERL_NIF_TERM
termstack_save(ErlNifEnv* env, TermStack* stack)
{
    return enif_make_tuple_from_array(env, stack->elements, stack->top);
}

int
termstack_restore(ErlNifEnv* env, ERL_NIF_TERM from, TermStack* stack)
{
    const ERL_NIF_TERM* elements;
    int arity;

    if(enif_get_tuple(env, from, &arity, &elements)) {
        stack->top = arity;

        if(arity <= SMALL_TERMSTACK_SIZE) {
            stack->elements = &stack->__default_elements[0];
            stack->size = SMALL_TERMSTACK_SIZE;
        } else {
            stack->size = arity * 2;
            stack->elements = static_cast<ERL_NIF_TERM*>(enif_alloc(stack->size * sizeof(ERL_NIF_TERM)));

            if(!stack->elements) {
                return 0;
            }
        }

        memcpy(stack->elements, elements, arity * sizeof(ERL_NIF_TERM));
        return 1;
    }

    return 0;
}

void
termstack_destroy(TermStack* stack)
{
    if(stack->elements != &stack->__default_elements[0]) {
        enif_free(stack->elements);
    }
}

inline void
termstack_push(TermStack* stack, ERL_NIF_TERM term)
{

    if(stack->top == stack->size) {
        size_t new_size = stack->size * 2;
        size_t num_bytes = new_size * sizeof(ERL_NIF_TERM);

        if (stack->elements == &stack->__default_elements[0]) {
            ERL_NIF_TERM* elems = static_cast<ERL_NIF_TERM*>(enif_alloc(num_bytes));
            memcpy(elems, stack->elements, num_bytes);
            stack->elements = elems;
        } else {
            stack->elements = static_cast<ERL_NIF_TERM*>(enif_realloc(stack->elements, num_bytes));
        }

        stack->size = new_size;
    }

    assert(stack->top < stack->size);
    stack->elements[stack->top++] = term;
}

inline ERL_NIF_TERM
termstack_pop(TermStack* stack)
{
    assert(stack->top > 0 && stack->top <= stack->size);
    return stack->elements[--stack->top];
}

inline int
termstack_is_empty(TermStack* stack)
{
    return stack->top == 0;
}

//----- Util -----

int should_yield(size_t used, size_t bytes_per_red)
{
    return (used / bytes_per_red) >= DEFAULT_ERLANG_REDUCTION_COUNT;
}

void bump_used_reds
(
  [[maybe_unused]] ErlNifEnv* env,
  [[maybe_unused]] size_t     used,
  [[maybe_unused]] size_t     bytes_per_red
)
{
#if CONSUME_TIMESLICE_PRESENT
    size_t reds_used;
    size_t pct_used;

    reds_used = used / bytes_per_red;
    pct_used = 100 * reds_used / DEFAULT_ERLANG_REDUCTION_COUNT;

    if(pct_used > 0) {
        if(pct_used > 100) {
            pct_used = 100;
        }

        enif_consume_timeslice(env, pct_used);
    }
#endif
}

ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

ERL_NIF_TERM
make_ok(jiffy_st* st, ErlNifEnv* env, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, ATOM_OK, value);
}

ERL_NIF_TERM
make_error(jiffy_st* st, ErlNifEnv* env, const char* error)
{
    return enif_make_tuple2(env, ATOM_ERROR, make_atom(env, error));
}

ERL_NIF_TERM
raise_error(jiffy_st* st, ErlNifEnv* env, const char* error)
{
    return enif_raise_exception(env, make_error(st, env, error));
}

ERL_NIF_TERM
make_obj_error(jiffy_st* st, ErlNifEnv* env,
        const char* error, ERL_NIF_TERM obj)
{
    ERL_NIF_TERM reason = enif_make_tuple2(env, make_atom(env, error), obj);
    return enif_make_tuple2(env, ATOM_ERROR, reason);
}

static const unsigned char hexvals[256] = {
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
      0,   1,   2,   3,   4,   5,   6,   7,
      8,   9, 255, 255, 255, 255, 255, 255,
    255,  10,  11,  12,  13,  14,  15, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255,  10,  11,  12,  13,  14,  15, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,

    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255
};

static const char hexdigits[16] = {
    '0', '1', '2', '3',
    '4', '5', '6', '7',
    '8', '9', 'A', 'B',
    'C', 'D', 'E', 'F'
};

int
int_from_hex(const unsigned char* p)
{
    unsigned char* h = (unsigned char*) p;
    int ret;

    if(hexvals[*(h+0)] == 255) return -1;
    if(hexvals[*(h+1)] == 255) return -1;
    if(hexvals[*(h+2)] == 255) return -1;
    if(hexvals[*(h+3)] == 255) return -1;

    ret = (hexvals[*(h+0)] << 12)
        + (hexvals[*(h+1)] << 8)
        + (hexvals[*(h+2)] << 4)
        + (hexvals[*(h+3)] << 0);

    return ret;
}

int
int_to_hex(int val, unsigned char* p)
{
    if(val < 0 || val > 65535)
        return -1;

    p[0] = hexdigits[(val >> 12) & 0xF];
    p[1] = hexdigits[(val >> 8) & 0xF];
    p[2] = hexdigits[(val >> 4) & 0xF];
    p[3] = hexdigits[val & 0xF];

    return 1;
}

int
utf8_len(int c)
{
    if(c < 128) {
        return 1;
    } else if(c < 0x800) {
        return 2;
    } else if(c < 0x10000) {
        if(c < 0xD800 || (c > 0xDFFF)) {
            return 3;
        } else {
            return -1;
        }
    } else if(c <= 0x10FFFF) {
        return 4;
    } else {
        return -1;
    }
}

int
utf8_esc_len(int c)
{
    if(c < 0x10000) {
        return 6;
    } else if(c <= 0x10FFFF) {
        return 12;
    } else {
        return -1;
    }
}

int
utf8_to_unicode(unsigned char* buf, size_t size)
{
    int ret;
    if((buf[0] & 0x80) == 0x00) {
        // 0xxxxxxx
        ret = buf[0];
    } else if((buf[0] & 0xE0) == 0xC0 && size >= 2) {
        // 110xxxxy 10yyyyyy
        ret = ((buf[0] & 0x1F) << 6)
            | ((buf[1] & 0x3F));
    } else if((buf[0] & 0xF0) == 0xE0 && size >= 3) {
        // 1110xxxx 10xyyyyy 10yyyyyy
        ret = ((buf[0] & 0x0F) << 12)
            | ((buf[1] & 0x3F) << 6)
            | ((buf[2] & 0x3F));
        if(ret >= 0xD800 && ret <= 0xDFFF) {
            ret = -1;
        }
    } else if((buf[0] & 0xF8) == 0xF0 && size >= 4) {
        // 11110xxx 10xxyyyy 10yyyyyy 10yyyyyy
        ret = ((buf[0] & 0x07) << 18)
            | ((buf[1] & 0x3F) << 12)
            | ((buf[2] & 0x3F) << 6)
            | ((buf[3] & 0x3F));
    } else {
        ret = -1;
    }
    return ret;
}

int
unicode_to_utf8(int c, unsigned char* buf)
{
    if(c < 0x80) {
        buf[0] = c;
        return 1;
    } else if(c < 0x800) {
        buf[0] = 0xC0 + (c >> 6);
        buf[1] = 0x80 + (c & 0x3F);
        return 2;
    } else if(c < 0x10000) {
        if(c < 0xD800 || (c > 0xDFFF)) {
            buf[0] = 0xE0 + (c >> 12);
            buf[1] = 0x80 + ((c >> 6) & 0x3F);
            buf[2] = 0x80 + (c & 0x3F);
            return 3;
        } else {
            return -1;
        }
    } else if(c <= 0x10FFFF) {
        buf[0] = 0xF0 + (c >> 18);
        buf[1] = 0x80 + ((c >> 12) & 0x3F);
        buf[2] = 0x80 + ((c >> 6) & 0x3F);
        buf[3] = 0x80 + (c & 0x3F);
        return 4;
    }
    return -1;
}

int
utf8_validate(unsigned char* data, size_t size)
{
    int ulen = -1;
    int ui;

    if((data[0] & 0x80) == 0x00) {
        ulen = 1;
    } if((data[0] & 0xE0) == 0xC0) {
        ulen = 2;
    } else if((data[0] & 0xF0) == 0xE0) {
        ulen = 3;
    } else if((data[0] & 0xF8) == 0xF0) {
        ulen = 4;
    }
    if(ulen < 0 || size_t(ulen) > size) {
        return -1;
    }

    // Check each continuation byte.
    for(ui = 1; ui < ulen; ui++) {
        if((data[ui] & 0xC0) != 0x80) return -1;
    }

    // Wikipedia says I have to check that a UTF-8 encoding
    // uses as few bits as possible. This means that we
    // can't do things like encode 't' in three bytes.
    // To check this all we need to ensure is that for each
    // of the following bit patterns that there is at least
    // one 1 bit in any of the x's
    //  1: 0yyyyyyy
    //  2: 110xxxxy 10yyyyyy
    //  3: 1110xxxx 10xyyyyy 10yyyyyy
    //  4: 11110xxx 10xxyyyy 10yyyyyy 10yyyyyy

    // ulen == 1 passes by definition
    if(ulen == 2) {
        if((data[0] & 0x1E) == 0)
            return -1;
    } else if(ulen == 3) {
        if((data[0] & 0x0F) + (data[1] & 0x20) == 0)
            return -1;
    } else if(ulen == 4) {
        if((data[0] & 0x07) + (data[1] & 0x30) == 0)
            return -1;
    }

    // Lastly we need to check some miscellaneous ranges for
    // some of the larger code point values.
    if(ulen >= 3) {
        ui = utf8_to_unicode(data, ulen);
        if(ui < 0) {
            return -1;
        } else if(ui >= 0xD800 && ui <= 0xDFFF) {
            return -1;
        } else if(ui > 0x10FFFF) {
            return -1;
        }
    }

    return ulen;
}

int
unicode_from_pair(int hi, int lo)
{
    if(hi < 0xD800 || hi >= 0xDC00) return -1;
    if(lo < 0xDC00 || lo > 0xDFFF) return -1;
    return ((hi & 0x3FF) << 10) + (lo & 0x3FF) + 0x10000;
}

int
unicode_uescape(int val, unsigned char* p)
{
    int n;
    if(val < 0x10000) {
        p[0] = '\\';
        p[1] = 'u';
        if(int_to_hex(val, p+2) < 0) {
            return -1;
        }
        return 6;
    } else if (val <= 0x10FFFF) {
        n = val - 0x10000;
        p[0] = '\\';
        p[1] = 'u';
        if(int_to_hex((0xD800 | ((n >> 10) & 0x03FF)), p+2) < 0) {
            return -1;
        }
        p[6] = '\\';
        p[7] = 'u';
        if(int_to_hex((0xDC00 | (n & 0x03FF)), p+8) < 0) {
            return -1;
        }
        return 12;
    }
    return -1;
}

//----- Encoder -----

typedef struct {
    ErlNifEnv*      env;
    jiffy_st*       atoms;

    size_t          bytes_per_red;

    int             uescape;
    int             pretty;
    int             use_nil;
    int             escape_forward_slashes;

    int             shiftcnt;
    int             count;

    size_t          iosize;
    ERL_NIF_TERM    iolist;
    int             partial_output;

    ErlNifBinary    buffer;
    int             have_buffer;

    unsigned char*  p;
    size_t          i;
} Encoder;


// String constants for pretty printing.
// Every string starts with its length.
#define NUM_SHIFTS 8
static const char* shifts[NUM_SHIFTS] = {
    "\x01\n",
    "\x03\n  ",
    "\x05\n    ",
    "\x07\n      ",
    "\x09\n        ",
    "\x0b\n          ",
    "\x0d\n            ",
    "\x0f\n              "
};


Encoder*
enc_new(ErlNifEnv* env)
{
    jiffy_st* st = (jiffy_st*) enif_priv_data(env);
    Encoder* e = static_cast<Encoder*>(enif_alloc_resource(st->res_enc, sizeof(Encoder)));

    e->atoms = st;
    e->bytes_per_red = DEFAULT_BYTES_PER_REDUCTION;
    e->uescape = 0;
    e->pretty = 0;
    e->use_nil = 0;
    e->escape_forward_slashes = 0;
    e->shiftcnt = 0;
    e->count = 0;

    e->iosize = 0;
    e->iolist = enif_make_list(env, 0);

    e->partial_output = 0;

    if(!enif_alloc_binary(BIN_INC_SIZE, &e->buffer)) {
        enif_release_resource(e);
        return NULL;
    }

    e->have_buffer = 1;

    e->p = e->buffer.data;
    e->i = 0;

    return e;
}

int
enc_init(Encoder* e, ErlNifEnv* env)
{
    e->env = env;
    return 1;
}

void
enc_destroy(ErlNifEnv* env, void* obj)
{
    Encoder* e = (Encoder*) obj;

    if(e->have_buffer) {
        enif_release_binary(&e->buffer);
    }
}

#define enc_error(e, msg) enc_error_info(e, msg, __LINE__)

ERL_NIF_TERM
enc_error_info(Encoder* e, const char* msg, int line)
{
    return enif_make_tuple2(e->env,
        ATOM_ERROR,
        enif_make_tuple2(e->env, make_atom(e->env, msg), enif_make_int(e->env, line)));
}

ERL_NIF_TERM
enc_obj_error(Encoder* e, const char* msg, ERL_NIF_TERM obj)
{
    return make_obj_error(e->atoms, e->env, msg, obj);
}

int
enc_flush(Encoder* e)
{
    ERL_NIF_TERM bin;

    if(e->i == 0) {
        return 1;
    }

    if(e->i < e->buffer.size) {
        if(!enif_realloc_binary(&e->buffer, e->i)) {
            return 0;
        }
    }

    bin = enif_make_binary(e->env, &e->buffer);
    e->have_buffer = 0;

    e->iolist = enif_make_list_cell(e->env, bin, e->iolist);
    e->iosize += e->i;

    return 1;
}

static inline int
enc_ensure(Encoder* e, size_t req)
{
    size_t new_size = BIN_INC_SIZE;

    if(e->have_buffer) {
        if(req < (e->buffer.size - e->i)) {
            return 1;
        }

        if(!enc_flush(e)) {
            return 0;
        }

        if(e->have_buffer) {
            return 1;
        }
    }

    for(new_size = BIN_INC_SIZE; new_size < req; new_size <<= 1);

    if(!enif_alloc_binary(new_size, &e->buffer)) {
        return 0;
    }

    e->have_buffer = 1;

    e->p = e->buffer.data;
    e->i = 0;

    return 1;
}

static inline int
enc_literal(Encoder* e, const char* literal, size_t len)
{
    if(!enc_ensure(e, len)) {
        return 0;
    }

    memcpy(&(e->p[e->i]), literal, len);
    e->i += len;
    e->count++;
    return 1;
}

static inline int
enc_unknown(Encoder* e, ERL_NIF_TERM value) {
    // Bignums are encoded in Erlang as the NIF API
    // does not have functions for dealing with them.
    if(!enc_flush(e)) {
        return 0;
    }

    e->iolist = enif_make_list_cell(e->env, value, e->iolist);
    e->partial_output = 1;

    return 1;
}

static inline int
enc_special_character(Encoder* e, int val) {
    switch(val) {
        case '\"':
        case '\\':
            e->p[e->i++] = '\\';
            e->p[e->i++] = val;
            return 1;
        case '\b':
            e->p[e->i++] = '\\';
            e->p[e->i++] = 'b';
            return 1;
        case '\f':
            e->p[e->i++] = '\\';
            e->p[e->i++] = 'f';
            return 1;
        case '\n':
            e->p[e->i++] = '\\';
            e->p[e->i++] = 'n';
            return 1;
        case '\r':
            e->p[e->i++] = '\\';
            e->p[e->i++] = 'r';
            return 1;
        case '\t':
            e->p[e->i++] = '\\';
            e->p[e->i++] = 't';
            return 1;
        case '/':
            if(e->escape_forward_slashes) {
                e->p[e->i++] = '\\';
            }
            e->p[e->i++] = '/';
            return 1;
        default:
            if(val < 0x20) {
                e->i += unicode_uescape(val, &(e->p[e->i]));
                return 1;
            }

            return 0;
    }
}

static int
enc_atom(Encoder* e, ERL_NIF_TERM val)
{
    static const int MAX_ESCAPE_LEN = 12;
    unsigned char data[512];

    if(!enif_get_atom(e->env, val, (char*)data, 512, ERL_NIF_LATIN1)) {
        return 0;
    }

    size_t size = strlen((const char*)data);

    /* Reserve space for the first quotation mark and most of the output. */
    if(!enc_ensure(e, size + MAX_ESCAPE_LEN + 1)) {
        return 0;
    }

    e->p[e->i++] = '\"';

    size_t i = 0;
    while(i < size) {
        if(!enc_ensure(e, MAX_ESCAPE_LEN)) {
            return 0;
        }

        if(enc_special_character(e, data[i])) {
            i++;
        } else if(data[i] < 0x80) {
            e->p[e->i++] = data[i];
            i++;
        } else if(data[i] >= 0x80) {
            /* The atom encoding is latin1, so we don't need validation
             * as all latin1 characters are valid Unicode codepoints. */
            if (!e->uescape) {
                e->i += unicode_to_utf8(data[i], &e->p[e->i]);
            } else {
                e->i += unicode_uescape(data[i], &e->p[e->i]);
            }

            i++;
        }
    }

    if(!enc_ensure(e, 1)) {
        return 0;
    }

    e->p[e->i++] = '\"';
    e->count++;

    return 1;
}

static int
enc_string(Encoder* e, ERL_NIF_TERM val)
{
    static const int MAX_ESCAPE_LEN = 12;
    ErlNifBinary bin;

    int esc_len;
    int ulen;
    int uval;

    if(!enif_inspect_binary(e->env, val, &bin)) {
        return 0;
    }

    auto data = bin.data;
    auto size = bin.size;

    /* Reserve space for the first quotation mark and most of the output. */
    if(!enc_ensure(e, size + MAX_ESCAPE_LEN + 1)) {
        return 0;
    }

    e->p[e->i++] = '\"';

    size_t i = 0;
    while(i < size) {
        if(!enc_ensure(e, MAX_ESCAPE_LEN)) {
            return 0;
        }

        if(enc_special_character(e, data[i])) {
            i++;
        } else if(data[i] < 0x80) {
            e->p[e->i++] = data[i++];
        } else if(data[i] >= 0x80) {
            ulen = utf8_validate(&(data[i]), size - i);

            if (ulen < 0) {
                return 0;
            } else if (e->uescape) {
                uval = utf8_to_unicode(&(data[i]), size-i);
                if(uval < 0) {
                    return 0;
                }

                esc_len = unicode_uescape(uval, &(e->p[e->i]));
                if(esc_len < 0) {
                    return 0;
                }

                e->i += esc_len;
            } else {
                memcpy(&e->p[e->i], &data[i], ulen);
                e->i += ulen;
            }

            i += ulen;
        }
    }

    if(!enc_ensure(e, 1)) {
        return 0;
    }

    e->p[e->i++] = '\"';
    e->count++;

    return 1;
}

static inline int
enc_object_key(ErlNifEnv *env, Encoder* e, ERL_NIF_TERM val)
{
    if(enif_is_atom(env, val)) {
        return enc_atom(e, val);
    }

    return enc_string(e, val);
}

// From https://www.slideshare.net/andreialexandrescu1/three-optimization-tips-for-c-15708507

#define P01 10
#define P02 100
#define P03 1000
#define P04 10000
#define P05 100000
#define P06 1000000
#define P07 10000000
#define P08 100000000
#define P09 1000000000
#define P10 10000000000
#define P11 100000000000L
#define P12 1000000000000L

int
digits10(ErlNifUInt64 v)
{
    if (v < P01) return 1;
    if (v < P02) return 2;
    if (v < P03) return 3;
    if (v < P12) {
        if (v < P08) {
            if (v < P06) {
                if (v < P04) {
                    return 4;
                }
                return 5 + (v >= P05);
            }
            return 7 + (v >= P07);
        }
        if (v < P10) {
            return 9 + (v >= P09);
        }
        return 11 + (v >= P11);
    }
    return 12 + digits10(v / P12);
}

unsigned int
u64ToAsciiTable(unsigned char *dst, ErlNifUInt64 value)
{
    static const char digits[201] =
        "0001020304050607080910111213141516171819"
        "2021222324252627282930313233343536373839"
        "4041424344454647484950515253545556575859"
        "6061626364656667686970717273747576777879"
        "8081828384858687888990919293949596979899";
    const int length = digits10(value);
    int next = length - 1;
    while (value >= 100) {
        const int i = (value % 100) * 2;
        value /= 100;
        dst[next] = digits[i + 1];
        dst[next - 1] = digits[i];
        next -= 2;
    }
    // Handle last 1-2 digits.
    if (value < 10) {
        dst[next] = '0' + (unsigned int) value;
    } else {
        const int i = (unsigned int) value * 2;
        dst[next] = digits[i + 1];
        dst[next - 1] = digits[i];
    }
    return length;
}

unsigned
i64ToAsciiTable(unsigned char *dst, ErlNifSInt64 value)
{
    if (value < 0) {
        *dst++ = '-';
        return 1 + u64ToAsciiTable(dst, -value);
    } else {
        return u64ToAsciiTable(dst, value);
    }
}

static inline int
enc_long(Encoder* e, ErlNifSInt64 val)
{
    if(!enc_ensure(e, 32)) {
        return 0;
    }

    e->i += i64ToAsciiTable(&(e->p[e->i]), val);
    e->count++;

    return 1;
}

static inline int
enc_double(Encoder* e, double val)
{
    unsigned char* start;

    if(!enc_ensure(e, 32)) {
        return 0;
    }

    start = &(e->p[e->i]);

    size_t len = simdjson::internal::to_chars(
      reinterpret_cast<char*>(start),
      reinterpret_cast<char*>(start + e->buffer.size), val) -
      reinterpret_cast<char*>(start);

    e->i += len;
    e->count++;
    return 1;
}

static inline int
enc_char(Encoder* e, char c)
{
    if(!enc_ensure(e, 1)) {
        return 0;
    }

    e->p[e->i++] = c;
    return 1;
}

static int
enc_shift(Encoder* e) {
    int i;
    assert(e->shiftcnt >= 0 && "Invalid shift count.");
    auto* shift = shifts[MIN(e->shiftcnt, NUM_SHIFTS-1)];

    if(!enc_literal(e, shift + 1, *shift))
        return 0;

    // Finish the rest of this shift it's it bigger than
    // our largest predefined constant.
    for(i = NUM_SHIFTS - 1; i < e->shiftcnt; i++) {
        if(!enc_literal(e, "  ", 2))
            return 0;
    }

    return 1;
}

static inline int
enc_start_object(Encoder* e)
{
    e->count++;
    e->shiftcnt++;
    if(!enc_char(e, '{'))
        return 0;
    MAYBE_PRETTY(e);
    return 1;
}

static inline int
enc_end_object(Encoder* e)
{
    e->shiftcnt--;
    MAYBE_PRETTY(e);
    return enc_char(e, '}');
}

static inline int
enc_start_array(Encoder* e)
{
    e->count++;
    e->shiftcnt++;
    if(!enc_char(e, '['))
        return 0;
    MAYBE_PRETTY(e);
    return 1;
}

static inline int
enc_end_array(Encoder* e)
{
    e->shiftcnt--;
    MAYBE_PRETTY(e);
    return enc_char(e, ']');
}

static inline int
enc_colon(Encoder* e)
{
    if(e->pretty)
        return enc_literal(e, " : ", 3);
    return enc_char(e, ':');
}

static inline int
enc_comma(Encoder* e)
{
    if(!enc_char(e, ','))
        return 0;
    MAYBE_PRETTY(e);
    return 1;
}

#if MAP_TYPE_PRESENT
int
enc_map_to_ejson(ErlNifEnv* env, ERL_NIF_TERM map, ERL_NIF_TERM* out)
{
    ErlNifMapIterator iter;
    size_t size;

    ERL_NIF_TERM list;
    ERL_NIF_TERM tuple;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;

    if(!enif_get_map_size(env, map, &size)) {
        return 0;
    }

    list = enif_make_list(env, 0);

    if(size == 0) {
        *out = enif_make_tuple1(env, list);
        return 1;
    }

    if(!enif_map_iterator_create(env, map, &iter, ERL_NIF_MAP_ITERATOR_HEAD)) {
        return 0;
    }

    do {
        if(!enif_map_iterator_get_pair(env, &iter, &key, &val)) {
            enif_map_iterator_destroy(env, &iter);
            return 0;
        }
        tuple = enif_make_tuple2(env, key, val);
        list = enif_make_list_cell(env, tuple, list);
    } while(enif_map_iterator_next(env, &iter));

    enif_map_iterator_destroy(env, &iter);

    *out = enif_make_tuple1(env, list);
    return 1;
}
#endif

ERL_NIF_TERM
encode_iter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    jiffy_st* st = (jiffy_st*) enif_priv_data(env);
    Encoder* e;
    TermStack stack;

    ERL_NIF_TERM ret = 0;

    ERL_NIF_TERM curr;
    ERL_NIF_TERM item;
    const ERL_NIF_TERM* tuple;
    ERL_NIF_TERM tmp_argv[3];
    int arity;
    ErlNifSInt64 lval;
    double dval;

    void* res;

    size_t start;
    size_t bytes_processed = 0;

    if(!enif_get_resource(env, argv[0], st->res_enc, &res)) {
        return enif_make_badarg(env);
    }

    e = (Encoder*) res;

    if(!enc_init(e, env)) {
        return enif_make_badarg(env);
    }

    if(!termstack_restore(env, argv[1], &stack)) {
        return enif_make_badarg(env);
    }

    e->iolist = argv[2];

    start = e->iosize + e->i;

    while(!termstack_is_empty(&stack)) {
        bytes_processed = (e->iosize + e->i) - start;

        if(should_yield(bytes_processed, e->bytes_per_red)) {

            assert(enif_is_list(env, e->iolist));

            tmp_argv[0] = argv[0];
            tmp_argv[1] = termstack_save(env, &stack);
            tmp_argv[2] = e->iolist;

            termstack_destroy(&stack);
            bump_used_reds(env, bytes_processed, e->bytes_per_red);

#if SCHEDULE_NIF_PRESENT
            return enif_schedule_nif(
                    env,
                    "nif_encode_iter",
                    0,
                    encode_iter,
                    3,
                    tmp_argv
                );
#else
            return enif_make_tuple2(
                    env,
                    ATOM_ITER,
                    enif_make_tuple_from_array(env, tmp_argv, 3)
                );
#endif
        }

        curr = termstack_pop(&stack);

        if(enif_is_atom(env, curr)) {
            if(enif_is_identical(curr, e->atoms->ref_object)) {
                curr = termstack_pop(&stack);

                if(!enif_get_list_cell(env, curr, &item, &curr)) {
                    if(!enc_end_object(e)) {
                        ret = enc_error(e, "internal_error");
                        goto done;
                    }
                    continue;
                }
                if(!enif_get_tuple(env, item, &arity, &tuple)) {
                    ret = enc_obj_error(e, "invalid_object_member", item);
                    goto done;
                }
                if(arity != 2) {
                    ret = enc_obj_error(e, "invalid_object_member_arity", item);
                    goto done;
                }
                if(!enc_comma(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }
                if(!enc_object_key(env, e, tuple[0])) {
                    ret = enc_obj_error(e, "invalid_object_member_key", tuple[0]);
                    goto done;
                }
                if(!enc_colon(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }

                termstack_push(&stack, curr);
                termstack_push(&stack, e->atoms->ref_object);
                termstack_push(&stack, tuple[1]);
            } else if(enif_is_identical(curr, e->atoms->ref_array)) {
                curr = termstack_pop(&stack);

                if(!enif_get_list_cell(env, curr, &item, &curr)) {
                    if(!enc_end_array(e)) {
                        ret = enc_error(e, "internal_error");
                        goto done;
                    }
                    continue;
                }
                if(!enc_comma(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }

                termstack_push(&stack, curr);
                termstack_push(&stack, e->atoms->ref_array);
                termstack_push(&stack, item);
            } else if(enif_is_identical(curr, ATOM_NULL)) {
                if(!enc_literal(e, "null", 4)) {
                    ret = enc_error(e, "null");
                    goto done;
                }
            } else if(e->use_nil && enif_is_identical(curr, ATOM_NIL)) {
                if(!enc_literal(e, "null", 4)) {
                    ret = enc_error(e, "null");
                    goto done;
                }
            } else if(enif_is_identical(curr, ATOM_TRUE)) {
                if(!enc_literal(e, "true", 4)) {
                    ret = enc_error(e, "true");
                    goto done;
                }
            } else if(enif_is_identical(curr, ATOM_FALSE)) {
                if(!enc_literal(e, "false", 5)) {
                    ret = enc_error(e, "false");
                    goto done;
                }
            } else if(!enc_atom(e, curr)) {
                ret = enc_obj_error(e, "invalid_string", curr);
                goto done;
            }
        } else if(enif_is_binary(env, curr)) {
            if(!enc_string(e, curr)) {
                ret = enc_obj_error(e, "invalid_string", curr);
                goto done;
            }
        } else if(enif_get_int64(env, curr, &lval)) {
            if(!enc_long(e, lval)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
        } else if(enif_get_double(env, curr, &dval)) {
            if(!enc_double(e, dval)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
        } else if(enif_get_tuple(env, curr, &arity, &tuple)) {
            if(arity != 1) {
                ret = enc_obj_error(e, "invalid_ejson", curr);
                goto done;
            }
            if(!enif_is_list(env, tuple[0])) {
                ret = enc_obj_error(e, "invalid_object", curr);
                goto done;
            }
            if(!enc_start_object(e)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
            if(!enif_get_list_cell(env, tuple[0], &item, &curr)) {
                if(!enc_end_object(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }
                continue;
            }
            if(!enif_get_tuple(env, item, &arity, &tuple)) {
                ret = enc_obj_error(e, "invalid_object_member", item);
                goto done;
            }
            if(arity != 2) {
                ret = enc_obj_error(e, "invalid_object_member_arity", item);
                goto done;
            }
            if(!enc_object_key(env, e, tuple[0])) {
                ret = enc_obj_error(e, "invalid_object_member_key", tuple[0]);
                goto done;
            }
            if(!enc_colon(e)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }

            termstack_push(&stack, curr);
            termstack_push(&stack, e->atoms->ref_object);
            termstack_push(&stack, tuple[1]);
#if MAP_TYPE_PRESENT
        } else if(enif_is_map(env, curr)) {
            if(!enc_map_to_ejson(env, curr, &curr)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }

            termstack_push(&stack, curr);
#endif
        } else if(enif_is_list(env, curr)) {
            if(!enc_start_array(e)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }

            if(!enif_get_list_cell(env, curr, &item, &curr)) {
                if(!enc_end_array(e)) {
                    ret = enc_error(e, "internal_error");
                    goto done;
                }
                continue;
            }

            termstack_push(&stack, curr);
            termstack_push(&stack, e->atoms->ref_array);
            termstack_push(&stack, item);
        } else {
            if(!enc_unknown(e, curr)) {
                ret = enc_error(e, "internal_error");
                goto done;
            }
        }
    }

    if(!enc_flush(e)) {
        ret = enc_error(e, "internal_error");
        goto done;
    }

    assert(enif_is_list(env, e->iolist));

    if(e->partial_output) {
        ret = enif_make_tuple2(env, ATOM_PARTIAL, e->iolist);
    } else {
        ret = e->iolist;
    }

done:
    bump_used_reds(env, bytes_processed, e->bytes_per_red);
    termstack_destroy(&stack);

    return ret;
}

ERL_NIF_TERM
encode_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    jiffy_st* st = (jiffy_st*) enif_priv_data(env);
    Encoder* e;

    ERL_NIF_TERM opts;
    ERL_NIF_TERM val;
    ERL_NIF_TERM tmp_argv[3];

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    e = enc_new(env);
    if(e == NULL) {
        return make_error(st, env, "internal_error");
    }

    tmp_argv[0] = enif_make_resource(env, e);
    tmp_argv[1] = enif_make_tuple1(env, argv[0]);
    tmp_argv[2] = enif_make_list(env, 0);

    enif_release_resource(e);

    opts = argv[1];
    if(!enif_is_list(env, opts)) {
        return enif_make_badarg(env);
    }

    int   arity;
    const ERL_NIF_TERM* array;

    while(enif_get_list_cell(env, opts, &val, &opts)) {
        if(enif_is_identical(val, ATOM_UESCAPE))
            e->uescape = 1;
        else if(enif_is_identical(val, ATOM_PRETTY))
            e->pretty = 1;
        else if(enif_is_identical(val, ATOM_ESCAPE_FWD_SLASH))
            e->escape_forward_slashes = 1;
        else if(enif_is_identical(val, ATOM_USE_NIL))
            e->use_nil = 1;
        else if(enif_is_identical(val, ATOM_FORCE_UTF8))
            continue; // Ignore, handled in Erlang
        else if(!enif_get_tuple(env, val, &arity, &array) || arity != 2)
            return enif_raise_exception(env, enif_make_tuple2(env, ATOM_BADARG, val));
        else if(enif_compare(array[0], ATOM_BYTES_PER_RED) && enif_get_uint64(env, val, &(e->bytes_per_red)))
            continue;
        else
          return enif_raise_exception(env, enif_make_tuple2(env, ATOM_BADARG, val));
    }

    return encode_iter(env, 3, tmp_argv);
}
