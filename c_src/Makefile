CURDIR		:= $(shell pwd)
BASEDIR		:= $(abspath $(dir $(CURDIR)))
PROJECT		?= simdjsone
PROJECT		:= $(strip $(PROJECT))

ERL_CXXFLAGS ?= $(shell erl -noshell -noinput -eval "io:format(\"-I~ts/erts-~ts/include -I~ts\", [code:root_dir(), erlang:system_info(version), code:lib_dir(erl_interface, include)]), halt(0).")
ERL_LDFLAGS  ?= $(shell erl -noshell -noinput -eval "io:format(\"-L~ts\", [code:lib_dir(erl_interface, lib)]), halt(0).")

DEBUG     ?= 0
NIF_DEBUG ?= $(DEBUG)

# System type and C compiler/flags.

ifneq ($(SIMDJSON_NOEXCEPTIONS),)
  ADD_FLAGS := -DSIMDJSON_DISABLE_DEPRECATED_API -DSIMDJSON_EXCEPTIONS=0
else
  ADD_FLAGS := -DSIMDJSON_DISABLE_DEPRECATED_API
endif

CXX       ?= g++
CXX_VSN   ?= $(shell $(CXX) --version | sed -n '1s/^[^0-9]\+\([0-9]\+\)\(.[0-9-]\)\+.*$$/\1/p')

ifeq ($(NIF_DEBUG),0)
  OPTIMIZE  = -O3 -DNDEBUG
else
  OPTIMIZE  = -O0 -g
endif

UNAME_SYS  := $(shell uname -s)
ifeq ($(UNAME_SYS),Linux)
  CXXFLAGS ?= -mavx2
else ifeq ($(UNAME_SYS),Darwin)
  CXXFLAGS ?= -pie
  LDFLAGS  += -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS),FreeBSD)
  CXXFLAGS ?= -mavx2
endif

ADD_FLAGS  += -fPIC -finline-functions -Wall -std=c++20
CXXFLAGS   += $(ADD_FLAGS) $(OPTIMIZE) $(ERL_CXXFLAGS)
LDFLAGS    += $(ERL_LDFLAGS) -lei -shared

SRC_DIR     = $(CURDIR)
SO_OUTPUT  ?= $(BASEDIR)/priv/$(PROJECT).so

SOURCES    := $(wildcard *.c*)
OBJECTS     = $(SOURCES:%.cpp=%.o)

COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

# Targets

all: deps $(SO_OUTPUT)

deps: simdjson.h simdjson.cpp

clean:
	rm -f $(SO_OUTPUT) *.o

info:
	@echo "SOURCES: $(SOURCES)"
	@echo "OBJECTS: $(OBJECTS)"
	@echo "OUTPUT:  $(SO_OUTPUT)"

simdjson.h simdjson.cpp:
	wget -q https://raw.githubusercontent.com/simdjson/simdjson/master/singleheader/$@

$(SO_OUTPUT): $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(CXX) $(OBJECTS) $(LDFLAGS) -o $(SO_OUTPUT)

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

