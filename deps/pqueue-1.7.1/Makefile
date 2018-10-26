ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar

ERL_VERSION := $(shell script -q -c 'erl +V' | cut -d' ' -f6)
ERL_VERSION_MAJOR := $(shell echo $(ERL_VERSION) | cut -d. -f1)
ERL_VERSION_MINOR := $(shell echo $(ERL_VERSION) | cut -d. -f2)
ERL_VERSION_GT_5_10 := $(shell [ $(ERL_VERSION_MAJOR) -gt 5 -o \( $(ERL_VERSION_MAJOR) -eq 5 -a $(ERL_VERSION_MINOR) -ge 10 \) ] && echo true)

ifeq ($(ERL_VERSION_GT_5_10),true)
all: compile
else
all:
endif

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
