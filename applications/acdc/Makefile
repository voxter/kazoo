PROJECT = acdc
ROOT = ../..

EBINS = $(shell find $(ROOT)/core/whistle-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_apps-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_couch-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/lager-* -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS += -Werror +debug_info +warn_export_all $(PA)
ERLC_TEST_OPTS += -Werror +debug_info $(PA)

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
TEST_MODULES = $(shell ls test/*.erl | sed 's/test\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app json
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl
	@mkdir -p ebin/
	ERL_LIBS=$(ERL_LIBS) erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

json: JSON = $(shell find priv/couchdb -name *.json -print)
json:
	@$(ROOT)/scripts/format-json.sh $(JSON)

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(TEST_MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl test/*.erl
	@mkdir -p test/
	ERL_LIBS=$(ERL_LIBS) erlc -v $(ERLC_TEST_OPTS) -DTEST -o test/ -pa test/ $?

clean:
	rm -f ebin/*
	rm -f erl_crash.dump

clean-test:
	rm -f test/*.beam test/$(PROJECT).app

test: clean-test compile-test eunit

TEST_EBINS = $(shell find $(ROOT) -maxdepth 4 -name ebin -print)
TEST_PA = $(foreach EBIN,$(TEST_EBINS),-pa $(EBIN))

eunit: compile-test
	erl -noshell $(PA) $(TEST_PA) -pa test -config test/test.config -sasl sasl_error_logger false -eval "case eunit:test([$(TEST_MODULES),$(MODULES)], [verbose]) of 'ok' -> init:stop(); _ -> init:stop(1) end."
