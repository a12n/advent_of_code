.PHONY: all clean check

MODULES := $(patsubst %.erl,%.beam,$(wildcard *.erl))

all: $(MODULES)

clean:
	$(RM) $(MODULES)

check: $(MODULES)
	dialyzer .

%.beam: %.erl
	erlc +debug_info -DTEST -Wall -o $@ $<
