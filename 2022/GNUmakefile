.PHONY: all clean

MODULES := $(patsubst %.erl,%.beam,$(wildcard *.erl))

all: $(MODULES)

clean:
	$(RM) $(MODULES)

%.beam: %.erl
	erlc -DTEST -Wall -o $@ $<
