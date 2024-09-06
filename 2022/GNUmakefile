.PHONY: all clean

all:	\
	day_01.beam	\
	day_02.beam

clean:
	$(RM) *.beam

%.beam: %.erl
	erlc -Wall -o $@ $<
