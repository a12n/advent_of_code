FLAGS = -O6 --intermodule-optimization --verbose-error-messages
MODULES = $(wildcard day_[0-2][0-9]_[12].m day_25.m)
PROGS = $(patsubst %.m,%,$(MODULES))

.PHONY: all clean

all:
	mmc --make $(FLAGS) $(PROGS)

clean:
	$(RM) $(PROGS)
