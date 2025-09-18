CXXFLAGS=	-O2 -Wall -Wextra -Wno-unused-function -Wpedantic -g -std=c++17

.PHONY: all clean

all: $(filter-out 25-2,$(foreach day,$(patsubst %.cpp,%,$(wildcard [0-2][0-9].cpp)),$(day)-1 $(day)-2))

clean:
	$(RM) [0-2][0-9]-[12] advent.o

%-1: %.cpp advent.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -DPART=1 $^ -o $@

%-2: %.cpp advent.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -DPART=2 $^ -o $@

advent.o: advent.cpp
