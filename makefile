CXX = g++
CXXFLAGS = -std=c++20 -Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wunreachable-code -Wno-unused-parameter -O3

.PHONY: all test clean

all: test

test: test_jass
	./test_jass

test_jass: jass.o test_jass.o
	$(CXX) $(CXXFLAGS) -o test_jass jass.o test_jass.o

jass.o: jass.cpp jass.h
	$(CXX) $(CXXFLAGS) -c jass.cpp

test_jass.o: test_jass.cpp jass.h
	$(CXX) $(CXXFLAGS) -c test_jass.cpp

clean:
	rm -f *.o test_jass