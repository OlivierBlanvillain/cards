CXX = g++
CXXFLAGS = -std=c++20 -Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wunreachable-code -Wno-unused-parameter -O3

.PHONY: all test clean bench

all: test

test: test_jass
	build/test_jass

bench: bench_jass
	build/bench_jass

test_jass: jass.o test_jass.o
	$(CXX) $(CXXFLAGS) -o build/test_jass jass.o test_jass.o

bench_jass: jass.o bench.o
	$(CXX) $(CXXFLAGS) -o build/bench_jass jass.o bench.o

%.o: %.cpp jass.h
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -f *.o test_jass bench_jass
