CXX = g++
CXXFLAGS = -std=c++20 -Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wunreachable-code -Wno-unused-parameter -O3

.PHONY: all test clean bench run

all: test

test: test_jass
	build/test_jass

bench: bench_jass
	build/bench_jass

run: simulation
	build/simulation

test_jass: jass.o test_jass.o
	$(CXX) $(CXXFLAGS) -o build/test_jass jass.o test_jass.o

bench_jass: jass.o bench.o
	$(CXX) $(CXXFLAGS) -o build/bench_jass jass.o bench.o

simulation: jass.o simulation.o
	$(CXX) $(CXXFLAGS) -o build/simulation jass.o simulation.o

%.o: %.cpp jass.h
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -f *.o test_jass bench_jass simulation
