CXX = g++
CXXFLAGS = -std=c++20 -Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wunreachable-code -Wno-unused-parameter -O3

.PHONY: all test clean bench run

all: test

test: build/test_jass
	build/test_jass

bench: build/bench_jass
	build/bench_jass

run: build/simulation
	build/simulation

build/%: build/jass.o build/%.o
	$(CXX) $(CXXFLAGS) -o $@ $^ 

build/%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm build/*
