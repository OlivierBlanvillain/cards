CXX = g++
CXXFLAGS = -std=c++20 -Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wunreachable-code -Wno-unused-parameter -O3
.ONESHELL:
.PHONY: test clean bench simulation

test: build/test
	build/test

bench: build/bench
	build/bench

simulation: build/simulation
	seq 1000 | xargs -n1 -P12 bash -xc 'build/simulation | tee sims/$$(cat /dev/urandom | tr -dc A-Za-z0-9 /dev/urandom | head -c16)'

build/%: build/jass.o build/%.o
	$(CXX) $(CXXFLAGS) -o $@ $^ 

build/%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -rf build/*
