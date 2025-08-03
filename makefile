CXX = g++
CXXFLAGS = -std=c++20 -O3 -Wall -Wextra -pedantic -I.
.ONESHELL:
.PHONY: test clean bench simulation

test: build/test
	build/test

bench: build/bench
	build/bench

run: build/simulation
	build/simulation

sims: build/simulation
	seq 1000 | xargs -n1 -P12 bash -xc 'build/simulation | tee sims/$$(cat /dev/urandom | tr -dc A-Za-z0-9 /dev/urandom | head -c16)'

build/test: build/jass.o build/test.o build/simulation.o
build/%: build/jass.o build/%.o
	$(CXX) $(CXXFLAGS) -o $@ $^

build/%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -rf build/*
