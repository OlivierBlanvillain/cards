SHELL = /bin/bash
.ONESHELL:
.RECIPEPREFIX=-         

CXX = g++
CXXFLAGS = -std=c++20 -Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wunreachable-code -Wno-unused-parameter -O3

.PHONY: test clean bench simulation

test: build/test
- build/test

bench: build/bench
- build/bench

simulation: build/simulation
- build/simulation

build/%: build/jass.o build/%.o
- $(CXX) $(CXXFLAGS) -o $@ $^ 

build/%.o: %.cpp
- $(CXX) $(CXXFLAGS) -c $< -o $@

clean:
- rm -rf build/*
