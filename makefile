CXX = g++
CXXFLAGS = -std=c++20 -O3 -Wall -Wextra -pedantic -I.
.ONESHELL:
.PHONY: test run puzzles clean

test: build/test
	build/test

run: build/puzzles
	build/puzzles 1000 7S,QH,10H,9H,8H,7H,QD,10D,AC

puzzles: build/puzzles
	mkdir -p puzzles
	seq 1000 | xargs -n1 -P12 bash -xc 'build/puzzles 1000 | tee puzzles/$$(cat /dev/urandom | tr -dc A-Za-z0-9 | head -c8)'

quick_eval: build/quick_eval
	mkdir -p quick_eval
	seq 1000 | xargs -n1 -P12 bash -xc 'build/quick_eval | tee quick_eval/$$(cat /dev/urandom | tr -dc A-Za-z0-9 | head -c16)'

build/test: build/jass.o build/simulation.o build/test.o
	$(CXX) $(CXXFLAGS) -o $@ $^

build/puzzles: build/jass.o build/simulation.o build/puzzles.o
	$(CXX) $(CXXFLAGS) -o $@ $^

build/quick_eval: build/jass.o build/simulation.o build/quick_eval.o
	$(CXX) $(CXXFLAGS) -o $@ $^

build/main: build/jass.o build/simulation.o build/main.o
	$(CXX) $(CXXFLAGS) -o $@ $^

build/%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -rf build/*
