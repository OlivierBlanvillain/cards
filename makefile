CXX = g++
CXXFLAGS = -std=c++20 -O3 -Wall -Wextra -pedantic -I.
.ONESHELL:
.PHONY: test run puzzles clean study

test: build/test
	build/test

study: build/puzzles
	for x in 9S,KS,6S,7H,6H,7C,6C,7D,6D:study/9K6-0T 9S,KS,6S,10H,6H,7C,6C,7D,6D:study/9K6-1T 9S,KS,6S,10H,6H,10C,6C,7D,6D:study/9K6-2T 9S,KS,6S,10H,6H,10C,6C,10D,6D:study/9K6-3T JS,7S,6S,7H,6H,7C,6C,7D,6D:study/J3rd-0T JS,7S,6S,10H,6H,7C,6C,7D,6D:study/J3rd-1T JS,7S,6S,10H,6H,10C,6C,7D,6D:study/J3rd-2T JS,7S,6S,10H,6H,10C,6C,10D,6D:study/J3rd-3T JS,9S,6S,7H,6H,7C,6C,7D,6D:study/J93rd-0T JS,9S,6S,10H,6H,7C,6C,7D,6D:study/J93rd-1T JS,9S,6S,10H,6H,10C,6C,7D,6D:study/J93rd-2T JS,9S,6S,10H,6H,10C,6C,10D,6D:study/J93rd-3T 9S,KS,10S,7H,6H,7C,6C,7D,6D:study/9KT-0T 9S,KS,10S,10H,6H,7C,6C,7D,6D:study/9KT-1T 9S,KS,10S,10H,6H,10C,6C,7D,6D:study/9KT-2T 9S,KS,10S,10H,6H,10C,6C,10D,6D:study/9KT-3T JS,7S,10S,7H,6H,7C,6C,7D,6D:study/JT3rd-0T JS,7S,10S,10H,6H,7C,6C,7D,6D:study/JT3rd-1T JS,7S,10S,10H,6H,10C,6C,7D,6D:study/JT3rd-2T JS,7S,10S,10H,6H,10C,6C,10D,6D:study/JT3rd-3T JS,9S,10S,7H,6H,7C,6C,7D,6D:study/J9T-0T JS,9S,10S,10H,6H,7C,6C,7D,6D:study/J9T-1T JS,9S,10S,10H,6H,10C,6C,7D,6D:study/J9T-2T JS,9S,10S,10H,6H,10C,6C,10D,6D:study/J9T-3T; do echo "build/puzzles 1000 $$(echo $$x | cut -d: -f1) > $$(echo $$x | cut -d: -f2)"; done | xargs -i -P12 bash -c "{}"

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
