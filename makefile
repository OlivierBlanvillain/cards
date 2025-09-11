CXX = g++
CXXFLAGS = -std=c++20 -O3 -Wall -Wextra -pedantic -I.
.ONESHELL:
.PHONY: test run puzzles clean study

test: build/test
	build/test

study: build/puzzles
	cp study/9KQ-0J2nd study/9KQ-0KQ
	build/puzzles 1000 9S,KS,QS,KH,QH,7C,6C,7D,6D > study/9KQ-1KQ&
	build/puzzles 1000 9S,KS,QS,KH,QH,KC,QC,7D,6D > study/9KQ-2KQ&
	build/puzzles 1000 9S,KS,QS,KH,QH,KC,QC,KD,QD > study/9KQ-3KQ&
	cp study/J3rd-0J2nd study/J3rd-0KQ
	build/puzzles 1000 JS,7S,6S,KH,QH,7C,6C,7D,6D > study/J3rd-1KQ&
	build/puzzles 1000 JS,7S,6S,KH,QH,KC,QC,7D,6D > study/J3rd-2KQ&
	build/puzzles 1000 JS,7S,6S,KH,QH,KC,QC,KD,QD > study/J3rd-3KQ&
	cp study/J93rd-0J2nd study/J93rd-0KQ
	build/puzzles 1000 JS,9S,6S,KH,QH,7C,6C,7D,6D > study/J93rd-1KQ&
	build/puzzles 1000 JS,9S,6S,KH,QH,KC,QC,7D,6D > study/J93rd-2KQ&
	build/puzzles 1000 JS,9S,6S,KH,QH,KC,QC,KD,QD > study/J93rd-3KQ&

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
