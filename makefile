CXX = g++
CXXFLAGS = -std=c++20 -O3 -Wall -Wextra -pedantic -I.
.ONESHELL:
.PHONY: test run puzzles clean

test: build/test
	build/test

survey: build/puzzles
	build/puzzles 1000 9S,7S,AH,8H,QC,7C,10D,QD,6D > survey/9-3nd&
	build/puzzles 1000 9S,QS,8S,7S,AH,8H,QC,7C,6D > survey/9-4th&
	build/puzzles 1000 9S,QS,10S,8S,7S,AH,8H,QC,6D > survey/9-5th&
	build/puzzles 1000 9S,QS,10S,8S,7S,6S,AH,QC,6D > survey/9-6th&
	build/puzzles 1000 JS,9S,7H,8H,10H,QH,QC,AC,AD > survey/solo-j-9&
	build/puzzles 1000 9S,KS,QS,AH,8H,QC,7C,10D,6D > survey/9-3th-belote&
	build/puzzles 1000 9S,KS,QS,7S,AH,8H,QC,7C,6D > survey/9-4th-belote&
	build/puzzles 1000 9S,QS,10S,8S,7S,AH,AC,KC,AD > survey/9-5th-3-aces&
	build/puzzles 1000 JS,QS,6S,QH,8H,AC,7C,AD,10D > survey/j-3rd&
	build/puzzles 1000 JS,QS,6S,9H,QH,AC,7C,AD,10D > survey/j-3rd-9-2nd&
	build/puzzles 1000 JS,QS,6S,9H,QH,AC,7C,9D,AD > survey/j-3rd-2x-9-2nd&
	build/puzzles 1000 JS,QS,6S,9H,QH,9C,AC,9D,AD > survey/j-3rd-3x-9-2nd&

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
