CXX = g++
CXXFLAGS = -std=c++20 -Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wunreachable-code -Wno-unused-parameter -O3

.PHONY: all test clean benchmark

all: test

test: test_jass
	./test_jass

benchmark: benchmark_jass
	./benchmark_jass

test_jass: jass.o test_jass.o
	$(CXX) $(CXXFLAGS) -o test_jass jass.o test_jass.o

benchmark_jass: jass.o benchmark.o
	$(CXX) $(CXXFLAGS) -o benchmark_jass jass.o benchmark.o

jass.o: jass.cpp jass.h ankerl/unordered_dense.h
	$(CXX) $(CXXFLAGS) -c jass.cpp

test_jass.o: test_jass.cpp jass.h ankerl/unordered_dense.h
	$(CXX) $(CXXFLAGS) -c test_jass.cpp

benchmark.o: benchmark.cpp jass.h ankerl/unordered_dense.h
	$(CXX) $(CXXFLAGS) -c benchmark.cpp

clean:
	rm -f *.o test_jass benchmark_jass