CXX = g++
CXXFLAGS = -std=c++20 -Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wunreachable-code -Wno-unused-parameter -O3

test: test_jass
	./test_jass

bench: bench_jass
	./bench_jass

test_jass: jass.o test_jass.o
	$(CXX) $(CXXFLAGS) -o test_jass jass.o test_jass.o

bench_jass: jass.o bench.o
	$(CXX) $(CXXFLAGS) -o bench_jass jass.o bench.o

%.o: %.cpp jass.h
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -f *.o test_jass bench_jass
