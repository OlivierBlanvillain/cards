CXX = g++
CXXFLAGS = -std=c++20 -Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wunreachable-code -Wno-unused-parameter -O3

.PHONY: all test clean bench run regress venv deps

all: test

test: build/test_jass
	build/test_jass

bench: build/bench_jass
	build/bench_jass

run: build/simulation
	build/simulation

regress: deps
	./venv/bin/python screenshot_test_selenium.py

venv: 
	python3 -m venv venv

deps: venv
	@if [ ! -f ./venv/bin/pip ]; then \
		echo "venv/bin/pip not found, recreating venv"; \
		rm -rf venv; \
		python3 -m venv venv; \
	fi
	@if ! ./venv/bin/pip show selenium > /dev/null 2>&1; then \
		echo "Installing Python dependencies..."; \
		./venv/bin/pip install selenium Pillow numpy; \
	else \
		echo "Python dependencies already installed."; \
	fi

build/%: build/jass.o build/%.o
	$(CXX) $(CXXFLAGS) -o $@ $^ 

build/%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -rf build/* venv/
