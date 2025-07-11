MAKEFLAGS += --no-builtin-rules
.ONESHELL:
.RECIPEPREFIX=-
.PHONY: FORCE

help/page:
- @grep -Po '^[a-z]+(?=:)' makefile | paste -sd"|" | xargs echo make

run: venv/activate FORCE
- venv/bin/python3 cards.py

test: venv/activate FORCE
- venv/bin/python3 -m pytest . -v --capture=no

bench: venv/activate FORCE
- venv/bin/python3 -m pytest benchmark.py -v --capture=no

watch: venv/activate FORCE
- git ls-files | entr make test

check: venv/activate FORCE
- venv/bin/pyright -p pyrightconfig.json

venv/activate:
- @echo "creating a new venv..."
- rm -rf venv __pycache__
- python3 -m venv venv
- venv/bin/pip install -r requirements.txt
- venv/bin/pip install pyright
- touch $@
