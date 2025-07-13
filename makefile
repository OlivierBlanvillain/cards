MAKEFLAGS += --no-builtin-rules
.ONESHELL:
.RECIPEPREFIX=-
.PHONY: FORCE

help/page:
- @grep -Po '^[a-z]+(?=:)' makefile | paste -sd"|" | xargs echo make

test: venv/activate FORCE
- venv/bin/pytest . --verbose --capture=no --exitfirst

bench: venv/activate FORCE
- venv/bin/python3 benchmark.py

check: venv/activate FORCE
- venv/bin/pyright -p pyrightconfig.json

venv/activate: requirements.txt
- @echo "creating a new venv..."
- rm -rf venv __pycache__
- python3 -m venv venv
- venv/bin/pip install -r requirements.txt
- touch $@
