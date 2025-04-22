.PHONY: build test clean fmt

build:
	dune build

test:
	dune runtest

clean:
	dune clean

fmt:
	dune build @fmt --auto-promote
