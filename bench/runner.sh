#!/usr/bin/env sh
export BENCHMARKS_RUNNER=TRUE
export BENCH_LIB=exercises
exec dune exec -- ./bench/bench.exe -fork -run-without-cross-library-inlining -quota 2 "$@"