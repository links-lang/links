#!/bin/bash

./_build/default/tests/unit/lens/benchmark/links_lens_benchmark.exe -runner sequential -database-args "links:localhost:5432:links:links" $@ \
    && julia tests/unit/lens/benchmark/plot_benchmarks.jl
