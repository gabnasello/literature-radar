root <- normalizePath(".")
Sys.setenv(LITERATURE_RADAR_ROOT = root)

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(yaml)
})

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

sample_size <- suppressWarnings(as.integer(Sys.getenv("BENCHMARK_SAMPLE_SIZE", unset = "")))
seed <- suppressWarnings(as.integer(Sys.getenv("BENCHMARK_SEED", unset = "")))
if (is.na(sample_size)) sample_size <- NULL
if (is.na(seed)) seed <- NULL

run_llm_benchmark(sample_size = sample_size, seed = seed)
