root <- normalizePath(".")
Sys.setenv(LITERATURE_RADAR_ROOT = root)

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(yaml)
})

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) {
  source(f)
}

batch_size <- suppressWarnings(as.integer(Sys.getenv("SUMMARIZATION_BATCH_SIZE", unset = "")))
if (is.na(batch_size)) batch_size <- NULL

run_paper_summarization(batch_size = batch_size, root = root)
