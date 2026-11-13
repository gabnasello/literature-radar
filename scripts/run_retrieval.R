args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(".")
Sys.setenv(LITERATURE_RADAR_ROOT = root)

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(tibble)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(yaml)
  library(purrr)
  library(xml2)
  library(glue)
  library(cli)
})

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

email <- Sys.getenv("NCBI_EMAIL", unset = "")


# Optional explicit IDs make debugging/manual partial runs straightforward.
ids <- if (length(args)) args else NULL
run_retrieval(journal_ids = ids)
