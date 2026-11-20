Sys.setenv(LITERATURE_RADAR_ROOT = normalizePath("."))
suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(yaml); library(xml2)
})
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
run_keyword_retrieval()
