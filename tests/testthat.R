Sys.setenv(LITERATURE_RADAR_ROOT = normalizePath("."))
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
testthat::test_dir("tests/testthat", reporter = "summary")
