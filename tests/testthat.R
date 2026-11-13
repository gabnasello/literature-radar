source("R/config.R")
source("R/scheduler.R")
source("R/state.R")
testthat::test_dir("tests/testthat", reporter = "summary")
