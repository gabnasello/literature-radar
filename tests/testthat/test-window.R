test_that("first retrieval uses configurable initial backfill", {
  w <- window_for_journal(list(id = "x"), list(), as.Date("2026-11-12"), 30, 2)
  expect_equal(w$start_iso, "2026-10-13")
  expect_equal(w$end_iso, "2026-11-12")
})

test_that("subsequent windows overlap the previous successful end", {
  entry <- list(last_successful_end_date = "2026-11-01")
  w <- window_for_journal(list(id = "x"), entry, as.Date("2026-11-12"), 30, 2)
  expect_equal(w$start_iso, "2026-10-31")
  expect_equal(w$end_iso, "2026-11-12")
})

test_that("paper merge deduplicates by PMID", {
  existing <- list(list(pmid = "1", title = "Old"))
  incoming <- list(list(pmid = "1", title = "Duplicate"), list(pmid = "2", title = "New"))
  merged <- merge_papers(existing, incoming)
  expect_equal(vapply(merged, `[[`, character(1), "pmid"), c("1", "2"))
})
