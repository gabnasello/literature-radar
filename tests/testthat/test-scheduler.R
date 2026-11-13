test_that("scheduler selects journals with oldest or missing report first", {
  journals <- list(
    list(id = "a", name = "A", enabled = TRUE),
    list(id = "b", name = "B", enabled = TRUE),
    list(id = "c", name = "C", enabled = TRUE),
    list(id = "d", name = "D", enabled = TRUE)
  )
  state <- list(a = list(last_successful_report_at = "2026-11-01T00:00:00Z"),
                b = list(last_successful_report_at = "2026-11-02T00:00:00Z"))
  selected <- select_journals(journals, state, 3)
  expect_equal(vapply(selected, `[[`, character(1), "id"), c("c", "d", "a"))
})

test_that("disabled journals are never selected", {
  journals <- list(list(id = "a", name = "A", enabled = FALSE))
  expect_length(select_journals(journals, list(), 3), 0)
})

test_that("scheduler rejects invalid batch sizes", {
  expect_error(select_journals(list(list(id = "a", enabled = TRUE)), list(), 0))
})
