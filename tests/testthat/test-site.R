test_that("empty and placeholder summary fields are suppressed", {
  expect_equal(summary_values(list(study_type = "Not specified in abstract"), "study_type"), character())
  expect_equal(summary_values(list(limitations = "Not returned."), "limitations"), character())
  expect_equal(summary_values(list(key_findings = list()), "key_findings"), character())
  expect_equal(summary_values(list(summary = "A real finding."), "summary"), "A real finding.")
})

test_that("site data joins paper records, retrieval reports, and successful summaries", {
  root <- tempfile("literature-radar-site-")
  dir.create(file.path(root, "data", "papers"), recursive = TRUE)
  dir.create(file.path(root, "data", "benchmarks", "run-1"), recursive = TRUE)
  dir.create(file.path(root, "reports", "test-journal"), recursive = TRUE)
  dir.create(file.path(root, "config"), recursive = TRUE)

  writeLines(
    "journals:\n  - id: test-journal\n    name: Test Journal\n    enabled: true\n",
    file.path(root, "config", "journals.yml")
  )

  article <- list(
    pmid = "123",
    title = "A test paper",
    abstract = "A test abstract.",
    journal = "Test Journal",
    publication_date = "2026",
    pubmed_url = "https://pubmed.ncbi.nlm.nih.gov/123/"
  )
  jsonlite::write_json(
    list(article),
    file.path(root, "data", "papers", "papers.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  report <- list(
    report_id = "test-report",
    generated_at_utc = "2026-11-13T10:00:00Z",
    journal = list(id = "test-journal", name = "Test Journal"),
    retrieval_window = list(query_start_inclusive = "2026-10-14", end_inclusive = "2026-11-13"),
    search = list(new_count = 1, fetched_count = 1),
    articles = list(article)
  )
  jsonlite::write_json(
    report,
    file.path(root, "reports", "test-journal", "test-report.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  benchmark <- list(
    generated_at_utc = "2026-11-13T11:00:00Z",
    papers = list(list(
      article = list(pmid = "123"),
      ollama = list(
        status = "ok",
        model = "qwen3:4b",
        parsed = list(summary = "A generated summary.")
      )
    ))
  )
  jsonlite::write_json(
    benchmark,
    file.path(root, "data", "benchmarks", "run-1", "benchmark.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  site <- load_literature_site_data(root)
  expect_equal(site$paper_count, 1L)
  expect_equal(site$report_count, 1L)
  expect_equal(site$summary_count, 1L)
  expect_equal(site$papers[[1]]$journal_name, "Test Journal")
  expect_equal(site$papers[[1]]$summary$parsed$summary, "A generated summary.")
  expect_equal(site$journals[[1]]$latest_report$report_id, "test-report")
})
