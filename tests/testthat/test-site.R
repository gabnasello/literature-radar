test_that("empty and placeholder summary fields are suppressed", {
  expect_equal(summary_values(list(study_type = "Not specified in abstract"), "study_type"), character())
  expect_equal(summary_values(list(limitations = "Not returned."), "limitations"), character())
  expect_equal(summary_values(list(key_findings = list()), "key_findings"), character())
  expect_equal(summary_values(list(summary = "A real finding."), "summary"), "A real finding.")
})

test_that("site data joins papers, immutable reports, and successful summaries", {
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
  expect_equal(site$reports[[1]]$article_count, 1L)
  expect_equal(site$reports[[1]]$summary_count, 1L)
  expect_equal(site$reports[[1]]$articles[[1]]$summary$parsed$summary, "A generated summary.")
  expect_equal(site$journals[[1]]$latest_report$report_id, "test-report")
  expect_length(site$journals[[1]]$reports, 1L)

  # Removing a journal from the monitoring configuration must not remove its archive.
  writeLines("journals: []\n", file.path(root, "config", "journals.yml"))
  archived_site <- load_literature_site_data(root)
  expect_length(archived_site$journals, 1L)
  expect_equal(archived_site$journals[[1]]$id, "test-journal")
  expect_false(archived_site$journals[[1]]$configured)
  expect_false(archived_site$journals[[1]]$enabled)
  expect_equal(archived_site$journals[[1]]$latest_report$report_id, "test-report")
})

test_that("summarization queue excludes only successfully summarized papers", {
  records <- list(
    list(pmid = "100", title = "Already done", journal_id = "a", report_id = "update-a"),
    list(pmid = "200", title = "Needs summary", journal_id = "a", report_id = "update-a"),
    list(pmid = "300", title = "Failed before", journal_id = "b", report_id = "update-b"),
    list(pmid = "400", title = "Another paper", journal_id = "c", report_id = "update-c")
  )
  pending <- pending_summarization_articles(records, completed_pmids = "100")
  expect_equal(vapply(pending[1:3], function(x) x$report_id, character(1)),
               c("update-a", "update-b", "update-c"))
  expect_equal(vapply(pending, function(x) x$pmid, character(1)), c("200", "300", "400"))
  expect_length(pending_summarization_articles(list(), character()), 0L)
})
