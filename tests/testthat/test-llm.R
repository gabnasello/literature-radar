test_that("summary prompt inserts paper metadata and abstract", {
  template <- paste(
    "Title={{TITLE}}",
    "Journal={{JOURNAL}}",
    "Abstract={{ABSTRACT}}",
    sep = "\n"
  )
  article <- list(
    title = "A test paper",
    journal_name = "Test Journal",
    abstract = "The abstract text."
  )
  prompt <- build_summary_prompt(article, template)
  expect_match(prompt, "Title=A test paper", fixed = TRUE)
  expect_match(prompt, "Journal=Test Journal", fixed = TRUE)
  expect_match(prompt, "The abstract text.", fixed = TRUE)
})

test_that("JSON model responses are parsed, including fenced JSON", {
  response <- parse_summary_response('{"summary":"A concise summary","key_findings":["Finding one"]}')
  expect_true(response$valid_json)
  expect_equal(response$parsed$summary, "A concise summary")

  fence <- intToUtf8(96L)
  fenced_text <- paste0(fence, fence, fence, "json\n", '{"summary":"Fenced"}', "\n", fence, fence, fence)
  fenced <- parse_summary_response(fenced_text)
  expect_true(fenced$valid_json)
  expect_equal(fenced$parsed$summary, "Fenced")
})

test_that("invalid model JSON is identified", {
  response <- parse_summary_response("This is not JSON.")
  expect_false(response$valid_json)
})

test_that("benchmark sampling is reproducible and balanced across journals", {
  records <- list(
    list(pmid = "1", journal_id = "a"),
    list(pmid = "2", journal_id = "a"),
    list(pmid = "3", journal_id = "a"),
    list(pmid = "4", journal_id = "b"),
    list(pmid = "5", journal_id = "b"),
    list(pmid = "6", journal_id = "b")
  )
  first <- sample_benchmark_articles(records, sample_size = 4, seed = 7)
  second <- sample_benchmark_articles(records, sample_size = 4, seed = 7)
  expect_equal(
    vapply(first, function(x) x$pmid, character(1)),
    vapply(second, function(x) x$pmid, character(1))
  )
  counts <- table(vapply(first, function(x) x$journal_id, character(1)))
  expect_equal(as.integer(counts), c(2L, 2L))
})

test_that("summary Markdown omits empty fields and benchmark diagnostics", {
  results <- list(
    list(
      article = list(pmid = "123", journal_name = "Test Journal", title = "Test paper",
                     pubmed_url = "https://pubmed.ncbi.nlm.nih.gov/123/"),
      ollama = list(
        model = "qwen3:4b",
        status = "ok",
        elapsed_seconds = 12.5,
        parsed = list(
          summary = "A concise summary.",
          key_findings = list("Finding one", "Finding two"),
          study_type = "Not specified in abstract",
          relevance_to_biomaterials_and_regenerative_medicine = "Relevant.",
          limitations_or_unknowns = "Not returned."
        ),
        response_text = "",
        error = NULL
      )
    )
  )

  metrics <- benchmark_metrics(results)
  expect_equal(nrow(metrics), 1L)
  expect_equal(metrics$provider, "ollama")
  expect_equal(metrics$model, "qwen3:4b")

  output <- tempfile(fileext = ".md")
  write_benchmark_markdown(results, "test-run", "paper_summary_v1", output)
  report <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(report, "A concise summary.", fixed = TRUE)
  expect_match(report, "- Finding one", fixed = TRUE)
  expect_match(report, "**Relevance**", fixed = TRUE)
  expect_false(grepl("OLLAMA", report, fixed = TRUE))
  expect_false(grepl("Status:", report, fixed = TRUE))
  expect_false(grepl("Elapsed:", report, fixed = TRUE))
  expect_false(grepl("Study type", report, fixed = TRUE))
  expect_false(grepl("Limitations / unknowns", report, fixed = TRUE))
})
