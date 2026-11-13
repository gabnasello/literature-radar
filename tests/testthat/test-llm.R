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

test_that("JSON model responses are parsed", {
  response <- parse_summary_response('{"summary":"A concise summary","key_findings":["Finding one"]}')
  expect_true(response$valid_json)
  expect_equal(response$parsed$summary, "A concise summary")
})

test_that("invalid model JSON is identified", {
  response <- parse_summary_response("This is not JSON.")
  expect_false(response$valid_json)
})
