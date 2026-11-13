# Incremental summarization queue for all papers in retrieval reports --------

read_completed_summary_pmids <- function(root = project_root()) {
  paths <- c(
    list.files(file.path(root, "data", "benchmarks"), pattern = "benchmark\\.json$", recursive = TRUE, full.names = TRUE),
    list.files(file.path(root, "data", "summaries"), pattern = "summaries\\.json$", recursive = TRUE, full.names = TRUE)
  )
  completed <- character()
  for (path in paths) {
    result <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(result$papers)) next
    for (paper in result$papers) {
      pmid <- if (is.null(paper$article$pmid)) "" else as.character(paper$article$pmid)
      output <- paper$ollama
      if (nzchar(pmid) && !is.null(output) &&
          identical(as.character(output$status), "ok") && !is.null(output$parsed)) {
        completed <- c(completed, pmid)
      }
    }
  }
  unique(completed)
}

pending_summarization_articles <- function(records, completed_pmids = character()) {
  if (!length(records)) return(list())
  Filter(function(article) !article$pmid %in% completed_pmids, records)
}

run_paper_summarization <- function(
  batch_size = NULL,
  root = project_root(),
  config_path = file.path(root, "config", "llm.yml")
) {
  config <- yaml::read_yaml(config_path)$benchmark
  if (is.null(batch_size)) {
    batch_size <- config$summarization_batch_size
    if (is.null(batch_size)) batch_size <- 24L
  }
  batch_size <- suppressWarnings(as.integer(batch_size)[1])
  if (is.na(batch_size) || batch_size < 1L) {
    stop("batch_size must be a positive integer.", call. = FALSE)
  }

  articles <- read_benchmark_articles(root)
  completed <- read_completed_summary_pmids(root)
  pending <- pending_summarization_articles(articles, completed)
  if (!length(pending)) {
    message("No papers are waiting for an AI summary.")
    return(invisible(list(processed = 0L, remaining = 0L, output_dir = NULL)))
  }

  selected <- head(pending, batch_size)
  prompt_template <- read_summary_prompt_template(file.path(
    root, "prompts", paste0(config$prompt_version, ".txt")
  ))
  run_id <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  paper_results <- list()

  for (i in seq_along(selected)) {
    article <- selected[[i]]
    prompt <- build_summary_prompt(article, prompt_template)
    message(sprintf("[%d/%d] Summarizing PMID %s: %s",
                    i, length(selected), article$pmid, article$title))
    paper_results[[i]] <- list(
      article = article,
      ollama = safe_ollama_call(prompt, config$ollama)
    )
    if (i < length(selected)) Sys.sleep(1)
  }

  output_dir <- file.path(root, "data", "summaries", run_id)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  metrics <- benchmark_metrics(paper_results)
  result <- list(
    schema_version = 1L,
    run_id = run_id,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    run_type = "automatic_summarization",
    prompt_version = config$prompt_version,
    prompt_template = prompt_template,
    requested_batch_size = batch_size,
    actual_batch_size = length(selected),
    remaining_after_run = max(0L, length(pending) - length(selected)),
    models = list(ollama = config$ollama$model),
    papers = paper_results
  )

  write_json_atomic(result, file.path(output_dir, "summaries.json"))
  utils::write.csv(metrics, file.path(output_dir, "metrics.csv"), row.names = FALSE, na = "")
  write_benchmark_markdown(
    paper_results, run_id, config$prompt_version,
    file.path(output_dir, "summaries.md")
  )

  successful <- sum(metrics$status == "ok")
  failed <- nrow(metrics) - successful
  message("Summarization artifacts written to: ", output_dir)
  message("Successful summaries: ", successful, " / ", nrow(metrics))
  message("Papers still waiting: ", max(0L, length(pending) - successful))

  if (failed > 0L) {
    stop(
      failed,
      " Ollama call(s) failed or returned invalid JSON. Partial results were saved; failed papers will be retried.",
      call. = FALSE
    )
  }

  invisible(list(
    processed = length(selected),
    successful = successful,
    remaining = max(0L, length(pending) - successful),
    output_dir = output_dir
  ))
}
