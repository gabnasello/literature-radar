# Reproducible single-provider benchmark using local Ollama ----------------

read_benchmark_articles <- function(root = project_root()) {
  report_dir <- file.path(root, "reports")
  paths <- sort(list.files(report_dir, pattern = "\\.json$", recursive = TRUE, full.names = TRUE))
  if (!length(paths)) stop("No retrieval reports found under reports/.", call. = FALSE)

  records <- list()
  for (path in paths) {
    report <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    if (is.null(report$articles) || !length(report$articles)) next
    journal_id <- if (is.null(report$journal$id)) basename(dirname(path)) else report$journal$id
    journal_name <- if (is.null(report$journal$name)) journal_id else report$journal$name
    for (article in report$articles) {
      pmid <- if (is.null(article$pmid)) "" else as.character(article$pmid)
      abstract <- if (is.null(article$abstract)) "" else as.character(article$abstract)
      if (!nzchar(pmid) || !nzchar(trimws(abstract)) || trimws(abstract) %in% c("NA", "N/A")) next
      records[[length(records) + 1L]] <- list(
        pmid = pmid,
        title = if (is.null(article$title)) "" else as.character(article$title),
        abstract = abstract,
        doi = if (is.null(article$doi)) "" else as.character(article$doi),
        doi_url = if (is.null(article$doi_url)) "" else as.character(article$doi_url),
        pubmed_url = if (is.null(article$pubmed_url)) "" else as.character(article$pubmed_url),
        journal_id = as.character(journal_id),
        journal_name = as.character(journal_name),
        report_id = if (is.null(report$report_id)) basename(path) else as.character(report$report_id)
      )
    }
  }
  if (!length(records)) stop("No articles with abstracts were found in retrieval reports.", call. = FALSE)

  pmids <- vapply(records, function(x) x$pmid, character(1))
  records <- records[!duplicated(pmids)]
  ord <- order(
    vapply(records, function(x) x$journal_id, character(1)),
    vapply(records, function(x) x$pmid, character(1))
  )
  records[ord]
}

sample_benchmark_articles <- function(records, sample_size = 12L, seed = 20261113L) {
  sample_size <- as.integer(sample_size)[1]
  seed <- as.integer(seed)[1]
  if (is.na(sample_size) || sample_size < 1L) stop("sample_size must be a positive integer.", call. = FALSE)
  if (is.na(seed)) stop("seed must be an integer.", call. = FALSE)
  if (!length(records)) stop("No candidate articles available for sampling.", call. = FALSE)

  n <- min(sample_size, length(records))
  journal_ids <- sort(unique(vapply(records, function(x) x$journal_id, character(1))))
  set.seed(seed)
  selected <- integer()
  while (length(selected) < n) {
    for (journal_id in journal_ids) {
      if (length(selected) >= n) break
      candidates <- which(vapply(records, function(x) x$journal_id == journal_id, logical(1)))
      candidates <- setdiff(candidates, selected)
      if (length(candidates)) selected <- c(selected, candidates[sample.int(length(candidates), 1L)])
    }
  }
  records[selected]
}

safe_ollama_call <- function(prompt, config) {
  tryCatch(
    call_ollama(
      prompt = prompt,
      model = config$model,
      base_url = config$base_url,
      temperature = config$temperature,
      num_ctx = config$num_ctx,
      num_predict = config$num_predict,
      timeout_seconds = config$timeout_seconds
    ),
    error = function(e) {
      list(
        provider = "ollama",
        model = config$model,
        status = "error",
        response_text = "",
        parsed = NULL,
        elapsed_seconds = NA_real_,
        input_tokens = NA_real_,
        output_tokens = NA_real_,
        total_tokens = NA_real_,
        error = conditionMessage(e)
      )
    }
  )
}

benchmark_metrics <- function(results) {
  rows <- lapply(results, function(paper) {
    x <- paper$ollama
    numeric_value <- function(name) {
      value <- x[[name]]
      if (is.null(value) || !length(value) || is.na(value[[1]])) return(NA_real_)
      as.numeric(value[[1]])
    }
    text_value <- function(object, name, default = "") {
      value <- object[[name]]
      if (is.null(value) || !length(value) || is.na(value[[1]])) return(default)
      as.character(value[[1]])
    }

    data.frame(
      pmid = text_value(paper$article, "pmid", NA_character_),
      journal = text_value(paper$article, "journal_name", ""),
      provider = "ollama",
      model = text_value(x, "model", ""),
      status = text_value(x, "status", "unknown"),
      elapsed_seconds = numeric_value("elapsed_seconds"),
      input_tokens = numeric_value("input_tokens"),
      output_tokens = numeric_value("output_tokens"),
      total_tokens = numeric_value("total_tokens"),
      error = text_value(x, "error", ""),
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

summary_field <- function(parsed, field, fallback = NULL) {
  values <- summary_values(parsed, field)
  if (!length(values)) return(fallback)
  paste(values, collapse = "; ")
}

write_benchmark_markdown <- function(results, run_id, prompt_version, output_path) {
  lines <- c("# Paper summaries", "")
  for (paper in results) {
    article <- paper$article
    title <- if (is.null(article$title) || !nzchar(article$title)) "Untitled paper" else article$title
    journal <- if (is.null(article$journal_name)) "Unknown journal" else article$journal_name
    pmid <- if (is.null(article$pmid)) "" else article$pmid
    metadata <- paste0("PMID: ", pmid, " | Journal: ", journal)

    lines <- c(lines, paste0("## ", title), "", metadata)
    if (!is.null(article$pubmed_url) && nzchar(article$pubmed_url)) {
      lines <- c(lines, paste0("PubMed: ", article$pubmed_url))
    }
    lines <- c(lines, "")

    x <- paper$ollama
    if (!is.null(x$parsed)) {
      summary <- summary_field(x$parsed, "summary", x$response_text)
      if (!is.null(summary) && nzchar(trimws(summary))) {
        lines <- c(lines, "**Summary**", "", summary, "")
      }

      findings <- summary_values(x$parsed, "key_findings")
      if (length(findings)) {
        lines <- c(lines, "**Key findings**", "", paste0("- ", findings), "")
      }

      fields <- list(
        "Study type" = "study_type",
        "Relevance" = "relevance_to_biomaterials_and_regenerative_medicine",
        "Limitations / unknowns" = "limitations_or_unknowns"
      )
      for (heading in names(fields)) {
        value <- summary_field(x$parsed, fields[[heading]])
        if (!is.null(value) && nzchar(trimws(value))) {
          lines <- c(lines, paste0("**", heading, "**"), "", value, "")
        }
      }
    } else if (!is.null(x$response_text) && nzchar(x$response_text)) {
      lines <- c(lines, x$response_text, "")
    } else {
      error <- if (is.null(x$error)) "No summary was returned." else x$error
      lines <- c(lines, paste0("Summary unavailable: ", error), "")
    }
    lines <- c(lines, "---", "")
  }
  writeLines(lines, output_path, useBytes = TRUE)
}

run_llm_benchmark <- function(
  sample_size = NULL,
  seed = NULL,
  root = project_root(),
  config_path = file.path(root, "config", "llm.yml")
) {
  config <- yaml::read_yaml(config_path)$benchmark
  if (is.null(sample_size)) sample_size <- as.integer(config$sample_size)
  if (is.null(seed)) seed <- as.integer(config$seed)
  articles <- read_benchmark_articles(root)
  selected <- sample_benchmark_articles(articles, sample_size, seed)
  prompt_template <- read_summary_prompt_template(file.path(
    root, "prompts", paste0(config$prompt_version, ".txt")
  ))

  run_id <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  paper_results <- list()
  for (i in seq_along(selected)) {
    article <- selected[[i]]
    prompt <- build_summary_prompt(article, prompt_template)
    message(sprintf("[%d/%d] Benchmarking PMID %s: %s", i, length(selected), article$pmid, article$title))

    ollama_result <- safe_ollama_call(prompt, config$ollama)
    paper_results[[i]] <- list(
      article = article,
      ollama = ollama_result
    )
    if (i < length(selected)) Sys.sleep(1)
  }

  output_dir <- file.path(root, "data", "benchmarks", run_id)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  metrics <- benchmark_metrics(paper_results)
  result_file <- file.path(output_dir, "benchmark.json")
  metrics_file <- file.path(output_dir, "metrics.csv")
  markdown_file <- file.path(output_dir, "summaries.md")

  result <- list(
    schema_version = 2L,
    run_id = run_id,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    prompt_version = config$prompt_version,
    prompt_template = prompt_template,
    requested_sample_size = as.integer(sample_size),
    actual_sample_size = length(selected),
    sampling_seed = as.integer(seed),
    models = list(ollama = config$ollama$model),
    papers = paper_results
  )
  write_json_atomic(result, result_file)
  utils::write.csv(metrics, metrics_file, row.names = FALSE, na = "")
  write_benchmark_markdown(paper_results, run_id, config$prompt_version, markdown_file)

  failed <- sum(metrics$status != "ok")
  message("Benchmark artifacts written to: ", output_dir)
  message("Successful outputs: ", sum(metrics$status == "ok"), " / ", nrow(metrics))

  if (failed > 0L) {
    stop(failed, " Ollama call(s) failed or returned invalid JSON. Partial results were saved.", call. = FALSE)
  }
  invisible(list(result = result, metrics = metrics, output_dir = output_dir))
}
