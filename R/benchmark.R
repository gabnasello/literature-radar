# Reproducible paired benchmark of local Ollama and Gemini -----------------

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

safe_provider_call <- function(provider, prompt, config, api_key) {
  tryCatch({
    if (provider == "ollama") {
      call_ollama(
        prompt = prompt,
        model = config$model,
        base_url = config$base_url,
        temperature = config$temperature,
        num_ctx = config$num_ctx,
        num_predict = config$num_predict,
        timeout_seconds = config$timeout_seconds
      )
    } else {
      call_gemini(
        prompt = prompt,
        model = config$model,
        api_key = api_key,
        temperature = config$temperature,
        thinking_level = config$thinking_level,
        max_output_tokens = config$max_output_tokens,
        timeout_seconds = config$timeout_seconds,
        max_tries = config$max_tries,
        max_retry_seconds = config$max_retry_seconds
      )
    }
  }, error = function(e) {
    list(
      provider = provider,
      model = config$model,
      status = "error",
      response_text = "",
      parsed = NULL,
      elapsed_seconds = NA_real_,
      input_tokens = NA_real_,
      output_tokens = NA_real_,
      thoughts_tokens = NA_real_,
      total_tokens = NA_real_,
      error = conditionMessage(e)
    )
  })
}

benchmark_metrics <- function(results, config) {
  rows <- list()
  for (paper in results) {
    for (provider in c("ollama", "gemini")) {
      x <- paper[[provider]]
      cost <- NA_real_
      if (provider == "gemini" && is.finite(x$input_tokens) && is.finite(x$output_tokens)) {
        thoughts <- if (is.finite(x$thoughts_tokens)) x$thoughts_tokens else 0
        cost <- (
          x$input_tokens * config$gemini$paid_input_usd_per_million +
          (x$output_tokens + thoughts) * config$gemini$paid_output_usd_per_million
        ) / 1e6
      }
      rows[[length(rows) + 1L]] <- data.frame(
        pmid = paper$article$pmid,
        journal = paper$article$journal_name,
        provider = provider,
        model = x$model,
        status = x$status,
        elapsed_seconds = x$elapsed_seconds,
        input_tokens = x$input_tokens,
        output_tokens = x$output_tokens,
        thoughts_tokens = x$thoughts_tokens,
        total_tokens = x$total_tokens,
        estimated_paid_tier_cost_usd = cost,
        error = if (is.null(x$error)) "" else x$error,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

summary_field <- function(parsed, field, fallback = "") {
  if (is.null(parsed) || is.null(parsed[[field]])) return(fallback)
  value <- parsed[[field]]
  if (is.character(value)) return(paste(value, collapse = "; "))
  if (is.list(value)) return(paste(unlist(value, use.names = FALSE), collapse = "; "))
  as.character(value)
}

write_benchmark_markdown <- function(results, run_id, prompt_version, output_path) {
  lines <- c(
    paste0("# LLM benchmark: ", run_id),
    "",
    paste0("Prompt version: ", prompt_version),
    "",
    "Each paper was sent to both providers with the same prompt and abstract.",
    ""
  )
  for (paper in results) {
    article <- paper$article
    lines <- c(
      lines,
      paste0("## ", article$title),
      "",
      paste0("PMID: ", article$pmid, " | Journal: ", article$journal_name),
      if (nzchar(article$pubmed_url)) paste0("PubMed: ", article$pubmed_url) else NULL,
      ""
    )
    for (provider in c("ollama", "gemini")) {
      x <- paper[[provider]]
      elapsed <- if (is.finite(x$elapsed_seconds)) paste0(round(x$elapsed_seconds, 2), " s") else "n/a"
      lines <- c(
        lines,
        paste0("### ", toupper(provider), " (", x$model, ")"),
        "",
        paste0("Status: ", x$status, " | Elapsed: ", elapsed),
        ""
      )
      if (!is.null(x$parsed)) {
        lines <- c(
          lines,
          "**Summary**",
          "",
          summary_field(x$parsed, "summary", x$response_text),
          "",
          "**Key findings**",
          "",
          summary_field(x$parsed, "key_findings", "Not returned."),
          "",
          "**Study type**",
          "",
          summary_field(x$parsed, "study_type", "Not returned."),
          "",
          "**Relevance**",
          "",
          summary_field(x$parsed, "relevance_to_biomaterials_and_regenerative_medicine", "Not returned."),
          "",
          "**Limitations / unknowns**",
          "",
          summary_field(x$parsed, "limitations_or_unknowns", "Not returned."),
          ""
        )
      } else if (nzchar(x$response_text)) {
        lines <- c(lines, x$response_text, "")
      } else {
        lines <- c(lines, paste0("Error: ", x$error), "")
      }
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
  api_key <- Sys.getenv("GEMINI_API_KEY", unset = "")
  if (!nzchar(api_key)) {
    stop("GEMINI_API_KEY is missing. Add it under repository Settings > Secrets and variables > Actions.", call. = FALSE)
  }

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

    ollama_result <- safe_provider_call("ollama", prompt, config$ollama, api_key)
    gemini_result <- safe_provider_call("gemini", prompt, config$gemini, api_key)
    paper_results[[i]] <- list(
      article = article,
      ollama = ollama_result,
      gemini = gemini_result
    )
    if (i < length(selected)) Sys.sleep(1)
  }

  output_dir <- file.path(root, "data", "benchmarks", run_id)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  metrics <- benchmark_metrics(paper_results, config)
  result_file <- file.path(output_dir, "benchmark.json")
  metrics_file <- file.path(output_dir, "metrics.csv")
  markdown_file <- file.path(output_dir, "comparison.md")

  result <- list(
    schema_version = 1L,
    run_id = run_id,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    prompt_version = config$prompt_version,
    prompt_template = prompt_template,
    requested_sample_size = as.integer(sample_size),
    actual_sample_size = length(selected),
    sampling_seed = as.integer(seed),
    models = list(ollama = config$ollama$model, gemini = config$gemini$model),
    gemini_pricing_assumption = list(
      tier = "paid standard tier estimate; actual free-tier use may be free",
      input_usd_per_million = config$gemini$paid_input_usd_per_million,
      output_usd_per_million = config$gemini$paid_output_usd_per_million,
      effective_until = config$gemini$pricing_effective_until
    ),
    papers = paper_results
  )
  write_json_atomic(result, result_file)
  utils::write.csv(metrics, metrics_file, row.names = FALSE, na = "")
  write_benchmark_markdown(paper_results, run_id, config$prompt_version, markdown_file)

  failed <- sum(metrics$status != "ok")
  message("Benchmark artifacts written to: ", output_dir)
  message("Successful outputs: ", sum(metrics$status == "ok"), " / ", nrow(metrics))
  message("Estimated Gemini paid-tier cost (USD): ",
    format(sum(metrics$estimated_paid_tier_cost_usd, na.rm = TRUE), scientific = FALSE, digits = 5))

  if (failed > 0L) {
    stop(failed, " provider call(s) failed or returned invalid JSON. Partial results were saved.", call. = FALSE)
  }
  invisible(list(result = result, metrics = metrics, output_dir = output_dir))
}
