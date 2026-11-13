# Data preparation and formatting helpers for the Quarto website ------------

site_text <- function(x, default = "") {
  if (is.null(x) || !length(x)) return(default)
  value <- as.character(x[[1]])
  if (is.na(value) || !nzchar(trimws(value))) default else value
}

site_read_json <- function(path, default = list()) {
  if (!file.exists(path)) return(default)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

site_clean_values <- function(value) {
  if (is.null(value) || !length(value)) return(character())
  if (is.list(value)) value <- unlist(value, recursive = TRUE, use.names = FALSE)
  values <- trimws(as.character(value))
  values <- values[!is.na(values) & nzchar(values)]
  placeholders <- grepl(
    "^(not returned|not specified( in abstract)?|not stated|not reported|not available|n/?a|none|unknown)([[:space:].,;:]|$)",
    values,
    ignore.case = TRUE
  )
  values[!placeholders]
}

summary_values <- function(parsed, field) {
  if (is.null(parsed) || is.null(parsed[[field]])) return(character())
  site_clean_values(parsed[[field]])
}

site_html_escape <- function(x) {
  x <- site_text(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

site_safe_url <- function(x) {
  x <- site_text(x)
  if (grepl("^https?://", x, ignore.case = TRUE)) x else ""
}

load_literature_site_data <- function(root = ".") {
  paper_index_path <- file.path(root, "data", "papers", "papers.json")
  paper_records <- site_read_json(paper_index_path, default = list())

  report_paths <- sort(list.files(
    file.path(root, "reports"),
    pattern = "\\.json$",
    recursive = TRUE,
    full.names = TRUE
  ))
  article_metadata <- list()
  report_records <- list()

  for (path in report_paths) {
    report <- site_read_json(path)
    journal_id <- site_text(report$journal$id, basename(dirname(path)))
    journal_name <- site_text(report$journal$name, journal_id)
    generated <- site_text(report$generated_at_utc)
    window <- report$retrieval_window
    search <- report$search
    report_records[[length(report_records) + 1L]] <- list(
      report_id = site_text(report$report_id, basename(path)),
      journal_id = journal_id,
      journal_name = journal_name,
      generated_at_utc = generated,
      start_date = site_text(window$query_start_inclusive),
      end_date = site_text(window$end_inclusive),
      new_count = if (is.null(search$new_count)) 0L else as.integer(search$new_count),
      fetched_count = if (is.null(search$fetched_count)) 0L else as.integer(search$fetched_count)
    )

    for (article in report$articles) {
      pmid <- site_text(article$pmid)
      if (!nzchar(pmid)) next
      article_metadata[[pmid]] <- list(
        journal_id = journal_id,
        journal_name = journal_name,
        retrieval_end_date = site_text(window$end_inclusive)
      )
    }
  }

  latest_summaries <- list()
  benchmark_paths <- sort(list.files(
    file.path(root, "data", "benchmarks"),
    pattern = "benchmark\\.json$",
    recursive = TRUE,
    full.names = TRUE
  ))
  for (path in benchmark_paths) {
    benchmark <- site_read_json(path)
    generated <- site_text(benchmark$generated_at_utc, basename(dirname(path)))
    for (paper in benchmark$papers) {
      pmid <- site_text(paper$article$pmid)
      output <- paper$ollama
      if (!nzchar(pmid) || is.null(output) || !identical(output$status, "ok") ||
          is.null(output$parsed)) next
      previous <- latest_summaries[[pmid]]
      if (is.null(previous) || generated >= previous$generated_at_utc) {
        latest_summaries[[pmid]] <- list(
          parsed = output$parsed,
          model = site_text(output$model),
          generated_at_utc = generated
        )
      }
    }
  }

  config_path <- file.path(root, "config", "journals.yml")
  config <- if (file.exists(config_path)) yaml::read_yaml(config_path) else list(journals = list())
  configured <- config$journals

  normalize_journal_name <- function(x) {
    tolower(gsub("[^a-z0-9]", "", site_text(x)))
  }
  resolve_journal <- function(paper, metadata) {
    id <- site_text(metadata$journal_id)
    raw_name <- site_text(metadata$journal_name, site_text(paper$journal, "Unknown journal"))
    if (nzchar(id)) {
      match <- Filter(function(x) identical(site_text(x$id), id), configured)
      canonical <- if (length(match)) site_text(match[[1]]$name, raw_name) else raw_name
      return(list(id = id, name = canonical))
    }
    raw_normalized <- normalize_journal_name(raw_name)
    matches <- Filter(function(x) {
      configured_name <- normalize_journal_name(x$name)
      nzchar(configured_name) && startsWith(raw_normalized, configured_name)
    }, configured)
    if (length(matches)) {
      lengths <- vapply(matches, function(x) nchar(normalize_journal_name(x$name)), integer(1))
      best <- matches[[which.max(lengths)]]
      return(list(id = site_text(best$id), name = site_text(best$name, raw_name)))
    }
    list(
      id = gsub("[^a-z0-9]+", "-", tolower(raw_name)),
      name = raw_name
    )
  }

  # Reports are also a fallback source if the consolidated paper index is absent.
  if (!length(paper_records)) {
    for (path in report_paths) {
      report <- site_read_json(path)
      if (length(report$articles)) paper_records <- c(paper_records, report$articles)
    }
  }

  seen <- character()
  papers <- list()
  for (paper in paper_records) {
    pmid <- site_text(paper$pmid)
    if (!nzchar(pmid) || pmid %in% seen) next
    seen <- c(seen, pmid)

    metadata <- article_metadata[[pmid]]
    if (is.null(metadata)) metadata <- list()
    journal <- resolve_journal(paper, metadata)
    paper$pmid <- pmid
    paper$journal_id <- journal$id
    paper$journal_name <- journal$name
    paper$pubmed_url <- site_text(paper$pubmed_url, paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/"))
    paper$doi_url <- site_text(paper$doi_url)
    paper$retrieval_end_date <- site_text(metadata$retrieval_end_date)
    paper$summary <- latest_summaries[[pmid]]
    papers[[length(papers) + 1L]] <- paper
  }

  if (length(papers)) {
    journal_names <- vapply(papers, function(x) x$journal_name, character(1))
    years <- vapply(papers, function(x) {
      year <- gsub("[^0-9]", "", site_text(x$publication_date))
      if (!nzchar(year)) "0000" else year
    }, character(1))
    titles <- vapply(papers, function(x) site_text(x$title), character(1))
    papers <- papers[order(journal_names, -as.integer(years), titles)]
  }

  if (length(report_records)) {
    generated <- vapply(report_records, function(x) x$generated_at_utc, character(1))
    report_records <- report_records[order(generated, decreasing = TRUE)]
  }

  latest_by_journal <- list()
  for (report in report_records) {
    id <- report$journal_id
    if (is.null(latest_by_journal[[id]])) latest_by_journal[[id]] <- report
  }

  configured_journals <- lapply(config$journals, function(journal) {
    id <- site_text(journal$id)
    list(
      id = id,
      name = site_text(journal$name, id),
      enabled = isTRUE(journal$enabled),
      latest_report = latest_by_journal[[id]]
    )
  })

  latest_update <- if (length(report_records)) {
    site_text(report_records[[1]]$generated_at_utc)
  } else ""

  list(
    papers = papers,
    reports = report_records,
    journals = configured_journals,
    paper_count = length(papers),
    report_count = length(report_records),
    summary_count = sum(vapply(papers, function(x) !is.null(x$summary), logical(1))),
    latest_update = latest_update
  )
}
