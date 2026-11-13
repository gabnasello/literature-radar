# Data preparation and rendering helpers for the Quarto website -----------

site_text <- function(x, default = "") {
  if (is.null(x) || !length(x)) return(default)
  value <- as.character(x[[1]])
  if (!length(value) || is.na(value[[1]]) || !nzchar(trimws(value[[1]]))) return(default)
  value[[1]]
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

site_has_summary <- function(article) {
  !is.null(article$summary) &&
    !is.null(article$summary$parsed) &&
    length(summary_values(article$summary$parsed, "summary")) > 0L
}

site_summary_preview <- function(x, max_words = 32L) {
  value <- paste(site_clean_values(x), collapse = " ")
  if (!nzchar(value)) return("")
  words <- strsplit(value, "[[:space:]]+")[[1]]
  if (length(words) <= max_words) return(value)
  paste0(paste(words[seq_len(max_words)], collapse = " "), "…")
}

site_sort_report_articles <- function(articles) {
  if (length(articles) < 2L) return(articles)
  priority <- vapply(articles, function(article) {
    if (site_has_summary(article)) 0L else if (site_has_abstract(article)) 1L else 2L
  }, integer(1))
  articles[order(priority, seq_along(articles))]
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

site_timestamp_order <- function(x) {
  x <- site_text(x)
  if (!nzchar(x)) return(0)
  parsed <- suppressWarnings(as.POSIXct(
    x,
    tz = "UTC",
    tryFormats = c(
      "%Y-%m-%d %H:%M:%S UTC",
      "%Y-%m-%dT%H:%M:%OSZ",
      "%Y-%m-%dT%H:%M:%SZ",
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d"
    )
  ))
  if (is.na(parsed)) 0 else as.numeric(parsed)
}

site_format_timestamp <- function(x) {
  x <- site_text(x)
  if (!nzchar(x)) return("Date unavailable")
  value <- site_timestamp_order(x)
  if (value <= 0) return(x)
  format(as.POSIXct(value, origin = "1970-01-01", tz = "UTC"),
         "%d %b %Y, %H:%M UTC", tz = "UTC")
}

site_format_date <- function(x) {
  x <- site_text(x)
  if (!nzchar(x)) return("Date unavailable")
  parsed <- suppressWarnings(as.Date(x, tryFormats = c("%Y-%m-%d", "%Y/%m/%d")))
  if (is.na(parsed)) return(x)
  format(parsed, "%d %b %Y")
}

site_slug <- function(x) {
  gsub("(^-|-$)", "", gsub("[^a-z0-9]+", "-", tolower(site_text(x))))
}

site_has_abstract <- function(article) {
  abstract <- site_text(article$abstract)
  nzchar(abstract) && !toupper(trimws(abstract)) %in% c("NA", "N/A", "NONE")
}

site_read_summary_files <- function(root) {
  paths <- c(
    sort(list.files(
      file.path(root, "data", "benchmarks"),
      pattern = "benchmark\\.json$",
      recursive = TRUE,
      full.names = TRUE
    )),
    sort(list.files(
      file.path(root, "data", "summaries"),
      pattern = "summaries\\.json$",
      recursive = TRUE,
      full.names = TRUE
    ))
  )
  latest <- list()

  for (path in paths) {
    result <- site_read_json(path)
    generated <- site_text(result$generated_at_utc, basename(dirname(path)))
    for (paper in result$papers) {
      pmid <- site_text(paper$article$pmid)
      output <- paper$ollama
      if (!nzchar(pmid) || is.null(output) ||
          !identical(site_text(output$status), "ok") ||
          is.null(output$parsed)) next

      previous <- latest[[pmid]]
      if (is.null(previous) ||
          site_timestamp_order(generated) >= site_timestamp_order(previous$generated_at_utc)) {
        latest[[pmid]] <- list(
          parsed = output$parsed,
          model = site_text(output$model),
          generated_at_utc = generated
        )
      }
    }
  }
  latest
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
  report_records <- list()
  article_metadata <- list()

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
      fetched_count = if (is.null(search$fetched_count)) 0L else as.integer(search$fetched_count),
      articles = if (is.null(report$articles)) list() else report$articles
    )
    for (article in report$articles) {
      pmid <- site_text(article$pmid)
      if (nzchar(pmid)) {
        article_metadata[[pmid]] <- list(
          journal_id = journal_id,
          journal_name = journal_name
        )
      }
    }
  }

  if (length(report_records)) {
    report_order <- order(
      vapply(report_records, function(x) site_timestamp_order(x$generated_at_utc), numeric(1)),
      decreasing = TRUE
    )
    report_records <- report_records[report_order]
  }

  latest_summaries <- site_read_summary_files(root)

  # Attach the latest successful summary to every article in its immutable report.
  for (i in seq_along(report_records)) {
    articles <- report_records[[i]]$articles
    for (j in seq_along(articles)) {
      pmid <- site_text(articles[[j]]$pmid)
      articles[[j]]$pmid <- pmid
      articles[[j]]$pubmed_url <- site_text(
        articles[[j]]$pubmed_url,
        if (nzchar(pmid)) paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/") else ""
      )
      articles[[j]]$summary <- latest_summaries[[pmid]]
    }
    articles <- site_sort_report_articles(articles)
    report_records[[i]]$articles <- articles
    report_records[[i]]$article_count <- length(articles)
    report_records[[i]]$summarisable_count <- sum(vapply(articles, site_has_abstract, logical(1)))
    report_records[[i]]$summary_count <- sum(vapply(articles, site_has_summary, logical(1)))
    report_records[[i]]$pending_count <- max(
      0L,
      report_records[[i]]$summarisable_count - report_records[[i]]$summary_count
    )
    summary_times <- vapply(articles, function(article) {
      if (is.null(article$summary)) "" else site_text(article$summary$generated_at_utc)
    }, character(1))
    summary_times <- summary_times[nzchar(summary_times)]
    report_records[[i]]$latest_summary_at <- if (length(summary_times)) {
      summary_times[[which.max(vapply(summary_times, site_timestamp_order, numeric(1)))]]
    } else ""
  }

  # Reports remain the source of truth for historical journal pages, even if a
  # journal is later removed from config/journals.yml or disabled.
  config_path <- file.path(root, "config", "journals.yml")
  config <- if (file.exists(config_path)) yaml::read_yaml(config_path) else list(journals = list())
  configured <- config$journals
  if (is.null(configured)) configured <- list()

  configured_ids <- vapply(configured, function(x) site_text(x$id), character(1))
  historical_ids <- if (length(report_records)) {
    unique(vapply(report_records, function(x) x$journal_id, character(1)))
  } else character()
  all_journal_ids <- c(configured_ids, setdiff(historical_ids, configured_ids))

  configured_journals <- lapply(all_journal_ids, function(id) {
    configured_match <- Filter(function(x) identical(site_text(x$id), id), configured)
    historical_reports <- Filter(function(x) identical(x$journal_id, id), report_records)
    journal_name <- if (length(configured_match)) {
      site_text(configured_match[[1]]$name, id)
    } else if (length(historical_reports)) {
      historical_reports[[1]]$journal_name
    } else {
      id
    }
    enabled <- length(configured_match) > 0L && isTRUE(configured_match[[1]]$enabled)
    list(
      id = id,
      name = journal_name,
      enabled = enabled,
      configured = length(configured_match) > 0L,
      reports = historical_reports,
      latest_report = if (length(historical_reports)) historical_reports[[1]] else NULL
    )
  })

  # Keep the complete indexed-paper count for the overview. The report-specific
  # pages below use their own article records, so each update remains distinct.
  if (!length(paper_records)) {
    for (report in report_records) {
      if (length(report$articles)) paper_records <- c(paper_records, report$articles)
    }
  }

  seen <- character()
  papers <- list()
  for (paper in paper_records) {
    pmid <- site_text(paper$pmid)
    if (!nzchar(pmid) || pmid %in% seen) next
    seen <- c(seen, pmid)
    paper$pmid <- pmid
    metadata <- article_metadata[[pmid]]
    if (!is.null(metadata)) {
      paper$journal_id <- metadata$journal_id
      paper$journal_name <- metadata$journal_name
    } else {
      paper$journal_name <- site_text(paper$journal, "Unknown journal")
    }
    paper$pubmed_url <- site_text(
      paper$pubmed_url,
      paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/")
    )
    paper$summary <- latest_summaries[[pmid]]
    papers[[length(papers) + 1L]] <- paper
  }

  if (length(papers)) {
    years <- vapply(papers, function(x) {
      year <- gsub("[^0-9]", "", site_text(x$publication_date))
      if (!nzchar(year)) "0000" else year
    }, character(1))
    titles <- vapply(papers, function(x) site_text(x$title), character(1))
    journals <- vapply(papers, function(x) site_text(x$journal_name), character(1))
    papers <- papers[order(journals, -as.integer(years), titles)]
  }

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

site_render_paper_card <- function(article) {
  title <- site_html_escape(site_text(article$title, "Untitled paper"))
  pubmed_url <- site_safe_url(article$pubmed_url)
  doi_url <- site_safe_url(article$doi_url)
  publication_date <- site_html_escape(site_format_date(article$publication_date))
  authors <- article$authors
  if (is.list(authors)) authors <- unlist(authors, use.names = FALSE)
  authors <- as.character(authors)
  authors <- authors[!is.na(authors) & nzchar(authors)]
  author_text <- if (!length(authors)) "" else if (length(authors) == 1L) authors[[1]] else paste0(authors[[1]], " et al.")
  abstract <- site_html_escape(site_text(article$abstract))
  summary <- article$summary

  cat('<article class="paper-card">')
  if (nzchar(pubmed_url)) {
    cat(sprintf('<h3><a href="%s" target="_blank" rel="noopener noreferrer">%s</a></h3>',
                site_html_escape(pubmed_url), title))
  } else {
    cat(sprintf("<h3>%s</h3>", title))
  }

  metadata <- c(
    if (nzchar(author_text)) site_html_escape(author_text) else NULL,
    if (nzchar(publication_date) && publication_date != "Date unavailable") publication_date else NULL
  )
  if (length(metadata)) cat(sprintf('<p class="paper-meta">%s</p>', paste(metadata, collapse = " · ")))

  if (site_has_summary(article)) {
    parsed <- summary$parsed
    summary_text <- paste(summary_values(parsed, "summary"), collapse = " ")
    cat(sprintf('<p class="ai-preview">%s</p>', site_html_escape(site_summary_preview(summary_text))))
    cat('<details class="paper-details"><summary>More details</summary><div class="paper-expanded">')
    cat(sprintf('<p class="full-ai-summary">%s</p>', site_html_escape(summary_text)))

    findings <- summary_values(parsed, "key_findings")
    if (length(findings)) {
      cat('<h4>Key findings</h4><ul class="key-findings">')
      for (finding in findings) cat(sprintf("<li>%s</li>", site_html_escape(finding)))
      cat("</ul>")
    }

    study_type <- summary_values(parsed, "study_type")
    if (length(study_type)) {
      cat(sprintf('<p class="detail-line"><strong>Study design:</strong> %s</p>',
                  site_html_escape(paste(study_type, collapse = "; "))))
    }

    scientific_significance <- summary_values(parsed, "scientific_significance")
    if (length(scientific_significance)) {
      cat(sprintf('<p class="detail-line"><strong>Scientific significance:</strong> %s</p>',
                  site_html_escape(paste(scientific_significance, collapse = " "))))
    }

    if (nzchar(abstract)) {
      cat(sprintf('<details class="abstract-details"><summary>Original abstract</summary><p>%s</p></details>', abstract))
    }
    if (nzchar(doi_url)) {
      cat(sprintf('<p class="paper-links"><a href="%s" target="_blank" rel="noopener noreferrer">DOI</a></p>',
                  site_html_escape(doi_url)))
    }
    cat("</div></details>")
  } else if (site_has_abstract(article)) {
    cat('<p class="summary-status pending">AI summary pending</p>')
    cat(sprintf('<details class="abstract-details"><summary>Read original abstract</summary><p>%s</p></details>', abstract))
  } else {
    cat('<p class="summary-status unavailable">No abstract available</p>')
  }
  cat("</article>")
}

site_render_journal_page <- function(site, journal_id) {
  matches <- Filter(function(x) identical(x$id, journal_id), site$journals)
  if (!length(matches)) {
    cat("<p>No update history is available.</p>")
    return(invisible(NULL))
  }
  journal <- matches[[1]]
  cat(sprintf("<h1>%s</h1>", site_html_escape(journal$name)))
  if (isTRUE(journal$enabled)) {
    cat('<p class="journal-status monitoring">Monitoring</p>')
  } else {
    cat('<p class="journal-status inactive">Archived</p>')
  }

  if (!length(journal$reports)) {
    cat("<p>No updates yet.</p>")
    return(invisible(NULL))
  }

  cat("<h2>Updates</h2>")
  for (report in journal$reports) {
    report_slug <- site_slug(report$report_id)
    update_date <- site_format_date(substr(report$generated_at_utc, 1, 10))
    window <- paste(site_format_date(report$start_date), "–", site_format_date(report$end_date))
    summary_progress <- if (report$summarisable_count > 0L) {
      sprintf("%d/%d summarized", report$summary_count, report$summarisable_count)
    } else {
      "No abstracts"
    }
    detail <- sprintf("PubMed Create Date: %s · %d papers · %s",
                      window, report$article_count, summary_progress)
    cat(sprintf(
      '<article class="update-card"><h3><a href="../updates/%s.html">Update · %s</a></h3><p class="update-meta">%s</p></article>',
      site_html_escape(report_slug), site_html_escape(update_date), site_html_escape(detail)
    ))
  }
  invisible(NULL)
}

site_render_update_page <- function(site, report_id) {
  matches <- Filter(function(x) identical(x$report_id, report_id), site$reports)
  if (!length(matches)) {
    cat("<p>This update could not be found.</p>")
    return(invisible(NULL))
  }
  report <- matches[[1]]
  cat(sprintf("<h1>%s</h1>", site_html_escape(report$journal_name)))
  cat(sprintf('<p><a href="../journals/%s.html">← Journal updates</a></p>',
              site_html_escape(site_slug(report$journal_id))))

  summary_progress <- if (report$summarisable_count > 0L) {
    sprintf("%d/%d summarized", report$summary_count, report$summarisable_count)
  } else {
    "No abstracts"
  }
  update_meta <- sprintf(
    "Updated %s · PubMed Create Date %s – %s · %d papers · %s",
    site_format_date(substr(report$generated_at_utc, 1, 10)),
    site_format_date(report$start_date), site_format_date(report$end_date),
    report$article_count, summary_progress
  )
  cat(sprintf('<p class="update-meta">%s</p>', site_html_escape(update_meta)))

  articles <- report$articles
  summarized <- Filter(site_has_summary, articles)
  awaiting <- Filter(function(article) !site_has_summary(article) && site_has_abstract(article), articles)
  no_abstract <- Filter(function(article) !site_has_summary(article) && !site_has_abstract(article), articles)
  mixed <- length(summarized) > 0L && (length(awaiting) + length(no_abstract) > 0L)

  if (length(summarized)) {
    if (mixed) cat("<h2>AI summaries</h2>")
    for (article in summarized) site_render_paper_card(article)
  }
  if (length(awaiting)) {
    cat("<h2>Awaiting AI summary</h2>")
    for (article in awaiting) site_render_paper_card(article)
  }
  if (length(no_abstract)) {
    if (length(awaiting) || mixed || length(summarized)) cat("<h2>No abstract</h2>")
    for (article in no_abstract) site_render_paper_card(article)
  }
  invisible(NULL)
}
