# Data preparation and rendering helpers for the Quarto website -----------

if (!exists("%||%", mode = "function")) `%||%` <- function(x, y) if (is.null(x)) y else x

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

site_is_summarizable <- function(article) {
  abstract <- trimws(site_text(article$abstract))
  title <- trimws(site_text(article$title))
  if (!nzchar(abstract) || toupper(abstract) %in% c("NA", "N/A", "NONE")) return(FALSE)
  if (grepl("^In Other Journals\\.?$", title, ignore.case = TRUE)) return(FALSE)
  types <- tolower(trimws(unlist(article$publication_types, use.names = FALSE)))
  excluded <- c("editorial", "news", "comment", "published erratum",
    "retracted publication", "expression of concern", "interview",
    "biography", "obituary", "personal narrative", "directory",
    "bibliography", "legal case")
  !any(types %in% excluded)
}

site_has_summary <- function(article) {
  site_is_summarizable(article) && !is.null(article$summary) &&
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
    if (site_has_summary(article)) 0L else if (site_is_summarizable(article)) 1L else 2L
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
      "%Y-%m-%d",
      "%Y"
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

  # PubMed sometimes supplies only the publication year (e.g. "2026").
  if (grepl("^[0-9]{4}$", x)) return(x)

  parsed <- tryCatch(
    suppressWarnings(as.Date(x, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))),
    error = function(e) as.Date(NA)
  )
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
          is.null(output$parsed) ||
          !isTRUE(validate_summary_payload(output$parsed)$valid)) next

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
    if (identical(site_text(report$channel), "keyword")) next
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
    report_records[[i]]$summarisable_count <- sum(vapply(articles, function(article) {
      site_is_summarizable(article) && site_has_abstract(article)
    }, logical(1)))
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
    summary_count <- if (length(historical_reports)) {
      sum(vapply(historical_reports, function(x) {
        if (is.null(x$summary_count)) 0L else as.integer(x$summary_count)
      }, integer(1)))
    } else {
      0L
    }
    summary_times <- if (length(historical_reports)) {
      vapply(historical_reports, function(x) site_text(x$latest_summary_at), character(1))
    } else {
      character()
    }
    summary_times <- summary_times[nzchar(summary_times)]
    latest_summary_at <- if (length(summary_times)) {
      summary_times[[which.max(vapply(summary_times, site_timestamp_order, numeric(1)))]]
    } else {
      ""
    }
    list(
      id = id,
      name = journal_name,
      enabled = enabled,
      configured = length(configured_match) > 0L,
      reports = historical_reports,
      latest_report = if (length(historical_reports)) historical_reports[[1]] else NULL,
      summary_count = as.integer(summary_count),
      latest_summary_at = latest_summary_at
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
    keywords = load_keyword_site_data(root),
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

  journal_name <- site_text(article$journal_name, site_text(article$journal))
  metadata <- c(
    if (nzchar(author_text)) site_html_escape(author_text) else NULL,
    if (nzchar(journal_name) && !identical(tolower(journal_name), "unknown journal")) site_html_escape(journal_name) else NULL,
    if (nzchar(publication_date) && publication_date != "Date unavailable") publication_date else NULL
  )
  if (length(metadata)) cat(sprintf('<p class="paper-meta">%s</p>', paste(metadata, collapse = " · ")))

  if (site_has_summary(article)) {
    parsed <- summary$parsed
    scientific_significance <- summary_values(parsed, "scientific_significance")
    if (length(scientific_significance)) {
      cat(sprintf('<p class="ai-significance">%s</p>',
                  site_html_escape(paste(scientific_significance, collapse = " "))))
    }
    cat('<details class="paper-details"><summary>Show more</summary><div class="paper-expanded">')
    summary_text <- paste(summary_values(parsed, "summary"), collapse = " ")
    if (nzchar(summary_text)) {
      cat(sprintf('<p class="full-ai-summary">%s</p>', site_html_escape(summary_text)))
    }

    findings <- summary_values(parsed, "key_findings")
    if (length(findings)) {
      cat('<h4>Key findings</h4><ul class="key-findings">')
      for (finding in findings) cat(sprintf("<li>%s</li>", site_html_escape(finding)))
      cat("</ul>")
    }

    if (nzchar(abstract)) {
      cat(sprintf('<details class="abstract-details"><summary>Original abstract</summary><p>%s</p></details>', abstract))
    }
    if (nzchar(doi_url)) {
      cat(sprintf('<p class="paper-links"><a href="%s" target="_blank" rel="noopener noreferrer">DOI</a></p>',
                  site_html_escape(doi_url)))
    }
    cat("</div></details>")
  } else if (site_is_summarizable(article) && site_has_abstract(article)) {
    cat('<p class="summary-status pending">AI summary pending</p>')
    cat(sprintf('<details class="abstract-details"><summary>Read original abstract</summary><p>%s</p></details>', abstract))
  } else if (!site_has_abstract(article)) {
    cat('<p class="summary-status unavailable">No abstract available</p>')
  } else {
    cat('<p class="summary-status unavailable">No AI summary for this item</p>')
  }
  cat("</article>")
}

site_visible_journals <- function(journals) {
  # List every journal with an update, even when its newest papers are still
  # awaiting summaries or older summaries fail the current quality checks.
  # Only currently monitored journals appear on the homepage. Removed or
  # disabled journals remain available through their historical archive pages.
  visible <- Filter(function(journal) {
    isTRUE(journal$enabled) && !is.null(journal$latest_report)
  }, journals)
  if (length(visible) < 2L) return(visible)

  update_times <- vapply(visible, function(journal) {
    site_timestamp_order(journal$latest_report$generated_at_utc)
  }, numeric(1))
  names <- vapply(visible, function(journal) site_text(journal$name), character(1))
  visible <- visible[order(-update_times, names)]
  head(visible, 2L)
}

site_journal_mark <- function(journal) {
  id <- site_text(journal$id)
  marks <- c(
    "nature-biomedical-engineering" = "nature",
    "nature-immunology" = "nature",
    "science-immunology" = "Science",
    "nature" = "nature",
    "science" = "Science",
    "cell" = "Cell",
    "immunity" = "Immunity",
    "cell-biomaterials" = "cell bio",
    "nature-materials" = "nature",
    "nature-biotechnology" = "nature"
  )
  mark <- unname(marks[id])
  if (!length(mark) || is.na(mark)) mark <- toupper(substr(site_text(journal$name), 1, 3))
  paste0('<span class="journal-mark journal-mark--', site_html_escape(site_slug(id)),
         '" aria-hidden="true">', site_html_escape(mark), '</span>')
}

site_render_journal_directory <- function(journals) {
  visible <- Filter(function(journal) isTRUE(journal$enabled) && length(journal$reports), journals)
  if (!length(visible)) return(invisible(cat("<p>No monitored journals yet.</p>")))
  cat('<ul class="journal-list">')
  for (journal in visible) {
    latest <- journal$latest_report
    date <- site_format_date(substr(site_text(latest$generated_at_utc), 1, 10))
    cat(sprintf('<li><a class="journal-card-link" href="%s.html">%s<span class="journal-card-copy"><strong>%s</strong><span>Last update %s</span></span><span class="journal-card-arrow" aria-hidden="true">›</span></a></li>',
      site_html_escape(site_slug(journal$id)), site_journal_mark(journal),
      site_html_escape(journal$name), site_html_escape(date)))
  }
  cat("</ul>")
  invisible(NULL)
}

site_render_homepage_journal_list <- function(journals) {
  visible <- site_visible_journals(journals)
  if (!length(visible)) return('<p class="empty-journal-list">No journal updates yet.</p>')
  items <- vapply(visible, function(journal) {
    latest <- journal$latest_report
    update_date <- site_format_date(substr(site_text(latest$generated_at_utc), 1, 10))
    sprintf(paste0(
      '<li><a class="journal-card-link" href="journals/%s.html">',
      '%s<span class="journal-card-copy"><strong>%s</strong>',
      '<span>Last update %s</span></span>',
      '<span class="journal-card-arrow" aria-hidden="true">›</span></a></li>'
    ), site_html_escape(site_slug(journal$id)), site_journal_mark(journal),
    site_html_escape(journal$name), site_html_escape(update_date))
  }, character(1))
  paste0('<ul class="journal-list">', paste(items, collapse = ""), '</ul>')
}

site_render_report_content <- function(report, show_journal_link = FALSE) {
  if (isTRUE(show_journal_link)) {
    cat(sprintf('<p><a href="../journals/%s.html">← %s</a></p>',
                site_html_escape(site_slug(report$journal_id)),
                site_html_escape(report$journal_name)))
  }

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

site_render_journal_page <- function(site, journal_id) {
  matches <- Filter(function(x) identical(x$id, journal_id), site$journals)
  if (!length(matches)) {
    cat("<p>No update history is available.</p>")
    return(invisible(NULL))
  }
  journal <- matches[[1]]
  if (!length(journal$reports)) {
    return(invisible(NULL))
  }

  # Show the newest report immediately; keep the older report links collapsed.
  site_render_report_content(journal$reports[[1]])

  older_reports <- if (length(journal$reports) > 1L) journal$reports[-1] else list()
  if (length(older_reports)) {
    cat(sprintf(
      '<details class="older-updates"><summary>Other and older updates (%d)</summary><div class="older-update-list">',
      length(older_reports)
    ))
    for (report in older_reports) {
      report_slug <- site_slug(report$report_id)
      update_date <- site_format_date(substr(report$generated_at_utc, 1, 10))
      window <- paste(site_format_date(report$start_date), "–", site_format_date(report$end_date))
      detail <- sprintf("PubMed Create Date: %s · %d papers · %d summaries",
                        window, report$article_count, report$summary_count)
      cat(sprintf(
        '<article class="update-card"><h3><a href="../updates/%s.html">%s</a></h3><p class="update-meta">%s</p></article>',
        site_html_escape(report_slug), site_html_escape(update_date), site_html_escape(detail)
      ))
    }
    cat("</div></details>")
  }
  invisible(NULL)
}

site_render_update_page <- function(site, report_id) {
  matches <- Filter(function(x) identical(x$report_id, report_id), site$reports)
  if (!length(matches)) {
    cat("<p>This update could not be found.</p>")
    return(invisible(NULL))
  }
  site_render_report_content(matches[[1]], show_journal_link = TRUE)
}


# Keyword-channel site data ------------------------------------------------

load_keyword_site_data <- function(root = ".") {
  paths <- sort(list.files(file.path(root, "reports", "keywords"),
    pattern = "\\.json$", recursive = TRUE, full.names = TRUE))
  config_path <- file.path(root, "config", "keywords.yml")
  config <- if (file.exists(config_path)) yaml::read_yaml(config_path) else list()
  journal_config_path <- file.path(root, "config", "journals.yml")
  journal_config <- if (file.exists(journal_config_path)) yaml::read_yaml(journal_config_path) else list()
  monitored_journals <- journal_config$journals %||% list()
  summaries <- site_read_summary_files(root)
  records <- list()
  for (path in paths) {
    report <- site_read_json(path)
    keyword <- report$keyword
    id <- site_text(keyword$id)
    if (!nzchar(id)) next
    articles <- report$articles %||% list()
    for (i in seq_along(articles)) {
      pmid <- site_text(articles[[i]]$pmid)
      articles[[i]]$pmid <- pmid
      articles[[i]]$summary <- summaries[[pmid]]
      articles[[i]]$pubmed_url <- site_text(articles[[i]]$pubmed_url,
        if (nzchar(pmid)) paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/") else "")
    }
    records[[length(records) + 1L]] <- list(
      report_id = site_text(report$report_id, basename(path)),
      keyword_id = id, keyword_name = site_text(keyword$name, id),
      query = site_text(keyword$query),
      generated_at_utc = site_text(report$generated_at_utc),
      start_date = site_text(report$retrieval_window$query_start_inclusive),
      end_date = site_text(report$retrieval_window$end_inclusive),
      article_count = length(articles),
      articles = articles
    )
  }
  if (length(records)) {
    ord <- order(vapply(records, function(x) site_timestamp_order(x$generated_at_utc), numeric(1)),
                 decreasing = TRUE)
    records <- records[ord]
  }
  configured <- config$keywords %||% list()
  ids <- unique(c(vapply(configured, function(x) site_text(x$id), character(1)),
                  vapply(records, function(x) x$keyword_id, character(1))))
  priority <- config$priority %||% list()
  preferred <- tolower(unlist(priority$journals %||% list(), use.names = FALSE))
  groups <- priority$publisher_groups %||% list()
  group_titles <- unlist(lapply(groups, function(x) x$journals %||% list()), use.names = FALSE)
  group_titles <- tolower(group_titles)
  for (i in seq_along(records)) {
    articles <- records[[i]]$articles
    if (length(articles)) {
      journals <- tolower(vapply(articles, function(x) site_text(x$journal), character(1)))
      dates <- vapply(articles, function(x) site_timestamp_order(site_text(x$pubmed_created_date,
        site_text(x$publication_date))), numeric(1))
      tier <- ifelse(journals %in% preferred, 0L,
                     ifelse(journals %in% group_titles, 1L, 2L))
      # Apply monitored-journal and explicit TReg-title priority before configured journal/publisher tiers.
      ranked <- records[[i]]$articles
      ranked_journals <- tolower(vapply(ranked, function(x) site_text(x$journal), character(1)))
      ranked_tier <- ifelse(ranked_journals %in% preferred, 0L,
                            ifelse(ranked_journals %in% group_titles, 1L, 2L))
      ranked_dates <- vapply(ranked, function(x) site_timestamp_order(site_text(
        x$pubmed_created_date, site_text(x$publication_date)
      )), numeric(1))
      monitored_title <- vapply(ranked, function(x) {
        is_monitored <- any(vapply(monitored_journals, function(j) {
          isTRUE(j$enabled) && identical(tolower(site_text(j$name)), tolower(site_text(x$journal)))
        }, logical(1)))
        is_monitored && site_is_treg_title_match(x)
      }, logical(1))
      monitored <- vapply(ranked, function(x) any(vapply(monitored_journals, function(j) {
        isTRUE(j$enabled) && identical(tolower(site_text(j$name)), tolower(site_text(x$journal)))
      }, logical(1))), logical(1))
      records[[i]]$articles <- ranked[order(-as.integer(monitored),
        -as.integer(vapply(ranked, site_is_treg_title_match, logical(1))),
        ranked_tier, -vapply(ranked, site_relevance_score, numeric(1)), -ranked_dates)]
    }
  }
  lapply(ids, function(id) {
    matches <- Filter(function(x) identical(x$keyword_id, id), records)
    cfg <- Filter(function(x) identical(site_text(x$id), id), configured)
    list(id = id,
      name = if (length(cfg)) site_text(cfg[[1]]$name, id) else if (length(matches)) matches[[1]]$keyword_name else id,
      enabled = length(cfg) == 0L || isTRUE(cfg[[1]]$enabled),
      reports = matches,
      latest_report = if (length(matches)) matches[[1]] else NULL)
  })
}

site_is_treg_title_match <- function(article) {
  title <- tolower(site_text(article$title))
  grepl("regulatory[ -]t[ -]cells?|tregs?", title, perl = TRUE)
}

site_is_monitored_journal <- function(article, site) {
  journal <- tolower(site_text(article$journal_name, site_text(article$journal)))
  any(vapply(site$journals, function(x) {
    isTRUE(x$enabled) && identical(journal, tolower(site_text(x$name)))
  }, logical(1)))
}

site_relevance_score <- function(article) {
  title <- tolower(site_text(article$title))
  abstract <- tolower(site_text(article$abstract))
  text <- paste(title, abstract)
  score <- 0
  patterns <- list(
    c("immune modulation|immunomodulat|immunoregul|immune response|inflamm", 5),
    c("regulatory t[ -]?cells?|tregs?", 4),
    c("biomaterial|hydrogel|scaffold|material[- ]based|nanoparticle|nanoplatform", 5),
    c("bone regeneration|bone repair|skeletal|osteogen|cartilage|fracture", 5),
    c("tissue engineering|tissue regeneration|regenerative medicine|wound healing", 4),
    c("organoid|organ[- ]on[- ]chip|microphysiological|in vitro model", 4),
    c("stem cell|mesenchymal stromal|cell therapy|cellular therapy", 3),
    c("single[- ]cell|spatial transcript|multiomics|machine learning|artificial intelligence", 2)
  )
  for (item in patterns) {
    if (grepl(item[[1]], text, perl = TRUE)) score <- score + as.numeric(item[[2]])
  }
  if (grepl(patterns[[1]][[1]], title, perl = TRUE)) score <- score + 2
  if (grepl(patterns[[3]][[1]], title, perl = TRUE)) score <- score + 2
  if (grepl(patterns[[4]][[1]], title, perl = TRUE)) score <- score + 2
  score
}

site_rank_keyword_articles <- function(articles, site) {
  if (length(articles) < 2L) return(articles)
  monitored <- vapply(articles, site_is_monitored_journal, logical(1), site = site)
  title_match <- vapply(articles, site_is_treg_title_match, logical(1))
  relevance <- vapply(articles, site_relevance_score, numeric(1))
  dates <- vapply(articles, function(x) site_timestamp_order(site_text(
    x$pubmed_created_date, site_text(x$publication_date)
  )), numeric(1))
  articles[order(-as.integer(monitored), -as.integer(title_match), -relevance, -dates)]
}

site_top_five <- function(site) {
  reports <- site$reports
  if (!length(reports)) return(list())
  latest_by_journal <- list()
  for (report in reports) {
    if (is.null(latest_by_journal[[report$journal_id]])) {
      latest_by_journal[[report$journal_id]] <- report
    }
  }
  recent <- head(latest_by_journal, 2L)
  selected <- list()
  selected_pmids <- character()
  for (report in recent) {
    articles <- report$articles
    if (length(articles)) {
      ranked <- articles[order(-vapply(articles, site_relevance_score, numeric(1)),
                               -vapply(articles, function(x) site_timestamp_order(site_text(x$publication_date)), numeric(1)))]
      chosen <- head(ranked, 2L)
      for (article in chosen) {
        article$selection_source <- report$journal_name
        selected[[length(selected) + 1L]] <- article
        selected_pmids <- c(selected_pmids, site_text(article$pmid))
      }
    }
  }
  pool <- list()
  for (keyword in site$keywords) {
    if (length(keyword$reports)) {
      pool <- c(pool, keyword$reports[[1]]$articles)
    }
  }
  for (journal in site$journals) {
    if (length(journal$reports)) pool <- c(pool, journal$reports[[1]]$articles)
  }
  if (length(pool)) {
    ids <- vapply(pool, function(x) site_text(x$pmid), character(1))
    pool <- pool[!duplicated(ids) & !ids %in% selected_pmids]
    if (length(pool)) {
      monitored <- vapply(pool, site_is_monitored_journal, logical(1), site = site)
      title_match <- vapply(pool, site_is_treg_title_match, logical(1))
      relevance <- vapply(pool, site_relevance_score, numeric(1))
      dates <- vapply(pool, function(x) site_timestamp_order(site_text(x$publication_date)), numeric(1))
      pool <- pool[order(-as.integer(monitored), -as.integer(title_match), -relevance, -dates)]
      if (length(pool)) {
        pool[[1]]$selection_source <- "Flexible selection"
        selected[[length(selected) + 1L]] <- pool[[1]]
      }
    }
  }
  head(selected, 5L)
}

site_render_top_five <- function(site) {
  papers <- site_top_five(site)
  if (!length(papers)) return('<p class="empty-journal-list">The weekly selection will appear after the next journal retrieval.</p>')

  groups <- list()
  for (article in papers) {
    source <- site_text(article$selection_source, "Keyword selection")
    key <- if (identical(source, "Flexible selection")) "Keyword selection" else source
    if (identical(key, "Flexible selection")) key <- "Keyword selection"
    if (is.null(groups[[key]])) groups[[key]] <- list()
    groups[[key]][[length(groups[[key]]) + 1L]] <- article
  }

  cat('<div class="top-five-list">')
  index <- 0L
  for (source in names(groups)) {
    is_keyword <- identical(source, "Keyword selection")
    if (is_keyword) {
      href <- "keywords/regulatory-t-cells.html"
    } else {
      journal <- Filter(function(x) identical(x$name, source), site$journals)
      journal_id <- if (length(journal)) journal[[1]]$id else site_slug(source)
      href <- paste0("journals/", site_html_escape(site_slug(journal_id)), ".html")
    }
    cat(sprintf('<section class="top-five-group"><h3 class="top-five-group-heading"><a href="%s">%s</a></h3>',
      href, site_html_escape(source)))
    for (article in groups[[source]]) {
      index <- index + 1L
      cat(sprintf('<div class="top-five-item"><span class="top-five-number">%d</span><div class="top-five-copy">',
        index))
      site_render_paper_card(article)
      cat('</div></div>')
    }
    cat('</section>')
  }
  cat('</div>')
  invisible(NULL)
}

site_render_keyword_page <- function(site, keyword_id) {
  matches <- Filter(function(x) identical(x$id, keyword_id), site$keywords)
  if (!length(matches) || !length(matches[[1]]$reports)) {
    cat("<p>No keyword results are available yet.</p>")
    return(invisible(NULL))
  }
  keyword <- matches[[1]]
  report <- keyword$reports[[1]]
  cat(sprintf('<p class="update-meta">Updated %s · %d matching papers · PubMed Create Date %s – %s</p>',
    site_html_escape(site_format_date(substr(report$generated_at_utc, 1, 10))),
    report$article_count, site_html_escape(site_format_date(report$start_date)),
    site_html_escape(site_format_date(report$end_date))))
  for (article in report$articles) site_render_paper_card(article)
  older <- if (length(keyword$reports) > 1L) keyword$reports[-1] else list()
  if (length(older)) {
    cat(sprintf('<details class="older-updates"><summary>Other and older updates (%d)</summary><div class="older-update-list">', length(older)))
    for (item in older) {
      cat(sprintf('<p>%s · %d papers</p>', site_html_escape(site_format_date(substr(item$generated_at_utc, 1, 10))),
                  item$article_count))
    }
    cat("</div></details>")
  }
  invisible(NULL)
}

site_render_homepage_keyword_list <- function(keywords) {
  visible <- Filter(function(x) isTRUE(x$enabled) && !is.null(x$latest_report), keywords)
  if (!length(visible)) return('<p class="empty-journal-list">No keyword updates yet.</p>')
  items <- vapply(visible, function(x) {
    latest <- x$latest_report
    sprintf(paste0('<li><a class="journal-card-link" href="keywords/%s.html">',
      '<span class="journal-mark keyword-mark" aria-hidden="true"><span class="treg-mark">T<sub>Reg</sub></span></span>',
      '<span class="journal-card-copy"><strong>%s</strong><span>%d papers · Updated %s</span></span>',
      '<span class="journal-card-arrow" aria-hidden="true">›</span></a></li>'),
      site_html_escape(site_slug(x$id)), site_html_escape(x$name), latest$article_count,
      site_html_escape(site_format_date(substr(latest$generated_at_utc, 1, 10))))
  }, character(1))
  paste0('<ul class="journal-list">', paste(items, collapse = ""), '</ul>')
}
