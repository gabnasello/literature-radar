# PubMed keyword monitoring ------------------------------------------------

keyword_state_path <- function(root = project_root()) {
  file.path(root, "data", "state", "keyword_state.json")
}

keyword_report_id <- function(keyword_id, start_date, end_date) {
  paste(keyword_id, gsub("-", "", start_date), gsub("-", "", end_date), sep = "__")
}

pubmed_search_query_once <- function(query, start_date, end_date, email = "") {
  term <- sprintf("(%s) AND (%s[crdt] : %s[crdt])", query, start_date, end_date)
  raw <- ncbi_request("esearch.fcgi", list(db = "pubmed", term = term,
    usehistory = "n", retmode = "json", retmax = 10000L), email)
  parsed <- jsonlite::fromJSON(raw, simplifyVector = TRUE)
  ids <- parsed$esearchresult$idlist
  if (is.null(ids)) ids <- character()
  list(pmids = as.character(ids), count = as.integer(parsed$esearchresult$count), term = term)
}

pubmed_search_query <- function(query, start_date, end_date, email = "") {
  first <- pubmed_search_query_once(query, start_date, end_date, email)
  if (first$count <= 10000L) return(first)
  start <- as.Date(start_date, "%Y/%m/%d")
  end <- as.Date(end_date, "%Y/%m/%d")
  if (is.na(start) || is.na(end) || start >= end) {
    stop("A one-day PubMed keyword query exceeded 10,000 records; refusing to truncate results.", call. = FALSE)
  }
  midpoint <- start + floor(as.numeric(end - start) / 2)
  left <- pubmed_search_query(query, format(start, "%Y/%m/%d"), format(midpoint, "%Y/%m/%d"), email)
  right <- pubmed_search_query(query, format(midpoint + 1, "%Y/%m/%d"), format(end, "%Y/%m/%d"), email)
  list(pmids = unique(c(left$pmids, right$pmids)),
       count = left$count + right$count,
       term = paste0("(", left$term, ") OR (", right$term, ")"))
}

keyword_window <- function(entry, today, initial_backfill_days = 7L, overlap_days = 2L) {
  last <- entry$last_successful_end_date
  start <- if (is.null(last) || !nzchar(last)) {
    as.Date(today) - as.integer(initial_backfill_days)
  } else {
    as.Date(last) - as.integer(overlap_days) + 1L
  }
  list(start_date = format(start, "%Y/%m/%d"), end_date = format(as.Date(today), "%Y/%m/%d"),
       start_iso = format(start, "%Y-%m-%d"), end_iso = format(as.Date(today), "%Y-%m-%d"))
}

run_keyword_retrieval <- function(config_path = file.path(project_root(), "config", "keywords.yml"),
                                  today = Sys.Date()) {
  cfg <- yaml::read_yaml(config_path)
  if (is.null(cfg$keywords) || !length(cfg$keywords)) return(invisible(list()))
  settings <- cfg$settings %||% list()
  email <- settings$email %||% Sys.getenv("NCBI_EMAIL", unset = "")
  state_path <- keyword_state_path()
  state <- read_json_or(state_path, list())
  papers <- load_papers()
  results <- list()

  for (keyword in Filter(function(x) isTRUE(x$enabled), cfg$keywords)) {
    id <- as.character(keyword$id)
    entry <- state[[id]] %||% list()
    if (!is.null(entry$last_successful_end_date) &&
        as.Date(entry$last_successful_end_date) >= as.Date(today)) next

    window <- keyword_window(entry, today,
      settings$initial_backfill_days %||% 7L, settings$overlap_days %||% 2L)
    search <- pubmed_search_query(keyword$query, window$start_date, window$end_date, email)
    articles <- pubmed_fetch_batch(search$pmids, email, settings$pubmed_batch_size %||% 200L)

    # Deduplicate within this keyword channel only. A paper found through a
    # journal monitor is still retained here as a second discovery source.
    prior_ids <- character()
    paths <- list.files(file.path(project_root(), "reports", "keywords", id),
      pattern = "\\.json$", full.names = TRUE)
    for (path in paths) {
      old <- read_json_or(path)
      prior_ids <- c(prior_ids, vapply(old$articles %||% list(), function(x) as.character(x$pmid), character(1)))
    }
    new_articles <- Filter(function(x) !as.character(x$pmid) %in% prior_ids, articles)
    report_id <- keyword_report_id(id, window$start_iso, window$end_iso)
    report_dir <- file.path(project_root(), "reports", "keywords", id)
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
    report_path <- file.path(report_dir, paste0(report_id, ".json"))
    if (file.exists(report_path)) stop("Refusing to overwrite immutable keyword report: ", report_path, call. = FALSE)

    report <- list(
      schema_version = 1L, channel = "keyword", report_id = report_id,
      generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      keyword = list(id = id, name = as.character(keyword$name), query = as.character(keyword$query)),
      journal = list(id = paste0("keyword-", id), name = paste0("Keyword: ", keyword$name)),
      retrieval_window = list(field = "PubMed Create Date (crdt)",
        query_start_inclusive = window$start_iso, end_inclusive = window$end_iso,
        previous_successful_end_date = entry$last_successful_end_date %||% NULL,
        overlap_days = as.integer(settings$overlap_days %||% 2L)),
      search = list(term = search$term, total_count = search$count,
        fetched_count = length(articles), new_count = length(new_articles)),
      articles = new_articles
    )
    write_json_atomic(report, report_path)
    save_papers(merge_papers(papers, articles))
    papers <- merge_papers(papers, articles)
    entry$last_successful_end_date <- window$end_iso
    entry$last_successful_report_at <- report$generated_at_utc
    entry$last_report_id <- report_id
    entry$last_count <- length(new_articles)
    state[[id]] <- entry
    save_state(state, state_path)
    results[[id]] <- list(report_file = report_path, new_count = length(new_articles),
      total_count = search$count)
    message(sprintf("Keyword '%s': %d new papers (%d PubMed matches).",
      keyword$name, length(new_articles), search$count))
  }
  invisible(results)
}
