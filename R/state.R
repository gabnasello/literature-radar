# Per-journal cursors and immutable reports -------------------------------

state_path <- function() file.path(project_root(), "data", "state", "journal_state.json")
papers_path <- function() file.path(project_root(), "data", "papers", "papers.json")

load_state <- function(path = state_path()) read_json_or(path, list())
save_state <- function(state, path = state_path()) write_json_atomic(state, path)
load_papers <- function(path = papers_path()) read_json_or(path, list())
save_papers <- function(papers, path = papers_path()) write_json_atomic(papers, path)

report_id <- function(journal_id, start_date, end_date) {
  paste(journal_id, gsub("-", "", start_date), gsub("-", "", end_date), sep = "__")
}

persist_report <- function(result, root = project_root()) {
  id <- report_id(result$journal$id, result$start_iso, result$end_iso)
  path <- file.path(root, "reports", result$journal$id, paste0(id, ".json"))
  if (file.exists(path)) stop("Refusing to overwrite immutable report: ", path, call. = FALSE)
  payload <- list(
    schema_version = 1L,
    report_id = id,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    journal = result$journal,
    retrieval_window = list(
      field = "PubMed Create Date (crdt)",
      # Query window includes the safety overlap; new_records_start marks the
      # actual incremental period after the previous successful cursor.
      query_start_inclusive = result$start_iso,
      new_records_start = result$new_records_start,
      end_inclusive = result$end_iso,
      previous_successful_end_date = result$previous_successful_end_date,
      overlap_days = result$overlap_days
    ),
    search = list(
      term = result$search_term,
      total_count = result$total_count,
      fetched_count = result$retrieved_count,
      overlap_or_previously_seen_count = result$overlap_excluded_count,
      new_count = length(result$articles)
    ),
    articles = result$articles
  )
  write_json_atomic(payload, path)
  path
}

merge_papers <- function(existing, new_articles) {
  by_id <- setNames(existing, vapply(existing, function(x) as.character(x$pmid), character(1)))
  for (article in new_articles) {
    pmid <- as.character(article$pmid)
    if (!nzchar(pmid)) next
    if (is.null(by_id[[pmid]])) by_id[[pmid]] <- article
  }
  unname(by_id)
}

initial_cursor <- function(today, days) as.character(as.Date(today) - as.integer(days))

window_for_journal <- function(journal, state_entry, today, initial_backfill_days = 30L, overlap_days = 2L) {
  last <- state_entry$last_successful_end_date
  start <- if (is.null(last) || !nzchar(last)) {
    as.Date(initial_cursor(today, initial_backfill_days))
  } else {
    as.Date(last) - as.integer(overlap_days) + 1L
  }
  list(start_date = format(start, "%Y/%m/%d"), end_date = format(as.Date(today), "%Y/%m/%d"),
       start_iso = format(start, "%Y-%m-%d"), end_iso = format(as.Date(today), "%Y-%m-%d"))
}

process_one_journal <- function(journal, state, papers, today = Sys.Date(), settings = list()) {
  entry <- state[[journal$id]] %||% list()
  today_iso <- format(as.Date(today), "%Y-%m-%d")

  # A manual rerun on the same day is a no-op rather than an immutable-report collision.
  if (!is.null(entry$last_successful_end_date) &&
      as.Date(entry$last_successful_end_date) >= as.Date(today)) {
    return(list(state = state, papers = papers, report_file = NULL, n = 0L,
      journal = journal$name, skipped = TRUE,
      reason = paste("Already retrieved through", entry$last_successful_end_date)))
  }

  window <- window_for_journal(journal, entry, today,
    settings$initial_backfill_days %||% 30L, settings$overlap_days %||% 2L)
  fetched <- retrieve_journal_window(journal, window$start_date, window$end_date,
    settings$email %||% "", settings$pubmed_batch_size %||% 200L)

  # The date overlap is re-queried as a safety margin, but a PMID already
  # stored in an earlier report is not repeated in this report.
  existing_ids <- if (length(papers)) {
    vapply(papers, function(x) as.character(x$pmid), character(1))
  } else character()
  new_articles <- Filter(function(x) !as.character(x$pmid) %in% existing_ids, fetched$articles)
  result <- fetched
  result$articles <- new_articles
  result$retrieved_count <- fetched$returned_count
  result$overlap_excluded_count <- fetched$returned_count - length(new_articles)
  result$overlap_days <- as.integer(settings$overlap_days %||% 2L)
  result$start_iso <- window$start_iso
  result$new_records_start <- if (is.null(entry$last_successful_end_date) ||
      !nzchar(entry$last_successful_end_date)) window$start_iso else
      format(as.Date(entry$last_successful_end_date) + 1L, "%Y-%m-%d")
  result$previous_successful_end_date <- entry$last_successful_end_date %||% NULL
  result$end_iso <- window$end_iso

  # Report, paper index and cursor are all committed together by the workflow.
  # Cursor advances only after every local write succeeds.
  report_file <- persist_report(result)
  all_papers <- merge_papers(papers, fetched$articles)
  save_papers(all_papers)

  entry$last_successful_report_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  entry$last_successful_end_date <- window$end_iso
  entry$last_report_id <- basename(report_file)
  entry$last_report_file <- report_file
  entry$last_count <- length(new_articles)
  entry$last_retrieved_count <- fetched$returned_count
  state[[journal$id]] <- entry
  save_state(state)

  list(state = state, papers = all_papers, report_file = report_file,
       n = length(new_articles), retrieved = fetched$returned_count,
       journal = journal$name, skipped = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
