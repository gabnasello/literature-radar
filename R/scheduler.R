# Fair dynamic round-robin scheduler --------------------------------------

select_journals <- function(journals, state, n = 3L) {
  active <- Filter(function(j) isTRUE(j$enabled), journals)
  if (!length(active)) return(list())
  n <- suppressWarnings(as.integer(n)[1])
  if (is.na(n) || n < 1L) stop("journals_per_run must be a positive integer.", call. = FALSE)

  # Journals never processed before sort first. After that, the oldest
  # successful report is prioritized. ID is a deterministic tie-breaker.
  last <- vapply(active, function(j) {
    x <- state[[j$id]]$last_successful_report_at
    if (is.null(x) || !nzchar(x)) "0000-00-00T00:00:00Z" else x
  }, character(1))
  ids <- vapply(active, `[[`, character(1), "id")
  ord <- order(last, ids)
  active[ord[seq_len(min(n, length(active)))]]
}
