# Orchestrates one scheduled retrieval batch ------------------------------

run_retrieval <- function(config_path = file.path(project_root(), "config", "journals.yml"),
                          today = Sys.Date(), journal_ids = NULL) {
  cfg <- read_radar_config(config_path)
  if (!nzchar(cfg$settings$email %||% "")) cfg$settings$email <- Sys.getenv("NCBI_EMAIL", unset = "")
  state <- load_state()
  papers <- load_papers()
  active <- Filter(function(j) isTRUE(j$enabled), cfg$journals)

  if (!is.null(journal_ids) && length(journal_ids)) {
    all_ids <- vapply(cfg$journals, `[[`, character(1), "id")
    unknown <- setdiff(journal_ids, all_ids)
    disabled <- setdiff(journal_ids, vapply(active, `[[`, character(1), "id"))
    if (length(unknown)) stop("Unknown journal ID(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    if (length(disabled)) stop("Journal(s) are disabled: ", paste(disabled, collapse = ", "), call. = FALSE)
    selected <- Filter(function(j) j$id %in% journal_ids, active)
  } else {
    n <- cfg$settings$journals_per_run %||% 3L
    selected <- select_journals(active, state, n)
  }

  if (!length(selected)) {
    message("No enabled journals selected; nothing to do.")
    return(invisible(list(selected = character(), results = list())))
  }

  results <- list()
  for (journal in selected) {
    message(sprintf("Retrieving %s (%s)", journal$name, journal$id))
    one <- process_one_journal(journal, state, papers, today, cfg$settings)
    state <- one$state
    papers <- one$papers
    results[[journal$id]] <- one
    if (isTRUE(one$skipped)) {
      message("  Skipped: ", one$reason)
    } else {
      message(sprintf("  Retrieved %d records; %d new PMIDs; report: %s",
        one$retrieved, one$n, one$report_file))
    }
  }
  invisible(list(selected = names(results), results = results))
}
