root <- normalizePath(".")
token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
chat_id <- Sys.getenv("TELEGRAM_CHAT_ID")
site_url <- Sys.getenv("SITE_URL", "https://gabnasello.github.io/literature-radar/")

if (!nzchar(token) || !nzchar(chat_id)) {
  stop("TELEGRAM_BOT_TOKEN and TELEGRAM_CHAT_ID must be configured as repository secrets.", call. = FALSE)
}

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(yaml)
})
source(file.path(root, "R", "site.R"))
source(file.path(root, "R", "llm.R"))

site <- load_literature_site_data(root)
top_five <- site_top_five(site)
if (!length(top_five)) {
  message("No Top 5 selection is available; Telegram digest skipped.")
  quit(status = 0L)
}

# The summarizer places these papers first in its queue. Wait only for the
# selected Top 5, not for the remaining journal and keyword backlog.
pending_top_five <- Filter(function(article) {
  site_is_summarizable(article) && !site_has_summary(article)
}, top_five)
if (length(pending_top_five)) {
  message(length(pending_top_five), " selected Top 5 paper(s) still need a valid AI summary.")
  quit(status = 0L)
}
ready <- Filter(site_has_summary, top_five)
if (!length(ready)) {
  message("None of the selected Top 5 papers has a valid AI summary yet.")
  quit(status = 0L)
}

# Build a stable weekly key from the newest journal and keyword retrieval reports.
journal_latest <- Filter(function(x) !is.null(x$latest_report), site$journals)
journal_reports <- lapply(journal_latest, function(x) x$latest_report)
keyword_latest <- Filter(function(x) !is.null(x$latest_report), site$keywords)
keyword_reports <- lapply(keyword_latest, function(x) x$latest_report)
all_reports <- c(journal_reports, keyword_reports)
if (!length(all_reports)) {
  message("No retrieval reports are available; Telegram digest skipped.")
  quit(status = 0L)
}
report_ids <- sort(vapply(all_reports, function(x) site_text(x$report_id), character(1)))
digest_key <- paste(report_ids, collapse = "|")
state_dir <- file.path(root, "data", "telegram")
state_file <- file.path(state_dir, "last_digest_key.txt")
dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
previous_key <- if (file.exists(state_file)) trimws(readLines(state_file, warn = FALSE)[1]) else ""
if (identical(previous_key, digest_key)) {
  message("Telegram digest already sent for these retrieval reports; skipping duplicate.")
  quit(status = 0L)
}

html_escape <- function(x) {
  x <- site_text(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}
send_telegram <- function(message) {
  response <- request(paste0("https://api.telegram.org/bot", token, "/sendMessage")) |>
    req_body_form(
      chat_id = chat_id,
      text = message,
      parse_mode = "HTML",
      disable_web_page_preview = "true"
    ) |>
    req_timeout(30) |>
    req_perform()
  payload <- resp_body_json(response, simplifyVector = FALSE)
  if (!isTRUE(payload$ok)) stop("Telegram API rejected a message.", call. = FALSE)
  invisible(payload)
}

# The weekly overview lists the latest checked report for each journal and keyword.
journal_lines <- vapply(journal_latest, function(journal) {
  report <- journal$latest_report
  sprintf("• %s: <b>%d</b> papers", html_escape(journal$name), as.integer(report$article_count))
}, character(1))
keyword_lines <- vapply(keyword_latest, function(keyword) {
  report <- keyword$latest_report
  sprintf("• %s: <b>%d</b> papers", html_escape(keyword$name), as.integer(report$article_count))
}, character(1))
overview <- c(
  "<b>Literature Radar · Weekly update</b>",
  "",
  "<b>Journals checked</b>",
  if (length(journal_lines)) journal_lines else "• None",
  "",
  "<b>Keyword searches</b>",
  if (length(keyword_lines)) keyword_lines else "• None",
  "",
  paste0('<a href="', html_escape(site_url), '">Open Literature Radar · all papers and analyses</a>')
)
send_telegram(paste(overview, collapse = "\n"))

# Match the homepage grouping: one Telegram message for each source journal/group.
groups <- list()
for (article in ready) {
  source_name <- site_text(article$selection_source, "Keyword selection")
  group_name <- if (identical(source_name, "Flexible selection")) "Keyword selection" else source_name
  if (is.null(groups[[group_name]])) groups[[group_name]] <- list()
  groups[[group_name]][[length(groups[[group_name]]) + 1L]] <- article
}
for (group_name in names(groups)) {
  is_keyword <- identical(group_name, "Keyword selection")
  page_url <- if (is_keyword) {
    paste0(sub("/+$", "", site_url), "/keywords/regulatory-t-cells.html")
  } else {
    journal <- Filter(function(x) identical(x$name, group_name), site$journals)
    journal_id <- if (length(journal)) journal[[1]]$id else site_slug(group_name)
    paste0(sub("/+$", "", site_url), "/journals/", site_slug(journal_id), ".html")
  }

  blocks <- vapply(groups[[group_name]], function(article) {
    pmid <- site_text(article$pmid)
    card_url <- paste0(page_url, "#paper-", utils::URLencode(pmid, reserved = TRUE))
    authors <- article$authors
    if (is.list(authors)) authors <- unlist(authors, use.names = FALSE)
    authors <- as.character(authors)
    authors <- authors[!is.na(authors) & nzchar(authors)]
    author_text <- if (!length(authors)) "Authors unavailable" else if (length(authors) == 1L) authors[[1]] else paste0(authors[[1]], " et al.")
    journal_name <- site_text(article$journal_name, site_text(article$journal, group_name))
    significance <- paste(summary_values(article$summary$parsed, "scientific_significance"), collapse = " ")
    paste0(
      '<a href="', html_escape(card_url), '"><b>', html_escape(article$title), '</b></a>',
      "\n", html_escape(author_text), " · ", html_escape(journal_name),
      "\n", html_escape(significance)
    )
  }, character(1))
  message_text <- paste(
    paste0("<b>Literature Radar · ", html_escape(group_name), "</b>"),
    "",
    paste(blocks, collapse = "\n\n"),
    sep = "\n"
  )
  send_telegram(message_text)
}

writeLines(digest_key, state_file, useBytes = TRUE)
message("Telegram weekly digest sent: overview plus ", length(groups), " paper group message(s).")
