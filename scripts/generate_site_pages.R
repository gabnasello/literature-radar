source("R/site.R")

root <- "."
website_dir <- file.path(root, "website")
journal_dir <- file.path(website_dir, "journals")
update_dir <- file.path(website_dir, "updates")

dir.create(journal_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(update_dir, recursive = TRUE, showWarnings = FALSE)

unlink(list.files(journal_dir, pattern = "\\.qmd$", full.names = TRUE))
unlink(list.files(update_dir, pattern = "\\.qmd$", full.names = TRUE))

site <- load_literature_site_data(root)

write_generated_page <- function(path, title, render_call) {
  yaml_title <- as.character(jsonlite::toJSON(title, auto_unbox = TRUE))
  content <- c(
    "---",
    paste0("title: ", yaml_title),
    "---",
    "",
    "~~~{r, results='asis'}",
    "source(\"../R/site.R\")",
    "site <- load_literature_site_data(\"..\")",
    render_call,
    "~~~",
    ""
  )
  writeLines(content, path, useBytes = TRUE)
}

for (journal in site$journals) {
  slug <- site_slug(journal$id)
  journal_id <- as.character(jsonlite::toJSON(journal$id, auto_unbox = TRUE))
  path <- file.path(journal_dir, paste0(slug, ".qmd"))
  write_generated_page(
    path,
    journal$name,
    paste0("site_render_journal_page(site, ", journal_id, ")")
  )
}

for (report in site$reports) {
  slug <- site_slug(report$report_id)
  report_id <- as.character(jsonlite::toJSON(report$report_id, auto_unbox = TRUE))
  path <- file.path(update_dir, paste0(slug, ".qmd"))
  write_generated_page(
    path,
    paste0(report$journal_name, " · Update"),
    paste0("site_render_update_page(site, ", report_id, ")")
  )
}

message(
  "Generated ", length(site$journals), " journal pages and ",
  length(site$reports), " update pages."
)
