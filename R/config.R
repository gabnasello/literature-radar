# Configuration and filesystem helpers ------------------------------------

project_root <- function() {
  root <- Sys.getenv("LITERATURE_RADAR_ROOT", unset = ".")
  normalizePath(root, mustWork = FALSE)
}

read_radar_config <- function(path = file.path(project_root(), "config", "journals.yml")) {
  cfg <- yaml::read_yaml(path)
  if (is.null(cfg$journals) || !length(cfg$journals)) {
    stop("No journals configured.", call. = FALSE)
  }
  if (is.null(cfg$settings)) cfg$settings <- list()

  ids <- vapply(cfg$journals, function(j) {
    if (is.null(j$id)) "" else as.character(j$id)
  }, character(1))
  if (any(!nzchar(ids))) stop("Every journal needs a non-empty id.", call. = FALSE)
  if (anyDuplicated(ids)) stop("Journal IDs must be unique.", call. = FALSE)

  for (j in cfg$journals) {
    if (is.null(j$name) || !nzchar(as.character(j$name))) {
      stop(sprintf("Journal '%s' needs a non-empty name.", j$id), call. = FALSE)
    }
    if (isTRUE(j$enabled)) {
      if (is.null(j$issn) || !nzchar(as.character(j$issn))) {
        stop(sprintf("Enabled journal '%s' needs an ISSN.", j$id), call. = FALSE)
      }
      if (!grepl("^[0-9]{4}-[0-9]{3}[0-9X]$", as.character(j$issn))) {
        stop(sprintf("Journal '%s' has an invalid ISSN format: %s", j$id, j$issn), call. = FALSE)
      }
    }
  }
  cfg
}

read_json_or <- function(path, default) {
  if (!file.exists(path)) return(default)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

write_json_atomic <- function(x, path, pretty = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  jsonlite::write_json(x, tmp, auto_unbox = TRUE, pretty = pretty, null = "null", na = "null")
  if (!file.rename(tmp, path)) stop("Could not atomically replace ", path, call. = FALSE)
  invisible(path)
}
