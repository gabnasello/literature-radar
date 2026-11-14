# Shared prompt and Ollama adapter ---------------------------------------

read_summary_prompt_template <- function(path = file.path(
  project_root(), "prompts", "paper_summary_v1.txt"
)) {
  if (!file.exists(path)) stop("Prompt template not found: ", path, call. = FALSE)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

build_summary_prompt <- function(article, template = NULL) {
  if (is.null(template)) template <- read_summary_prompt_template()
  values <- list(
    TITLE = if (is.null(article$title)) "" else as.character(article$title),
    JOURNAL = if (is.null(article$journal_name)) "" else as.character(article$journal_name),
    ABSTRACT = if (is.null(article$abstract)) "" else as.character(article$abstract)
  )
  for (key in names(values)) {
    template <- gsub(paste0("{{", key, "}}"), values[[key]], template, fixed = TRUE)
  }
  template
}

parse_summary_response <- function(text) {
  cleaned <- trimws(as.character(text))
  fence <- intToUtf8(96L)
  cleaned <- sub(paste0("^", fence, "{3}[[:alnum:]]*[[:space:]]*"), "", cleaned)
  cleaned <- sub(paste0("[[:space:]]*", fence, "{3}$"), "", cleaned)
  parsed <- tryCatch(
    jsonlite::fromJSON(cleaned, simplifyVector = FALSE),
    error = function(e) NULL
  )
  list(valid_json = !is.null(parsed) && is.list(parsed), parsed = parsed, cleaned_text = cleaned)
}

validate_summary_payload <- function(parsed, min_words = 45L, max_words = 65L) {
  errors <- character()
  required <- c("summary", "key_findings", "study_type", "scientific_significance")

  if (!is.list(parsed) || is.null(names(parsed))) {
    return(list(valid = FALSE, errors = "The response must be a JSON object."))
  }

  missing <- setdiff(required, names(parsed))
  unexpected <- setdiff(names(parsed), required)
  if (length(missing)) {
    errors <- c(errors, paste("Missing fields:", paste(missing, collapse = ", ")))
  }
  if (length(unexpected)) {
    errors <- c(errors, paste(
      "Unexpected fields (the schema must not add relevance or limitations sections):",
      paste(unexpected, collapse = ", ")
    ))
  }

  scalar_string <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value)
  }

  summary <- parsed$summary
  if (!scalar_string(summary) || !nzchar(trimws(summary))) {
    errors <- c(errors, "summary must be a non-empty string.")
  } else {
    word_count <- length(strsplit(trimws(summary), "[[:space:]]+")[[1]])
    if (word_count < min_words || word_count > max_words) {
      errors <- c(errors, sprintf(
        "summary must contain %d–%d words; received %d.",
        min_words, max_words, word_count
      ))
    }
  }

  findings <- parsed$key_findings
  if (!(is.list(findings) || is.character(findings))) {
    errors <- c(errors, "key_findings must be an array of strings.")
  } else {
    if (length(findings) > 3L) {
      errors <- c(errors, "key_findings must contain no more than three items.")
    }
    if (length(findings) && any(!vapply(findings, scalar_string, logical(1)))) {
      errors <- c(errors, "Every key_findings item must be a string.")
    }
  }

  for (field in c("study_type", "scientific_significance")) {
    if (!scalar_string(parsed[[field]])) {
      errors <- c(errors, paste0(field, " must be a string (use an empty string when unavailable)."))
    }
  }

  list(valid = length(errors) == 0L, errors = errors)
}

call_ollama <- function(prompt, model, base_url, temperature = 0.2,
                        num_ctx = 4096L, num_predict = 800L,
                        timeout_seconds = 600) {
  started <- Sys.time()
  endpoint <- paste0(sub("/+$", "", base_url), "/api/chat")
  payload <- list(
    model = model,
    messages = list(list(role = "user", content = prompt)),
    stream = FALSE,
    think = FALSE,
    format = "json",
    options = list(
      temperature = temperature,
      num_ctx = as.integer(num_ctx),
      num_predict = as.integer(num_predict)
    ),
    keep_alive = "5m"
  )
  request <- httr2::request(endpoint) |>
    httr2::req_body_json(payload, auto_unbox = TRUE) |>
    httr2::req_timeout(timeout_seconds)
  response <- httr2::req_perform(request)
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  response_text <- body$message$content
  if (is.null(response_text) || !nzchar(response_text)) {
    if (!is.null(body$message$thinking) && nzchar(body$message$thinking)) {
      stop(
        "Ollama returned a thinking-only response for model ", model,
        " despite think = false. Check the Ollama version/model template.",
        call. = FALSE
      )
    }
    stop("Ollama returned an empty response for model ", model, call. = FALSE)
  }
  parsed <- parse_summary_response(response_text)
  ns_to_seconds <- function(x) {
    if (is.null(x) || !is.numeric(x)) NA_real_ else as.numeric(x) / 1e9
  }
  list(
    provider = "ollama",
    model = model,
    status = if (parsed$valid_json) "ok" else "invalid_json",
    response_text = response_text,
    parsed = parsed$parsed,
    elapsed_seconds = elapsed,
    input_tokens = if (is.null(body$prompt_eval_count)) NA_real_ else as.numeric(body$prompt_eval_count),
    output_tokens = if (is.null(body$eval_count)) NA_real_ else as.numeric(body$eval_count),
    thoughts_tokens = NA_real_,
    total_tokens = if (is.null(body$prompt_eval_count) || is.null(body$eval_count)) {
      NA_real_
    } else {
      as.numeric(body$prompt_eval_count) + as.numeric(body$eval_count)
    },
    model_eval_seconds = ns_to_seconds(body$eval_duration),
    error = if (parsed$valid_json) NULL else "Response was not valid JSON."
  )
}
