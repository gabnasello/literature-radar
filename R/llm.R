# Shared prompt and LLM provider adapters ---------------------------------

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

call_ollama <- function(prompt, model, base_url, temperature = 0.2,
                        num_ctx = 4096L, num_predict = 800L,
                        timeout_seconds = 600) {
  started <- Sys.time()
  endpoint <- paste0(sub("/+$", "", base_url), "/api/chat")
  payload <- list(
    model = model,
    messages = list(list(role = "user", content = prompt)),
    stream = FALSE,
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

call_gemini <- function(prompt, model, api_key, temperature = 0.2,
                        max_output_tokens = 800L, timeout_seconds = 240) {
  if (is.null(api_key) || !nzchar(api_key)) {
    stop("GEMINI_API_KEY is not set. Add it as a GitHub Actions repository secret.", call. = FALSE)
  }
  started <- Sys.time()
  endpoint <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model, ":generateContent"
  )
  payload <- list(
    contents = list(list(
      role = "user",
      parts = list(list(text = prompt))
    )),
    generationConfig = list(
      temperature = temperature,
      maxOutputTokens = as.integer(max_output_tokens),
      responseMimeType = "application/json"
    )
  )
  request <- httr2::request(endpoint) |>
    httr2::req_headers("x-goog-api-key" = api_key) |>
    httr2::req_body_json(payload, auto_unbox = TRUE) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_retry(max_tries = 3)
  response <- httr2::req_perform(request)
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  if (is.null(body$candidates) || !length(body$candidates)) {
    stop("Gemini returned no candidates. Check safety blocks and API response.", call. = FALSE)
  }
  parts <- body$candidates[[1]]$content$parts
  if (is.null(parts) || !length(parts)) {
    stop("Gemini returned an empty candidate.", call. = FALSE)
  }
  text_parts <- vapply(parts, function(part) {
    if (is.null(part$text)) "" else as.character(part$text)
  }, character(1))
  response_text <- paste(text_parts[nzchar(text_parts)], collapse = "\n")
  if (!nzchar(response_text)) stop("Gemini returned no text.", call. = FALSE)

  parsed <- parse_summary_response(response_text)
  usage <- body$usageMetadata
  token_value <- function(name) {
    value <- usage[[name]]
    if (is.null(value)) NA_real_ else as.numeric(value)
  }
  list(
    provider = "gemini",
    model = model,
    status = if (parsed$valid_json) "ok" else "invalid_json",
    response_text = response_text,
    parsed = parsed$parsed,
    elapsed_seconds = elapsed,
    input_tokens = token_value("promptTokenCount"),
    output_tokens = token_value("candidatesTokenCount"),
    thoughts_tokens = token_value("thoughtsTokenCount"),
    total_tokens = token_value("totalTokenCount"),
    finish_reason = body$candidates[[1]]$finishReason,
    error = if (parsed$valid_json) NULL else "Response was not valid JSON."
  )
}
