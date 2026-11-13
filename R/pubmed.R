# PubMed E-utilities retrieval --------------------------------------------

ncbi_base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"

ncbi_request <- function(endpoint, query = list(), email = "") {
  if (nzchar(email)) query$email <- email
  if (nzchar(Sys.getenv("NCBI_API_KEY"))) query$api_key <- Sys.getenv("NCBI_API_KEY")
  query$tool <- "LiteratureRadar"
  req <- httr2::request(paste0(ncbi_base, "/", endpoint)) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("LiteratureRadar/0.1 (PubMed research monitoring)") |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 4)
  resp <- httr2::req_perform(req)
  httr2::resp_check_status(resp)
  body <- httr2::resp_body_string(resp)
  # Enforce a conservative global pace across ESearch and EFetch requests.
  Sys.sleep(0.34)
  body
}

pubmed_search_once <- function(issn, start_date, end_date, email = "") {
  term <- sprintf('"%s"[ISSN] AND (%s[crdt] : %s[crdt])', issn, start_date, end_date)
  raw <- ncbi_request("esearch.fcgi", list(db = "pubmed", term = term,
    usehistory = "n", retmode = "json", retmax = 10000L), email)
  parsed <- jsonlite::fromJSON(raw, simplifyVector = TRUE)
  ids <- parsed$esearchresult$idlist
  if (is.null(ids)) ids <- character()
  list(pmids = as.character(ids),
       count = as.integer(parsed$esearchresult$count), term = term)
}

pubmed_search <- function(issn, start_date, end_date, email = "") {
  # PubMed ESearch exposes at most 10,000 UIDs per query. If a window exceeds
  # that limit, split it into non-overlapping date intervals recursively so
  # records are not silently dropped. A single day above the limit is fatal.
  first <- pubmed_search_once(issn, start_date, end_date, email)
  if (first$count <= 10000L) return(first)

  start <- as.Date(start_date, format = "%Y/%m/%d")
  end <- as.Date(end_date, format = "%Y/%m/%d")
  if (is.na(start) || is.na(end) || start >= end) {
    stop(sprintf("PubMed returned %s records for %s on a one-day interval; cannot safely retrieve all records with ESearch.",
      first$count, issn), call. = FALSE)
  }
  midpoint <- start + floor(as.numeric(end - start) / 2)
  left <- pubmed_search(issn, format(start, "%Y/%m/%d"), format(midpoint, "%Y/%m/%d"), email)
  right_start <- midpoint + 1
  right <- pubmed_search(issn, format(right_start, "%Y/%m/%d"), format(end, "%Y/%m/%d"), email)
  list(pmids = unique(c(left$pmids, right$pmids)),
       count = left$count + right$count,
       term = paste0("(", left$term, ") OR (", right$term, ")"))
}

xml_text <- function(node, xpath, default = "") {
  x <- xml2::xml_find_first(node, xpath)
  if (inherits(x, "xml_missing")) default else trimws(xml2::xml_text(x))
}

parse_pubmed_article <- function(article_node) {
  pmid <- xml_text(article_node, ".//MedlineCitation/PMID")
  title_node <- xml2::xml_find_first(article_node, ".//ArticleTitle")
  title <- if (inherits(title_node, "xml_missing")) "" else trimws(xml2::xml_text(title_node))
  abstract_nodes <- xml2::xml_find_all(article_node, ".//Abstract/AbstractText")
  abstract <- if (!length(abstract_nodes)) "" else paste(vapply(abstract_nodes, function(n) {
    label <- xml2::xml_attr(n, "Label")
    value <- trimws(xml2::xml_text(n))
    if (!is.null(label) && nzchar(label)) paste0(label, ": ", value) else value
  }, character(1)), collapse = "\n")
  doi_nodes <- xml2::xml_find_all(article_node, ".//ArticleId[@IdType='doi']")
  doi <- if (length(doi_nodes)) trimws(xml2::xml_text(doi_nodes[[1]])) else ""
  journal <- xml_text(article_node, ".//Journal/Title")
  journal_issn <- xml_text(article_node, ".//Journal/ISSN")
  authors <- xml2::xml_find_all(article_node, ".//AuthorList/Author")
  author_names <- vapply(authors, function(a) {
    collective <- xml_text(a, "./CollectiveName")
    if (nzchar(collective)) return(collective)
    last <- xml_text(a, "./LastName")
    fore <- xml_text(a, "./ForeName")
    trimws(paste(fore, last))
  }, character(1))
  pubdate <- xml_text(article_node, ".//JournalIssue/PubDate/Year")
  if (!nzchar(pubdate)) pubdate <- xml_text(article_node, ".//JournalIssue/PubDate/MedlineDate")
  created <- xml_text(article_node, ".//DateCreated/Year")
  if (nzchar(created)) created <- paste(created,
    xml_text(article_node, ".//DateCreated/Month"),
    xml_text(article_node, ".//DateCreated/Day"), sep = "-")
  list(pmid = pmid, title = title, abstract = abstract,
       doi = doi, doi_url = if (nzchar(doi)) paste0("https://doi.org/", doi) else "",
       journal = journal, journal_issn = journal_issn,
       authors = as.list(author_names), publication_date = pubdate,
       pubmed_created_date = created,
       pubmed_url = if (nzchar(pmid)) paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/") else "")
}

pubmed_fetch_batch <- function(pmids, email = "", batch_size = 200L) {
  if (!length(pmids)) return(list())
  groups <- split(pmids, ceiling(seq_along(pmids) / as.integer(batch_size)))
  out <- list()
  for (i in seq_along(groups)) {
    group <- groups[[i]]
    raw <- ncbi_request("efetch.fcgi", list(db = "pubmed", id = paste(group, collapse = ","), retmode = "xml"), email)
    doc <- xml2::read_xml(raw)
    nodes <- xml2::xml_find_all(doc, ".//PubmedArticle")
    out <- c(out, lapply(nodes, parse_pubmed_article))
  }
  out
}

retrieve_journal_window <- function(journal, start_date, end_date, email = "", batch_size = 200L) {
  search <- pubmed_search(journal$issn, start_date, end_date, email)
  articles <- pubmed_fetch_batch(search$pmids, email, batch_size)
  list(journal = journal, start_date = start_date, end_date = end_date,
       search_term = search$term, total_count = search$count,
       returned_count = length(articles), articles = articles)
}
