# Literature Radar

Automated, configurable PubMed monitoring with immutable per-journal retrieval reports. This first milestone retrieves citation metadata and abstracts only; LLM summarization and the Quarto website will be added in later milestones.

## Current design

- **R-first** implementation using NCBI E-utilities.
- **Weekly GitHub Actions** workflow; manual `workflow_dispatch` is available for testing.
- **Dynamic round-robin selection**: enabled journals are sorted by oldest successful report, so adding/removing journals requires editing only `config/journals.yml`.
- **12 example journals enabled initially**, with 3 selected per weekly run. Change `enabled` or `journals_per_run` in the YAML file at any time.
- **PubMed Create Date (`[crdt]`)** defines the retrieval interval. Publication date is metadata only, so an older paper first indexed this month appears in this month's report.
- **Per-journal cursor**: each journal has its own last successful retrieval end date.
- **Overlap + PMID deduplication**: the API query rechecks a small overlap, while PMIDs already included in earlier reports are excluded from the new report.
- **Immutable reports**: report files are written once under `reports/<journal-id>/`; existing reports are never overwritten.
- **Cursor safety**: a journal cursor advances only after retrieval, report writing, and persistence succeed.
- **No same-day duplicate runs**: manually rerunning a journal after it has completed through today's date is a no-op.
- **PubMed result limit safety**: if an interval exceeds PubMed ESearch's 10,000-UID limit, the interval is recursively split into smaller non-overlapping date ranges.

## First setup

1. Add `NCBI_EMAIL` as an Actions secret (recommended by NCBI; optional). An `NCBI_API_KEY` secret is also supported but not required.
2. Review `config/journals.yml` and verify the ISSNs for your chosen journals.
3. In Actions → Weekly PubMed retrieval → Run workflow, run the first retrieval. The scheduler selects the 3 enabled journals with the oldest (or missing) report history.
4. Review the committed files under `reports/`, `data/state/`, and `data/papers/`.

## Journal scheduling

The scheduler is not tied to fixed cohorts. Each run selects up to `settings.journals_per_run` enabled journals with the oldest `last_successful_report_at`. A new journal with no history is prioritized. With 12 evenly monitored journals and 3 selected per week, a complete cycle is approximately four weeks. With fewer or more journals, the cycle adjusts automatically.

Manual workflow input `journal_ids` can override selection for a one-off test, e.g. `nature-immunology,science-immunology`.

## Retrieval windows and reports

On first run, each selected journal retrieves the prior `initial_backfill_days` (30 by default) through today. Thereafter, each window starts with a configurable overlap before the previous successful end date and ends today. The overlap protects against boundary/indexing timing issues. PMIDs already stored from earlier reports are not repeated in the new report.

The query uses PubMed Create Date (`[crdt]`), not publication date. Thus, a paper published in September but first added to PubMed in November is included in the November retrieval report. The report month means **when PubMed indexed the citation**, not when the paper was published.

Every successful execution creates a new immutable report, including an empty report if no new PMIDs were found. The global `papers.json` is a deduplicated index; the historical report files are the permanent record. Previous reports are never rewritten.

If a run fails before the cursor is updated, the next run retries the uncompleted interval. The GitHub workflow commits reports, paper index, and state together only after the script succeeds.

## Data layout

```text
config/journals.yml             journal registry and settings
data/state/journal_state.json   per-journal successful retrieval cursors
data/papers/papers.json         deduplicated paper index
reports/<journal-id>/*.json    immutable retrieval reports
```

## Local development

Install dependencies and run the tests: 

```r
install.packages(c("httr2", "jsonlite", "tibble", "dplyr", "stringr",
                   "lubridate", "yaml", "purrr", "xml2", "glue", "cli", "testthat"))
Rscript tests/testthat.R
```

Run the automatically selected journals:

```bash
Rscript scripts/run_retrieval.R
```

Or explicitly run a subset:

```bash
Rscript scripts/run_retrieval.R nature-immunology science-immunology
```

## Roadmap

1. PubMed retrieval, per-journal cursors, immutable JSON reports (current milestone).
2. LLM adapter interface and benchmark harness for Ollama and Gemini 3.
3. Quarto website and GitHub Pages deployment.
4. Monthly journal overview, search/filter, quality checks and optional notifications.

## Notes

- ISSNs should be verified against PubMed. Some titles have print and electronic ISSNs; if a journal does not return expected results, we can extend its config to support multiple ISSNs.
- NCBI recommends identifying your application and providing a contact email. The workflow supports `NCBI_EMAIL` and `NCBI_API_KEY` repository secrets.
- GitHub scheduled workflow starts are best-effort and can be delayed during platform load. The per-journal state and dynamic selection make the system tolerant of delays.
- Do not store API keys in committed files. Later Gemini integration will use a GitHub Actions secret.
