# Literature Radar

Automated, configurable PubMed monitoring with immutable per-journal retrieval reports and a reproducible LLM comparison workflow.

## Current design

- R-first implementation using NCBI E-utilities.
- Weekly GitHub Actions retrieval; manual workflow dispatch is available for testing.
- Dynamic journal scheduling based on each journal's oldest successful retrieval cursor.
- PubMed Create Date (crdt) defines retrieval windows, not publication date.
- Per-journal cursors, overlap protection, PMID deduplication, and immutable JSON reports.
- A separate, manual LLM benchmark compares a local Ollama model with the Gemini API using identical abstracts and prompts.
- The Quarto website will be added in the next milestone.

## Phase 1: PubMed retrieval

1. Add NCBI_EMAIL as an Actions secret (recommended by NCBI; optional). NCBI_API_KEY is also supported.
2. Review config/journals.yml and verify the ISSNs.
3. In Actions, run Weekly PubMed retrieval.
4. Review reports/, data/state/, and data/papers/.

The scheduler selects enabled journals with the oldest successful retrieval history. New journals without history are prioritized. The PubMed Create Date (crdt) field determines when an article enters a retrieval report.

## Phase 2: LLM benchmark

The benchmark is intentionally separate from weekly retrieval. An LLM/API failure cannot interrupt PubMed collection, and benchmarking only runs when manually requested.

### Models and prompt

- Local model through Ollama: qwen3:4b, running on the GitHub Actions CPU runner.
- Gemini API: gemini-3.8-flash.
- Shared prompt: prompts/paper_summary_v1.txt.
- Default sample: 12 abstracts, sampled reproducibly and approximately evenly across journals.
- Both providers receive the exact same prompt and abstract. JSON output mode is enabled for both.

The prompt requests a concise summary, key findings, study type, relevance to biomaterials and regenerative medicine, and limitations or unknowns. It instructs both models not to infer facts beyond the abstract.

Model names, generation settings, sample size, random seed, and Gemini pricing assumptions are editable in config/llm.yml.

### Configure the Gemini API key

1. Create an API key in Google AI Studio.
2. In the GitHub repository, open Settings → Secrets and variables → Actions.
3. Select New repository secret.
4. Set the name to GEMINI_API_KEY and paste the key as its value.

Never put the key in a committed file or in workflow inputs. The workflow reads it from the repository secret.

### Run the benchmark

1. Open Actions → LLM benchmark.
2. Select Run workflow.
3. Keep the default sample size of 12 for the first comparison, or enter a different positive integer.
4. Wait for the job to finish.

The first run installs Ollama and downloads the selected model, so it may take several minutes. The local model runs on CPU; its elapsed time is recorded.

### Outputs

Each run is saved in a unique folder under data/benchmarks/:

- benchmark.json: sampled paper metadata, prompt, model outputs, token counts, timings, and errors.
- metrics.csv: one row per paper and provider, for easy comparison in a spreadsheet.
- comparison.md: paired summaries for manual scientific review.

The benchmark output records an estimated Gemini paid-tier cost from input, output, and reported reasoning-token counts using the configured rates. This is an estimate, not an invoice; actual charges depend on the account's free-tier eligibility and billing status. The configured Gemini 3.8 Flash rates are the published introductory standard-tier rates through 31 December 2026 and should be reviewed before 2027.

The benchmark does not automatically decide which model is scientifically better. Review factual accuracy, omitted findings, hallucinations, usefulness, and reading time. Human review is important for scientific reliability.

## Data layout

- config/journals.yml: monitored journal registry and retrieval settings.
- config/llm.yml: model IDs, sampling, and generation settings.
- prompts/: versioned prompts shared across providers.
- data/state/: per-journal successful retrieval cursors.
- data/papers/: deduplicated paper index.
- reports/: immutable PubMed retrieval reports.
- data/benchmarks/: immutable paired LLM benchmark runs.

## Local development

Install the R dependencies with install.packages, then run:

- Rscript tests/testthat.R
- Rscript scripts/run_retrieval.R
- Rscript scripts/run_benchmark.R (requires a running Ollama server/model and GEMINI_API_KEY)

## Roadmap

1. PubMed retrieval, per-journal cursors, immutable JSON reports (complete).
2. LLM adapters and paired benchmark for Ollama and Gemini (implemented; first benchmark run pending).
3. Quarto website and GitHub Pages deployment.
4. Monthly journal overview, search/filter, quality checks, and optional notifications.

## Operational notes

- GitHub scheduled workflow starts are best-effort and can be delayed.
- Never store API keys in the repository.
- The LLM benchmark is manual and separate from retrieval to control API use and compute time.
