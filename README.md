# Literature Radar

Automated, configurable PubMed monitoring with immutable per-journal retrieval reports and an Ollama-based abstract summarization workflow.

## Current design

- R-first implementation using NCBI E-utilities.
- Weekly GitHub Actions retrieval; manual workflow dispatch is available for testing.
- Dynamic journal scheduling based on each journal's oldest successful retrieval cursor.
- PubMed Create Date (crdt) defines retrieval windows, not publication date.
- Per-journal cursors, overlap protection, PMID deduplication, and immutable JSON reports.
- A separate, manual LLM benchmark uses a local Ollama model to summarize selected abstracts.
- The Quarto website will be added in the next milestone.

## Phase 1: PubMed retrieval

1. Add NCBI_EMAIL as an Actions secret (recommended by NCBI; optional). NCBI_API_KEY is also supported.
2. Review config/journals.yml and verify the ISSNs.
3. In Actions, run Weekly PubMed retrieval.
4. Review reports/, data/state/, and data/papers/.

The scheduler selects enabled journals with the oldest successful retrieval history. New journals without history are prioritized. The PubMed Create Date (crdt) field determines when an article enters a retrieval report.

## Phase 2: Ollama abstract summarization

The benchmark is intentionally separate from weekly retrieval. A model failure cannot interrupt PubMed collection, and benchmarking only runs when manually requested. It currently uses Ollama only; no Gemini API key or paid LLM API quota is needed.

### Model and prompt

- Local model through Ollama: qwen3:4b, running on the GitHub Actions CPU runner.
- Versioned prompt: prompts/paper_summary_v1.txt.
- Default sample: 12 abstracts, sampled reproducibly and approximately evenly across journals.
- Ollama JSON output mode is enabled and Qwen3 thinking output is disabled so the benchmark captures the final answer.

The prompt requests a concise summary, key findings, study type, relevance to biomaterials and regenerative medicine, and limitations or unknowns. It instructs the model not to infer facts beyond the abstract.

The model name, generation settings, sample size, and random seed are editable in config/llm.yml. Ollama elapsed time and token counts are recorded where available.

### Run the benchmark

1. Open Actions → Ollama benchmark.
2. Select Run workflow.
3. Keep the default sample size of 12, or enter a different positive integer.
4. Wait for the job to finish.

The workflow installs Ollama and downloads the selected model, so the first run may take several minutes. The model runs on the GitHub Actions CPU runner. No API key is required.

### Outputs

Each run is saved in a unique folder under data/benchmarks/:

- benchmark.json: sampled paper metadata, prompt, Ollama output, token counts, timings, and errors.
- metrics.csv: one row per paper, for easy review or spreadsheet analysis.
- summaries.md: formatted Ollama summaries for manual scientific review.

Ollama runs locally on the GitHub Actions runner, so this workflow does not make paid LLM API calls. GitHub Actions usage limits still apply.

The benchmark does not automatically assess scientific accuracy. Review factual accuracy, omitted findings, unsupported claims, usefulness, and reading time. Human review is important for scientific reliability.

## Data layout

- config/journals.yml: monitored journal registry and retrieval settings.
- config/llm.yml: Ollama model, sampling, and generation settings.
- prompts/: versioned prompts.
- data/state/: per-journal successful retrieval cursors.
- data/papers/: deduplicated paper index.
- reports/: immutable PubMed retrieval reports.
- data/benchmarks/: immutable benchmark runs.

## Local development

Install the R dependencies with install.packages, then run:

- Rscript tests/testthat.R
- Rscript scripts/run_retrieval.R
- Rscript scripts/run_benchmark.R (requires a running Ollama server and the configured model)

## Roadmap

1. PubMed retrieval, per-journal cursors, immutable JSON reports (complete).
2. Ollama-only abstract summarization benchmark (implemented; next run will validate the provider fixes).
3. Quarto website and GitHub Pages deployment.
4. Monthly journal overview, search/filter, quality checks, and optional notifications.

## Operational notes

- GitHub scheduled workflow starts are best-effort and can be delayed.
- The Ollama benchmark is manual and separate from retrieval to control compute time.
