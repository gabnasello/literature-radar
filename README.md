# Literature Radar

Automated PubMed monitoring, Ollama-based paper summaries, and a compact journal-by-journal website.

## How it works

- **Retrieval:** every Sunday at 05:00 UTC, all enabled journals are queried using PubMed Create Date (`crdt`).
- **Tracking:** each journal has its own successful retrieval cursor; reports are immutable and papers are deduplicated by PMID.
- **Summaries:** Ollama generates summaries in batches of 24 after successful retrieval and every six hours while papers remain in the queue. Manual dispatch is also available.
- **Website:** GitHub Pages is rebuilt after retrieval, summarization, or relevant source changes.

The current registry contains 11 enabled journals, including Nature, Science, and Cell. New journals receive an initial 30-day retrieval window. Summary generation uses Ollama only; no external LLM API key is required.

## Configuration

- `config/journals.yml`: journal names, ISSNs, enabled status, and retrieval settings.
- `config/llm.yml`: Ollama model and generation settings.
- `prompts/`: versioned summary prompts.

## Workflows

- **Weekly PubMed retrieval:** scheduled for Sunday morning; can also be started manually.
- **Automatic paper summaries:** processes up to 24 papers per run, then continues every six hours.
- **Ollama benchmark:** optional manual workflow for evaluating a reproducible sample separately from production summaries.
- **Publish Quarto website:** renders and deploys the site to GitHub Pages.

## Website

The homepage lists only journals with completed AI summaries, ordered by latest retrieval date. Opening a journal shows its latest update immediately; older updates are available in a collapsed history. Each paper shows a short AI summary preview, with findings, study type, scientific significance, and the original abstract available in the expanded details.

The site is published at <https://gabnasello.github.io/literature-radar/>.

To enable GitHub Pages, open **Settings → Pages** and set the build source to **GitHub Actions**.

## Local development

Install the R dependencies, then run:

```sh
Rscript tests/testthat.R
Rscript scripts/run_retrieval.R
Rscript scripts/run_summarization.R
quarto render website
```
