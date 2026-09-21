# Blog 2 replication package

This folder reproduces the analysis for **One Policy, Two Mandates, Three Treasury Reactions**.

## Structure

- `code/` contains the complete R Markdown analysis and the generated R script.
- `data/raw/` contains the official public-page snapshots parsed by `rvest`.
- `results/` contains the cleaned CSV files, figures, checks, source register, output fingerprints, and R session information.
- `replication-guide.html` is the complete rendered technical walkthrough.
- `PROVENANCE.md` documents data lineage and validation.

## Reproduce the results

1. Install R packages: `rvest`, `xml2`, `dplyr`, `tidyr`, `stringr`, `lubridate`, `ggplot2`, `digest`, `knitr`, and `rmarkdown`.
2. Clone `https://github.com/xia071212/blog2repo.git` and open a terminal in the repository root.
3. Run `Rscript code/Blog2_Treasury_Yields.R` to rebuild the CSV and PNG outputs from the saved page snapshots.
4. To rebuild the full guide, run `Rscript -e "rmarkdown::render('code/Blog2_Treasury_Yields.Rmd', output_file='replication-guide.html', output_dir=getwd())"` from the repository root. Install Pandoc or run this from RStudio, which bundles Pandoc.
5. Compare the regenerated files with `results/output_manifest.csv` and review `results/data_checks.csv`.

Delete the relevant file in `data/raw/` before a run only when a fresh public-page request is intended. The scraper pauses between new requests and otherwise uses the local snapshots.

## Core outputs

- `results/policy_events_rvest.csv`: 18 verified rate changes and their official statements.
- `results/macro_conditions.csv`: CPI index, 12-month CPI inflation, and unemployment.
- `results/daily_yields_with_events.csv`: daily 2-, 10-, and 30-year yields with policy markers.
- `results/announcement_day_yield_changes.csv`: one-day yield changes around announcements.
- `results/yield_response_summary.csv`: maturity-level response statistics.
- `results/source_log.csv`: source URLs and snapshot fingerprints.

Read the [published Blog 2](https://xia071212.github.io/myrepo/blog/posts/post2/index.html). Its article source is maintained separately in [the website repository](https://github.com/xia071212/myrepo/blob/main/blog/posts/post2/index.qmd).
