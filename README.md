# One Policy, Two Mandates, Three Treasury Reactions

Can Fed interest-rate policy balance inflation and employment, and do Treasury yields respond differently across maturities? This package reproduces the descriptive analysis in [Blog 2](https://xia071212.github.io/myrepo/blog/posts/post2/): the policy cycle, inflation and unemployment, and announcement-day changes in 2-, 10-, and 30-year Treasury yields.

## Data and the role of scraping

The analysis uses saved public documents in `data/raw/`. These HTML files are source-data snapshots required by the scraper, not rendered reports. `results/source_log.csv` records the 26 parsed documents’ exact source URLs, snapshot paths, formats, and SHA256 fingerprints. Source columns in the output datasets trace observations back to these snapshots.

| Source and saved files | Extracted information | Use |
|---|---|---|
| [Federal Reserve policy history](https://www.federalreserve.gov/monetarypolicy/openmarket.htm), `fed_openmarket.html` | Effective dates, increases/decreases, target-range bounds | Select rate changes and construct the policy-rate path |
| [FOMC calendar](https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm), `fed_calendar.html` | Links to policy statements | Locate the announcement corresponding to each rate change |
| `fed_statement_YYYYMMDD.html` | Printed publication date and decision paragraph | Verify announcement date and hike/cut direction; retain `statement_url` and `decision_evidence` |
| [Treasury daily tables](https://home.treasury.gov/resource-center/data-chart-center/interest-rates/TextView?type=daily_treasury_yield_curve&field_tdr_date_value=2026), `treasury_2024.html`–`treasury_2026.html`, and the [2020–2023 archive](https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rate-archives/par-yield-curve-render-2020-2023.xml), `treasury_2020_2023_archive.html` | Date and 2-, 10-, 30-year par yields | Daily yield levels and announcement-day changes |
| [BLS CPI](https://data.bls.gov/timeseries/CUSR0000SA0), `bls_cpi.html` | Seasonally adjusted CPI-U, CUSR0000SA0 | Twelve-month inflation |
| [BLS unemployment](https://data.bls.gov/timeseries/LNS14000000), `bls_unemployment.html` | Seasonally adjusted unemployment rate, LNS14000000 | Labor-market conditions |

The script uses `rvest` to extract Treasury yields, BLS macro series, and Federal Reserve policy records from the saved pages. Its key value is extracting FOMC announcement dates and decision text, allowing yield changes to be matched to the days policy news reaches markets. The numerical series themselves are also available as downloads.

A statement must be the unique calendar-linked candidate within seven days before the effective date. Its printed date must match the date in its link, and its decision direction must agree with the policy-history table. This checks dates and direction, not every detail of the statement's target range. The script preserves the supporting paragraph for inspection.

## Definitions and sample

The fixed sample starts on January 1, 2022 and ends on September 19, 2026. Available yields run through September 18, with 1,178 distinct dates; macro data run through August, with 56 monthly rows; there are 18 rate-changing announcements. Meetings without a rate change are excluded. October 2025 macro values remain missing, not filled or interpolated.

CPI inflation is 100 times the ratio of the current index to its value twelve months earlier, minus 100. The policy rate is the target-range midpoint. Yield changes subtract the previous available Treasury trading observation and multiply by 100 to express percentage-point differences in basis points. Response summaries describe absolute changes; yields and yield changes are not bond returns. Monthly milestone policy rates are measured at month end.

The figures describe associations. They do not isolate policy surprises or causal effects, and CPI is not the price index used for the Fed's formal inflation target.

## Code and outputs

`code/Blog2_Treasury_Yields.R` is the complete executable entry point. Its numbered stages set up paths, read and clean snapshots, match announcements, calculate changes, save figures, and export data and article tables. Paths are relative to the script's project directory; the output directory is created automatically.

| Output in `results/` | Connection to the blog |
|---|---|
| `policy_events_rvest.csv`, `policy_schedule.md` | Rate-change schedule, dates, and statement evidence |
| `macro_conditions.csv`, `macro_milestones.md`, `inflation_unemployment_with_events.png` | Four-stage discussion of inflation, employment, and policy |
| `daily_yields_with_events.csv`, `yield_levels.md`, `daily_yields_with_events.png` | Yield levels across maturities and the policy cycle |
| `announcement_day_yield_changes.csv`, `yield_response_summary.csv`, `yield_responses.md`, `announcement_day_yield_changes.png` | Absolute announcement-day changes by maturity |
| `data_checks.csv`, `source_log.csv`, `output_manifest.csv`, `session_info.txt` | Sample checks, document lineage, output fingerprints, and the actual R environment |

All four article tables are generated from the analysis objects. The website keeps deployment copies of these Markdown tables and the three figures. The R script is the sole analysis entry point. The blog’s decorative illustration is maintained in the website repository and is not a statistical output.

The code checks dates, duplicates, numeric conversion, missing values, statement direction, and announcement joins. Figure legend order is explicit so repeated runs preserve the original presentation.

## Reproduce in RStudio

1. Download the repository with its `code/`, `data/`, and `results/` directories intact, and open `blog2repo.Rproj` in RStudio.
2. Install the packages listed in `requirements.txt` if needed.
3. Open `code/Blog2_Treasury_Yields.R` and choose **Source** to run the complete workflow. The saved snapshots allow replication without new network requests.
4. Inspect the rebuilt figures, tables, and sample checks in `results/`. `session_info.txt` records the environment used; the output manifest records the files generated by that run.

Keep the supplied snapshots for the fixed-sample reproduction. If a snapshot is absent, the script requests its recorded official URL, saves the parsed page, and pauses two seconds. Live pages can change, so such a run is a refresh rather than an exact replication. No login, paywall, CAPTCHA, or other access restriction is bypassed.
