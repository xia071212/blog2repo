# Blog 2 replication: run this script to rebuild results from saved snapshots.
# Yields are percentages; daily changes are basis points, not bond returns.
# 1. Setup: packages, paths, and output directories
# ------------------------------------------------------------------------
set.seed(6400)

library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

# Resolve paths when run with Rscript, Source in RStudio, or from the project root.
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
source_files <- unlist(lapply(sys.frames(), function(frame) frame$ofile))
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[1]) else
  if (length(source_files)) tail(source_files, 1) else NA_character_
script_path <- gsub("~+~", " ", script_path, fixed = TRUE)
project_dir <- if (!is.na(script_path)) dirname(dirname(normalizePath(script_path))) else
  normalizePath(".")
stopifnot(file.exists(file.path(project_dir, "code", "Blog2_Treasury_Yields.R")))
raw_dir <- file.path(project_dir, "data", "raw")
result_dir <- Sys.getenv("BLOG2_RESULTS_DIR", unset = file.path(project_dir, "results"))
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
options(timeout = 120)
source_log <- list()

# 2. Read and clean data: fixed sample and official source URLs
# ------------------------------------------------------------------------
sample_start <- as.Date("2022-01-01")
sample_end <- as.Date("2026-09-19")

treasury_url <- function(year) {
  paste0(
    "https://home.treasury.gov/resource-center/data-chart-center/interest-rates/TextView",
    "?type=daily_treasury_yield_curve&field_tdr_date_value=", year
  )
}
archive_url <- paste0(
  "https://home.treasury.gov/resource-center/data-chart-center/interest-rates/",
  "daily-treasury-rate-archives/par-yield-curve-render-2020-2023.xml"
)
policy_url <- "https://www.federalreserve.gov/monetarypolicy/openmarket.htm"
calendar_url <- "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"
unemployment_url <- "https://data.bls.gov/timeseries/LNS14000000"
cpi_url <- "https://data.bls.gov/timeseries/CUSR0000SA0"

# 2a. Parse saved public pages; request only missing snapshots
# ------------------------------------------------------------------------
read_page <- function(address, filename, format = "HTML") {
  snapshot <- file.path(raw_dir, filename)

  if (file.exists(snapshot)) {
    page <- rvest::read_html(snapshot)
  } else {
    page <- rvest::read_html(url(address, method = "libcurl"))
    xml2::write_html(page, snapshot)
    Sys.sleep(2)
  }

  # Record the source URL and fingerprint of the document actually parsed.
  source_log[[filename]] <<- data.frame(
    source_url = address,
    format = format,
    snapshot = paste0("data/raw/", filename),
    snapshot_sha256 = digest::digest(file = snapshot, algo = "sha256")
  )
  page
}

page <- read_page(treasury_url(2026), "treasury_2026.html")

# 2b. Identify the Treasury table by its required columns
# ------------------------------------------------------------------------
tables <- page |>
  html_elements("table") |>
  html_table(fill = TRUE)

required_columns <- c("Date", "2 Yr", "10 Yr", "30 Yr")
matches <- vapply(
  tables,
  function(table) all(required_columns %in% names(table)),
  logical(1)
)
stopifnot(sum(matches) == 1)

# 2c. Extract recent Treasury yields
# ------------------------------------------------------------------------
extract_year <- function(year) {
  filename <- paste0("treasury_", year, ".html")
  page <- read_page(treasury_url(year), filename)
  tables <- html_table(page, fill = TRUE)
  matches <- vapply(tables, function(x) {
    all(required_columns %in% names(x))
  }, logical(1))
  stopifnot(sum(matches) == 1)

  tables[[which(matches)]] |>
    select(all_of(required_columns)) |>
    transmute(
      date = mdy(Date),
      yield_2y_pct = as.character(`2 Yr`),
      yield_10y_pct = as.character(`10 Yr`),
      yield_30y_pct = as.character(`30 Yr`),
      yield_source = paste0("data/raw/", filename)
    )
}

recent_yields <- bind_rows(lapply(2024:2026, extract_year))

# 2d. Extract named fields from the historical XML archive
# ------------------------------------------------------------------------
archive <- read_page(
  archive_url,
  "treasury_2020_2023_archive.html",
  format = "Static XML archive"
)
entries <- html_elements(archive, "entry")
stopifnot(length(entries) > 0)

archive_field <- function(field_name) {
  entries |>
    html_element(xpath = paste0(".//*[name()='", field_name, "']")) |>
    html_text2()
}

historical_yields <- tibble(
  date = as.Date(substr(archive_field("d:new_date"), 1, 10)),
  yield_2y_pct = archive_field("d:bc_2year"),
  yield_10y_pct = archive_field("d:bc_10year"),
  yield_30y_pct = archive_field("d:bc_30year"),
  yield_source = "data/raw/treasury_2020_2023_archive.html"
)

# 2e. Convert dates and numbers; reject duplicates and malformed values
# ------------------------------------------------------------------------
clean_number <- function(x) {
  x <- str_squish(as.character(x))
  x[x %in% c("", ".", "N/A", "NA")] <- NA_character_
  number <- suppressWarnings(as.numeric(x))
  if (any(!is.na(x) & is.na(number))) stop("Unexpected numeric text.")
  number
}

all_yields <- bind_rows(historical_yields, recent_yields)
stopifnot(!anyNA(all_yields$date), !anyDuplicated(all_yields$date))
yields <- all_yields |>
  filter(date >= sample_start, date <= sample_end) |>
  mutate(across(starts_with("yield_") & ends_with("_pct"), clean_number)) |>
  arrange(date)

yield_checks <- yields |>
  summarise(
    observations = n(),
    first_date = min(date),
    last_date = max(date),
    across(ends_with("_pct"), ~ sum(is.na(.x)), .names = "missing_{.col}")
  )

# 2f. Extract rate changes and effective dates from policy-history tables
# ------------------------------------------------------------------------
policy_page <- read_page(policy_url, "fed_openmarket.html")
policy_tables <- html_elements(policy_page, "table")

extract_changes <- function(table_node) {
  year_text <- table_node |>
    html_element(xpath = "preceding::h4[1]") |>
    html_text2() |>
    str_squish()
  if (is.na(year_text) || !str_detect(year_text, "^20[0-9]{2}$")) return(NULL)
  year <- as.integer(year_text)
  if (year < 2020 || year > lubridate::year(sample_end)) return(NULL)

  table <- html_table(table_node)
  stopifnot(all(c("Date", "Increase", "Decrease", "Level (%)") %in% names(table)))
  bounds <- str_split_fixed(str_replace_all(table$`Level (%)`, "[–—−‑]", "-"), "-", 2)

  tibble(
    effective_date = mdy(paste(table$Date, year)),
    change_bp = clean_number(table$Increase) - clean_number(table$Decrease),
    target_lower_pct = clean_number(bounds[, 1]),
    target_upper_pct = clean_number(bounds[, 2]),
    policy_source = "data/raw/fed_openmarket.html"
  )
}

policy_history <- bind_rows(lapply(policy_tables, extract_changes)) |>
  filter(effective_date <= sample_end, change_bp != 0) |>
  mutate(decision = if_else(change_bp > 0, "Hike", "Cut")) |>
  arrange(effective_date)

changes <- policy_history |>
  filter(effective_date >= sample_start)

stopifnot(!anyNA(changes$effective_date), !anyDuplicated(changes$effective_date),
          !anyNA(changes$change_bp))

# 2g. Extract statement links; match within seven days before effectiveness
# ------------------------------------------------------------------------
calendar <- read_page(calendar_url, "fed_calendar.html")
links <- calendar |>
  html_elements("a") |>
  html_attr("href")
links <- unique(links[!is.na(links)])
links <- links[str_detect(links, "/pressreleases/monetary[0-9]{8}a\\.htm$")]

statements <- tibble(
  statement_url = xml2::url_absolute(links, calendar_url),
  link_date = ymd(str_extract(links, "[0-9]{8}"))
) |>
  filter(link_date >= sample_start, link_date <= sample_end)

find_statement <- function(effective_date) {
  candidates <- statements |>
    filter(link_date <= effective_date, link_date >= effective_date - 7)
  if (nrow(candidates) != 1) stop("Policy change does not have one matching statement.")
  candidates
}
matched_statements <- bind_rows(lapply(changes$effective_date, find_statement))
changes <- bind_cols(changes, matched_statements)

# 2h. Verify printed publication dates and the direction in decision text
# ------------------------------------------------------------------------
read_statement <- function(i) {
  filename <- paste0("fed_statement_", format(changes$link_date[i], "%Y%m%d"), ".html")
  page <- read_page(changes$statement_url[i], filename)

  date_text <- page |>
    html_element(".article__time") |>
    html_text2()
  announcement_date <- mdy(str_squish(date_text))

  paragraphs <- page |>
    html_elements("p") |>
    html_text2() |>
    str_squish()
  action <- paragraphs[str_detect(
    paragraphs, regex("decided to (raise|increase|lower|reduce) the target range", ignore_case = TRUE)
  )]
  if (length(action) != 1) stop("Cannot identify one rate-decision paragraph.")
  action_direction <- if (str_detect(action, "decided to (raise|increase)")) "Hike" else "Cut"
  stopifnot(!is.na(announcement_date), announcement_date == changes$link_date[i],
            action_direction == changes$decision[i])

  tibble(
    announcement_date = announcement_date,
    statement_source = paste0("data/raw/", filename),
    decision_evidence = action
  )
}

verified <- bind_rows(lapply(seq_len(nrow(changes)), read_statement))
events <- bind_cols(changes, verified) |>
  select(announcement_date, effective_date, decision, change_bp,
         target_lower_pct, target_upper_pct, statement_url,
         policy_source, statement_source, decision_evidence) |>
  arrange(announcement_date)

stopifnot(!anyDuplicated(events$announcement_date),
          all(events$announcement_date <= events$effective_date))

# 2i. Read monthly CPI and unemployment; calculate twelve-month inflation
# ------------------------------------------------------------------------
extract_bls_series <- function(address, filename, value_name) {
  page <- read_page(address, filename)
  tables <- html_table(page, fill = TRUE)
  matches <- vapply(tables, function(table) {
    all(c("Year", month.abb) %in% names(table))
  }, logical(1))
  stopifnot(sum(matches) == 1)

  table <- tables[[which(matches)]] |>
    select(Year, all_of(month.abb)) |>
    filter(str_detect(as.character(Year), "^[0-9]{4}$")) |>
    pivot_longer(all_of(month.abb), names_to = "month", values_to = "raw_value") |>
    mutate(
      date = make_date(as.integer(Year), match(month, month.abb), 1L),
      value = as.numeric(str_extract(str_squish(as.character(raw_value)),
                                     "^[0-9]+(?:\\.[0-9]+)?"))
    ) |>
    select(date, value)

  names(table)[2] <- value_name
  table
}

cpi <- extract_bls_series(cpi_url, "bls_cpi.html", "cpi_index") |>
  arrange(date) |>
  mutate(inflation_yoy_pct = 100 * (cpi_index / lag(cpi_index, 12) - 1))

unemployment <- extract_bls_series(
  unemployment_url,
  "bls_unemployment.html",
  "unemployment_pct"
)

macro <- full_join(cpi, unemployment, by = "date") |>
  filter(date >= sample_start, date <= sample_end) |>
  arrange(date)
last_macro_date <- macro |>
  filter(!is.na(inflation_yoy_pct) | !is.na(unemployment_pct)) |>
  summarise(last_date = max(date)) |>
  pull(last_date)
macro <- macro |>
  filter(date <= last_macro_date)

macro_checks <- macro |>
  summarise(
    observations = n(),
    first_date = min(date),
    last_date = max(date),
    missing_inflation = sum(is.na(inflation_yoy_pct)),
    missing_unemployment = sum(is.na(unemployment_pct))
  )

# 3. Merge daily yields with verified announcement records
# ------------------------------------------------------------------------
stopifnot(all(events$announcement_date %in% yields$date))
daily <- yields |>
  left_join(events, by = c("date" = "announcement_date")) |>
  arrange(date)
stopifnot(nrow(daily) == nrow(yields), sum(!is.na(daily$decision)) == nrow(events))

# 4. Analyze announcement-day changes and summarize by maturity
# ------------------------------------------------------------------------
yield_changes <- yields |>
  arrange(date) |>
  mutate(
    change_2y_bp = 100 * (yield_2y_pct - lag(yield_2y_pct)),
    change_10y_bp = 100 * (yield_10y_pct - lag(yield_10y_pct)),
    change_30y_bp = 100 * (yield_30y_pct - lag(yield_30y_pct))
  )

event_moves <- events |>
  select(announcement_date, decision, policy_change_bp = change_bp) |>
  left_join(yield_changes, by = c("announcement_date" = "date")) |>
  pivot_longer(
    cols = c(change_2y_bp, change_10y_bp, change_30y_bp),
    names_to = "tenor",
    values_to = "yield_change_bp"
  ) |>
  mutate(
    tenor = factor(
      tenor,
      levels = c("change_2y_bp", "change_10y_bp", "change_30y_bp"),
      labels = c("2-year", "10-year", "30-year")
    ),
    absolute_change_bp = abs(yield_change_bp)
  )

response_summary <- event_moves |>
  group_by(tenor) |>
  summarise(
    meetings = n(),
    mean_absolute_change_bp = mean(absolute_change_bp, na.rm = TRUE),
    median_absolute_change_bp = median(absolute_change_bp, na.rm = TRUE),
    maximum_absolute_change_bp = max(absolute_change_bp, na.rm = TRUE),
    .groups = "drop"
  )

stopifnot(!anyNA(event_moves$yield_change_bp))

# 5a. Draw and save daily yield levels
# ------------------------------------------------------------------------
long <- yields |>
  pivot_longer(ends_with("_pct"), names_to = "tenor", values_to = "yield_pct") |>
  mutate(tenor = factor(tenor,
    levels = c("yield_2y_pct", "yield_10y_pct", "yield_30y_pct"),
    labels = c("2-year", "10-year", "30-year")))
markers <- events |>
  mutate(label = sprintf("%+d", as.integer(change_bp)),
         decision = factor(decision, levels = c("Hike", "Cut")))
top <- max(long$yield_pct, na.rm = TRUE)

yield_plot <- ggplot(long, aes(date, yield_pct, colour = tenor)) +
  geom_vline(data = markers,
    aes(xintercept = announcement_date, linetype = decision),
    colour = "#A4ACB5", linewidth = 0.4) +
  geom_line(linewidth = 0.65, na.rm = TRUE) +
  geom_text(data = markers,
    aes(x = announcement_date, y = top + 0.4, label = label),
    inherit.aes = FALSE, angle = 90, size = 3, colour = "#546274") +
  scale_colour_manual(values = c("#2459A6", "#15958C", "#D77C43")) +
  scale_linetype_manual(values = c(Hike = "dashed", Cut = "dotted")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(NA, top + 0.8)) +
  labs(title = "U.S. Treasury yields and Fed rate decisions",
    subtitle = "Daily yields; vertical lines mark announcement dates",
    x = NULL, y = "Yield (%)", colour = NULL, linetype = "Fed decision",
    caption = paste0("Sources: U.S. Treasury and Federal Reserve.\n",
      "Top labels show the policy change in basis points.")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0))

yield_plot <- yield_plot + guides(colour = guide_legend(order = 1),
                                  linetype = guide_legend(order = 2))

ggsave(file.path(result_dir, "daily_yields_with_events.png"), yield_plot,
       width = 14, height = 7.4, dpi = 160, bg = "white")

# 5b. Draw and save policy, inflation, and unemployment
# ------------------------------------------------------------------------
macro_long <- macro |>
  select(date, inflation_yoy_pct, unemployment_pct) |>
  pivot_longer(-date, names_to = "indicator", values_to = "percent") |>
  mutate(indicator = factor(
    indicator,
    levels = c("inflation_yoy_pct", "unemployment_pct"),
    labels = c("CPI inflation, 12-month change", "Unemployment rate")
  ))

initial_policy <- policy_history |>
  filter(effective_date < sample_start) |>
  slice_max(effective_date, n = 1, with_ties = FALSE)
stopifnot(nrow(initial_policy) == 1)

policy_path <- bind_rows(
  initial_policy |>
    transmute(
      date = sample_start,
      target_midpoint_pct = (target_lower_pct + target_upper_pct) / 2,
      decision = NA_character_
    ),
  policy_history |>
    filter(effective_date >= sample_start) |>
    transmute(
      date = effective_date,
      target_midpoint_pct = (target_lower_pct + target_upper_pct) / 2,
      decision
    )
)
policy_points <- policy_path |>
  filter(!is.na(decision))

macro_plot <- ggplot() +
  geom_line(
    data = macro_long,
    aes(date, percent, colour = indicator),
    linewidth = 0.9, na.rm = TRUE
  ) +
  geom_point(
    data = macro_long,
    aes(date, percent, colour = indicator),
    size = 1.5, na.rm = TRUE
  ) +
  geom_step(
    data = policy_path,
    aes(date, target_midpoint_pct, colour = "Federal funds target midpoint"),
    linewidth = 0.85, direction = "hv"
  ) +
  geom_point(
    data = policy_points,
    aes(date, target_midpoint_pct, shape = decision),
    colour = "#4D4D4D", size = 2.2
  ) +
  scale_colour_manual(
    values = c(
      "CPI inflation, 12-month change" = "#C4553D",
      "Unemployment rate" = "#31688E",
      "Federal funds target midpoint" = "#4D4D4D"
    ),
    breaks = c(
      "Federal funds target midpoint",
      "CPI inflation, 12-month change",
      "Unemployment rate"
    )
  ) +
  scale_shape_manual(values = c(Hike = 17, Cut = 16)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title = "Policy tightened after inflation surged, while unemployment stayed low",
    subtitle = "CPI is a 12-month change; the policy line is the midpoint of the target range",
    x = NULL, y = "Percent", colour = NULL, shape = "Decision",
    caption = paste0(
      "Sources: Federal Reserve and U.S. Bureau of Labor Statistics.\n",
      "October 2025 CPI and unemployment data are unavailable because of a lapse in appropriations."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top", legend.justification = "left",
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  )

macro_plot <- macro_plot + guides(colour = guide_legend(order = 2),
                                  shape = guide_legend(order = 1))

ggsave(file.path(result_dir, "inflation_unemployment_with_events.png"), macro_plot,
       width = 14, height = 6.8, dpi = 160, bg = "white")

# 5c. Draw and save absolute announcement-day changes
# ------------------------------------------------------------------------
response_plot <- ggplot(
  event_moves,
  aes(tenor, absolute_change_bp, colour = tenor)
) +
  geom_boxplot(aes(fill = tenor), width = 0.55, alpha = 0.14,
               outlier.shape = NA, show.legend = FALSE) +
  geom_point(
    aes(shape = decision),
    position = position_jitter(width = 0.10, height = 0, seed = 6400),
    size = 2.7, alpha = 0.82
  ) +
  scale_colour_manual(values = c("#2459A6", "#15958C", "#D77C43")) +
  scale_fill_manual(values = c("#2459A6", "#15958C", "#D77C43")) +
  labs(
    title = "Shorter-maturity yields moved more on Fed announcement days",
    subtitle = "Absolute change from the previous Treasury observation; each point is one announcement",
    x = NULL, y = "Absolute daily yield change (basis points)", colour = NULL,
    shape = "Decision",
    caption = paste0(
      "Sources: U.S. Treasury and Federal Reserve. ",
      "The chart describes announcement-day moves, not causal effects."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top", legend.justification = "left",
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  )

response_plot <- response_plot + guides(colour = guide_legend(order = 1),
                                        shape = guide_legend(order = 2))

ggsave(file.path(result_dir, "announcement_day_yield_changes.png"), response_plot,
       width = 14, height = 6.8, dpi = 160, bg = "white")

# 6. Save datasets, checks, and source records
# ------------------------------------------------------------------------
write.csv(daily, file.path(result_dir, "daily_yields_with_events.csv"), row.names = FALSE, na = "")
write.csv(events, file.path(result_dir, "policy_events_rvest.csv"), row.names = FALSE, na = "")
write.csv(macro, file.path(result_dir, "macro_conditions.csv"), row.names = FALSE, na = "")
write.csv(event_moves, file.path(result_dir, "announcement_day_yield_changes.csv"),
          row.names = FALSE, na = "")
write.csv(response_summary, file.path(result_dir, "yield_response_summary.csv"), row.names = FALSE)
data_checks <- bind_rows(
  yield_checks |> mutate(dataset = "Daily Treasury yields", .before = 1),
  macro_checks |> mutate(dataset = "Monthly macro indicators", .before = 1)
)
write.csv(data_checks, file.path(result_dir, "data_checks.csv"), row.names = FALSE)
source_register <- bind_rows(source_log)
write.csv(source_register, file.path(result_dir, "source_log.csv"), row.names = FALSE)

# 7. Export the four article tables from the same analysis objects
# ------------------------------------------------------------------------
save_table <- function(x, filename, digits = 1) {
  writeLines(as.character(knitr::kable(x, format = "pipe", digits = digits)),
             file.path(result_dir, filename))
}
save_table(events |> transmute(
  Announcement = format(announcement_date, "%b. %d, %Y"), Decision = decision,
  `Change (bp)` = sprintf("%+d", as.integer(change_bp)),
  `New target range (%)` = sprintf("%.2f–%.2f", target_lower_pct, target_upper_pct)
), "policy_schedule.md")
milestone_dates <- as.Date(c("2022-03-01", "2023-07-01", "2024-09-01", "2026-08-01"))
milestones <- macro[match(milestone_dates, macro$date), ]
month_ends <- ceiling_date(milestone_dates, "month") - days(1)
midpoints <- vapply(month_ends, function(d) {
  row <- policy_history |> filter(effective_date <= d) |> slice_tail(n = 1)
  (row$target_lower_pct + row$target_upper_pct) / 2
}, numeric(1))
save_table(data.frame(
  Milestone = c("First hike", "Last hike before easing", "First cut", "Latest macro data"),
  Month = format(milestone_dates, "%b. %Y"),
  `Policy midpoint (%)` = sprintf("%.2f", floor(midpoints * 100 + 0.5) / 100),
  `CPI inflation (%)` = sprintf("%.1f", milestones$inflation_yoy_pct),
  `Unemployment (%)` = sprintf("%.1f", milestones$unemployment_pct), check.names = FALSE
), "macro_milestones.md")
yield_summary <- bind_rows(lapply(c("2y", "10y", "30y"), function(tenor) {
  values <- yields[[paste0("yield_", tenor, "_pct")]]
  data.frame(Maturity = sub("y", "-year", tenor), `Start (%)` = values[1],
    `Sample peak (%)` = max(values), `Peak date` = format(yields$date[which.max(values)], "%b. %d, %Y"),
    `Latest (%)` = tail(values, 1), check.names = FALSE)
}))
save_table(yield_summary, "yield_levels.md", digits = 2)
save_table(response_summary |> transmute(
  Maturity = tenor, Meetings = meetings, `Mean absolute move (bp)` = mean_absolute_change_bp,
  `Median absolute move (bp)` = median_absolute_change_bp,
  `Maximum absolute move (bp)` = maximum_absolute_change_bp
), "yield_responses.md")

# Fingerprint the outputs, allowing later changes to be detected.
output_files <- c(
  "daily_yields_with_events.csv", "policy_events_rvest.csv",
  "macro_conditions.csv", "announcement_day_yield_changes.csv",
  "yield_response_summary.csv", "daily_yields_with_events.png",
  "inflation_unemployment_with_events.png", "announcement_day_yield_changes.png",
  "data_checks.csv", "source_log.csv", "policy_schedule.md",
  "macro_milestones.md", "yield_levels.md", "yield_responses.md"
)
output_manifest <- tibble(
  file = paste0("results/", output_files),
  sha256 = vapply(file.path(result_dir, output_files), function(path) {
    digest::digest(file = path, algo = "sha256")
  }, character(1))
)
write.csv(output_manifest, file.path(result_dir, "output_manifest.csv"), row.names = FALSE)
writeLines(trimws(capture.output(sessionInfo()), which = "right"),
           file.path(result_dir, "session_info.txt"))
