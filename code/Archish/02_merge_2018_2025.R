# =============================================================
# Script: 02_merge_2018_2025.R
# Author: Archish Prakhya
# Date: November 2025
# Purpose: Merge the 2018–2020 combined dataset with 2021–2025 data
# Output: data/processed/birds_2018_2025.csv
#
# How to run:
#  - Make sure 01_combine_2018_2020.R has been run first
#  - This will read data/Combined_2018_2020.csv and merge with all CSVs
#    under data/2021–data/2025
#  - Run from the project root: source("code/Archish/02_merge_2018_2025.R")
#
# Notes:
#  - Standardizes column names & date formats automatically
#  - Adds missing years from filenames when needed
#  - Prints yearly survey summary to console for review
# =============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(janitor)
  library(purrr)
  library(tidyr)
})

# ---- Step 1: Define file paths ----
path_18_20 <- "data/Combined_2018_2020.csv"
year_dirs  <- file.path("data", 2021:2025)
stopifnot(all(dir.exists(year_dirs)))

out_path <- "data/processed/birds_2018_2025.csv"

# ---- Step 2: Load datasets ----
cat("📥 Reading 2018–2020 combined file...\n")
d_18_20 <- read_csv(path_18_20, guess_max = 100000) |> clean_names()

csvs_21_25 <- unlist(lapply(year_dirs, list.files, pattern = "\\.csv$", full.names = TRUE))
cat("📂 Found", length(csvs_21_25), "files for 2021–2025\n")

read_safe <- safely(read_csv, otherwise = NULL)
d_21_25 <- map_dfr(csvs_21_25, function(file) {
  cat("Reading:", basename(file), "\n")
  res <- read_safe(file, guess_max = 100000)
  if (is.null(res$result)) return(tibble())
  df <- res$result |> clean_names()
  df$source_file <- basename(file)
  df
})

# ---- Step 3: Standardize columns ----
coalesce_many <- function(df, cols)
  reduce(cols[cols %in% names(df)], ~ coalesce(df[[.x]], df[[.y]]))

parse_dt <- function(x) {
  x <- as.character(x)
  dt <- suppressWarnings(ymd_hms(x, tz = "UTC"))
  need <- is.na(dt)
  if (any(need)) dt[need] <- suppressWarnings(mdy_hms(x[need], tz = "UTC"))
  if (any(is.na(dt))) dt[is.na(dt)] <- suppressWarnings(ymd(x[is.na(dt)]))
  dt
}

standardize_cols <- function(df) {
  datetime_chr <- coalesce_many(df, c("date", "observation_date", "time", "starting_time"))
  dt <- parse_dt(datetime_chr)
  df <- df |>
    mutate(
      date = if_else(!is.na(dt), as_date(dt), suppressWarnings(as_date(date))),
      year = if_else(!is.na(year), as.numeric(year), year(dt)),
      year_from_file = parse_integer(str_extract(source_file, "(19|20)\\d{2}")),
      year = coalesce(year, year_from_file)
    )
  
  # Clean up column naming differences
  if (!"observer" %in% names(df)) {
    cand <- intersect(c("observers", "observer_name"), names(df))
    if (length(cand)) df <- df |> rename(observer = !!sym(cand[1]))
  }
  if (!"repeat_observation" %in% names(df)) {
    cand <- intersect(c("repeat", "repeat_obs"), names(df))
    if (length(cand)) df <- df |> rename(repeat_observation = !!sym(cand[1]))
  }
  
  keep_first <- c("date", "year", "common_name", "scientific_name",
                  "count", "observer", "repeat_observation", "substrate", "source_file")
  df |> select(any_of(keep_first), everything())
}

cat("⚙️ Cleaning and harmonizing columns...\n")
d_18_20_clean <- standardize_cols(d_18_20)
d_21_25_clean <- standardize_cols(d_21_25)

# ---- Step 4: Merge + summarize ----
birds_2018_2025 <- bind_rows(d_18_20_clean, d_21_25_clean)

qa_summary <- birds_2018_2025 |>
  summarise(
    total_rows = n(),
    missing_counts = sum(is.na(count)),
    negative_counts = sum(count < 0, na.rm = TRUE)
  )
print(qa_summary)

yearly_summary <- birds_2018_2025 |>
  group_by(year) |>
  summarise(
    unique_survey_days = n_distinct(date),
    total_records = n(),
    total_individuals = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year)

cat("\n📊 Yearly summary (2018–2025):\n")
print(yearly_summary, n = 10)

# ---- Step 5: Save output ----
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_csv(birds_2018_2025, out_path)
cat("\n💾 Saved final merged dataset to:", out_path, "\n")
