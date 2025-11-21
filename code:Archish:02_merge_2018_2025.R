# =============================================================
# Script: 02_merge_2018_2025.R
# Author: Archish Prakhya
# Date: November 2025
# Purpose: Merge the 2018–2020 combined dataset with 2021–2025 CSV data
# Output: data/processed/birds_2018_2025.csv
#
# How to run:
#   – First run: source("code/Archish/01_combine_2018_2020.R")
#   – Ensure data/Combined_2018_2020.csv exists
#   – Ensure the yearly folders data/2021 ... data/2025 contain raw CSVs
#   – Then run: source("code/Archish/02_merge_2018_2025.R")
#
# Notes:
#   - Reads 2018–2020 combined CSV + all 2021–2025 CSV datasets
#   - Standardizes column names and harmonizes key fields
#   - Infers missing years from filenames when needed
#   - Prints QA summary + yearly summary table
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

# ---- Step 1: Define file paths ----------------------------------------------

path_18_20 <- "data/Combined_2018_2020.csv"
year_dirs  <- file.path("data", 2021:2025)

# Check that required folders exist
stopifnot(all(dir.exists(year_dirs)))

out_path <- "data/processed/birds_2018_2025.csv"

# ---- Step 2: Load datasets --------------------------------------------------

cat("📥 Reading 2018–2020 combined dataset...\n")
d_18_20 <- read_csv(path_18_20, guess_max = 100000) |> 
  clean_names()

# Gather all CSV files from yearly subfolders
csvs_21_25 <- unlist(
  lapply(year_dirs, list.files, pattern = "\\.csv$", full.names = TRUE)
)

cat("📂 Found", length(csvs_21_25), "annual CSV files for 2021–2025\n")

read_safe_csv <- safely(read_csv, otherwise = NULL)

d_21_25 <- map_dfr(csvs_21_25, function(file) {
  cat("Reading:", basename(file), "\n")
  res <- read_safe_csv(file, guess_max = 100000)
  if (is.null(res$result)) {
    warning("Failed to read:", file)
    return(tibble())
  }
  df <- res$result |>
    clean_names()
  df$source_file <- basename(file)
  df
})

# ---- Step 3: Standardize & harmonize columns --------------------------------

coalesce_many <- function(df, cols) {
  reduce(cols[cols %in% names(df)], ~ coalesce(.x, .y))
}

parse_dt <- function(x) {
  x <- as.character(x)
  
  # Attempt ymd_hms
  dt <- suppressWarnings(ymd_hms(x, tz = "UTC"))
  
  # Try mdy_hms for failures
  need <- is.na(dt)
  if (any(need)) {
    dt[need] <- suppressWarnings(mdy_hms(x[need], tz = "UTC"))
  }
  
  # Final fallback: date only
  need2 <- is.na(dt)
  if (any(need2)) {
    dt[need2] <- suppressWarnings(ymd(x[need2]))
  }
  
  dt
}

standardize_cols <- function(df) {
  
  # Combine potential datetime columns
  datetime_chr <- coalesce_many(df, c(
    "date", "observation_date",
    "time", "starting_time"
  ))
  
  dt <- parse_dt(datetime_chr)
  
  df <- df |>
    mutate(
      date = if_else(!is.na(dt), as_date(dt), suppressWarnings(as_date(date))),
      year = coalesce(
        suppressWarnings(as.numeric(year)),
        year(dt),
        parse_integer(str_extract(source_file, "(19|20)\\d{2}"))
      )
    )
  
  # Normalize inconsistent column names
  if (!"observer" %in% names(df)) {
    candidates <- intersect(c("observers", "observer_name"), names(df))
    if (length(candidates) > 0)
      df <- df |> rename(observer = !!sym(candidates[1]))
  }
  
  if (!"repeat_observation" %in% names(df)) {
    candidates <- intersect(c("repeat", "repeat_obs"), names(df))
    if (length(candidates) > 0)
      df <- df |> rename(repeat_observation = !!sym(candidates[1]))
  }
  
  # Reorder key variables first for consistency
  keep_first <- c(
    "date", "year", "common_name", "scientific_name",
    "count", "observer", "repeat_observation",
    "substrate", "source_file"
  )
  
  df |> select(any_of(keep_first), everything())
}

cat("⚙️ Harmonizing datasets...\n")
d_18_20_clean <- standardize_cols(d_18_20)
d_21_25_clean <- standardize_cols(d_21_25)

# ---- Step 4: Merge & QA summary ---------------------------------------------

birds_2018_2025 <- bind_rows(d_18_20_clean, d_21_25_clean)

qa_summary <- birds_2018_2025 |>
  summarise(
    total_rows      = n(),
    missing_counts  = sum(is.na(count)),
    negative_counts = sum(count < 0, na.rm = TRUE)
  )

cat("\nℹ️ QA summary for merged 2018–2025 data:\n")
print(qa_summary)

# Yearly summary table
yearly_summary <- birds_2018_2025 |>
  group_by(year) |>
  summarise(
    unique_survey_days = n_distinct(date),
    total_records      = n(),
    total_individuals  = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year)

cat("\n📊 Yearly survey summary (2018–2025):\n")
print(yearly_summary, n = 10)

# ---- Step 5: Save final merged dataset --------------------------------------

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_csv(birds_2018_2025, out_path)

cat("\n💾 Saved final merged dataset to:", out_path, "\n")
