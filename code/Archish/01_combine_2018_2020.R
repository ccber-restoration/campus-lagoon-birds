# =============================================================
# Script: 01_combine_2018_2020.R
# Author: Archish Prakhya
# Date: November 2025
# Purpose: Combine all Campus Lagoon CSVs from 2018–2020 into one dataset
# Output: data/Combined_2018_2020.csv
#
# How to run:
#  - Run from the project root (where campus-lagoon-birds.Rproj is located)
#  - Make sure the folders data/2018, data/2019, data/2020 exist
#  - Then just run: source("code/Archish/01_combine_2018_2020.R")
#
# Notes:
#  - I used dplyr/purrr for functional-style combining
#  - The code auto-cleans column names and records source filenames
#  - Quick QA summary printed at the end for sanity checking
# =============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(janitor)
  library(purrr)
})

# ---- Step 1: Identify source files ----
years <- 2018:2020
year_dirs <- file.path("data", years)
stopifnot(all(dir.exists(year_dirs)))

csv_files <- unlist(lapply(year_dirs, list.files, pattern = "\\.csv$", full.names = TRUE))
cat("📂 Found", length(csv_files), "CSV files from 2018–2020\n")

# ---- Step 2: Read and clean each file ----
read_safe <- safely(read_csv, otherwise = NULL)

raw_list <- map(csv_files, function(file) {
  cat("Reading:", basename(file), "\n")
  res <- read_safe(file, guess_max = 100000)
  if (is.null(res$result)) return(NULL)
  df <- res$result |> clean_names()
  df$source_file <- basename(file)
  df
})

combined_18_20 <- bind_rows(raw_list)
cat("✅ Combined all files. Rows so far:", nrow(combined_18_20), "\n")

# ---- Step 3: Light standardization ----
coalesce_many <- function(df, cols) reduce(cols[cols %in% names(df)], ~ coalesce(df[[.x]], df[[.y]]))

combined_18_20 <- combined_18_20 |>
  mutate(
    date = coalesce_many(cur_data(), c("date", "observation_date")),
    time = coalesce_many(cur_data(), c("time", "starting_time")),
    year = if_else(!is.na(year), as.numeric(year), year(as.Date(date)))
  )

# ---- Step 4: Quick Sanity check ----
qa_summary <- combined_18_20 |>
  summarise(
    total_rows = n(),
    missing_counts = sum(is.na(count)),
    negative_counts = sum(count < 0, na.rm = TRUE)
  )
print(qa_summary)

# ---- Step 5: Save output ----
out_path <- "data/Combined_2018_2020.csv"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_csv(combined_18_20, out_path)
cat("💾 Saved merged dataset to:", out_path, "\n")
