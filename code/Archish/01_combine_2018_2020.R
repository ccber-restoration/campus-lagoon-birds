# =============================================================
# Script: 01_combine_2018_2020.R
# Author: Archish Prakhya
# Date: November 2025
# Purpose: Combine the raw annual Excel files for 2018–2020 into one dataset
# Output: data/Combined_2018_2020.csv
#
# How to run:
#   – Run from the project root (where campus-lagoon-birds.Rproj is located)
#   – Ensure the raw Excel files for 2018–2020 are in data/from_Box/
#   – Then run: source("code/Archish/01_combine_2018_2020.R")
#
#
# Notes:
#   - Uses readxl, dplyr, purrr, janitor for a tidy workflow
#   - Cleans column names and tags each row with source file
#   - Performs some light date/year standardization
#   - Prints a quick QA summary at the end
# =============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(janitor)
  library(purrr)
})

# ---- Step 1: Identify raw Excel files in data/from_Box ----------------------

box_dir <- "data/from_Box"

# Pattern assumes filenames contain the year (2018/2019/2020) and end in .xlsx.
# If the actual filenames are different, you can adjust the pattern.
xlsx_files <- list.files(
  path       = box_dir,
  pattern    = "201(8|9|0).*\\.xlsx$",  # matches *2018*.xlsx, *2019*.xlsx, *2020*.xlsx
  full.names = TRUE
)

cat("📂 Found", length(xlsx_files), "annual Excel files for 2018–2020 in", box_dir, "\n")
if (length(xlsx_files) > 0) {
  print(basename(xlsx_files))
}

if (length(xlsx_files) == 0L) {
  warning(
    "No 2018–2020 Excel files were found in data/from_Box. ",
    "Please place the annual raw files there and re-run this script."
  )
  # Write an empty placeholder file so downstream code doesn't explode
  empty_df <- tibble()
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
  write_csv(empty_df, "data/Combined_2018_2020.csv")
  cat("💾 Saved empty Combined_2018_2020.csv (placeholder).\n")
  quit(save = "no")
}

# ---- Step 2: Read and clean each Excel file --------------------------------

read_safe_xlsx <- safely(read_excel, otherwise = NULL)



raw_list <- map(xlsx_files, function(file) {
  cat("Reading:", basename(file), "\n")
  res <- read_safe_xlsx(file)
  if (is.null(res$result)) {
    warning("Failed to read: ", file)
    return(NULL)
  }
  
  df <- res$result |>
    clean_names()
  
  df$source_file <- basename(file)
  df
})

combined_18_20 <- bind_rows(raw_list)
cat("✅ Combined Excel files. Rows so far:", nrow(combined_18_20), "\n")

# ---- Step 3: Light standardization (date / time / year) --------------------

combined_18_20 <- combined_18_20 |>
  mutate(
    # ensure date is date format
    date = as.Date(date),
    
    # force all time values to character (avoids datetime vs character issues)
    time = as.character(time),
    
    # extract year
    year = year(date)
  )
    
# ---- Step 4: Basic QA summary ----------------------------------------------

qa_summary <- combined_18_20 |>
  summarise(
    total_rows      = n(),
    missing_counts  = sum(is.na(count)),
    negative_counts = sum(count < 0, na.rm = TRUE)
  )

cat("ℹ️ QA summary for 2018–2020 combined data:\n")
print(qa_summary)

#no negative counts, but 60 missing.
#TODO - figure out what is going on with the missing counts and make plan to clean

#create dataframe filtered just to NA counts

missing_counts <- combined_18_20 %>% 
  filter(is.na(count)) %>% 
  filter(!(is.na(common_name)))

#49 rows have value in common name. Presumably those counts should be 1
#could use mutate & case_when to make correction

missing_species <- combined_18_20 %>% 
  filter(is.na(common_name))
#65 rows missing common_name
#some indicate the end of the survey
#in others the species was indicated in the observation_notes column

# ---- Step 5: Save output ---------------------------------------------------

out_path <- "data/Combined_2018_2020.csv"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_csv(combined_18_20, out_path)
cat("💾 Saved merged dataset to:", out_path, "\n")

