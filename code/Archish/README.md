# Archish Prakhya – Campus Lagoon Bird Survey Workflow

This directory contains scripts used to process and visualize UCSB Campus Lagoon bird survey data from **2018–2025**.  
The scripts are ordered by workflow stages, from raw data to cleaned dataset outputs ready for analysis and visualization.

---

## 🧩 Overview of Scripts

### `01_combine_2018_2020.R`
**Purpose:**  
Combines all individual CSV files from the `data/2018`, `data/2019`, and `data/2020` folders into a single dataset.  

**Highlights:**
- Uses `dplyr`, `readr`, `stringr`, `janitor`, and `purrr` for efficient functional-style merging.  
- Automatically cleans column names and adds file-source metadata.  
- Performs a light QA summary for missing or inconsistent records.  

**Output:**  
`data/Combined_2018_2020.csv`

---

### `02_merge_2018_2025.R`
**Purpose:**  
Merges the combined 2018–2020 file with all data from **2021–2025** to create one unified dataset for visualization and trend analysis.  

**Highlights:**
- Harmonizes column names and date formats across all years.  
- Extracts missing year values from filenames when needed.  
- Produces console QA summaries and yearly aggregation tables for verification.    

**Output:**  
`data/processed/birds_2018_2025.csv`

---
