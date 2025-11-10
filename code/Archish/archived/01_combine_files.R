#example workflow for combining csvs

#Gemini AI overview suggestion: 

#load tidyverse into R session.
library(tidyverse)

# Specify the path to the directory containing your CSV files

files_path <- "path/to/your/csv/files" # Replace with your actual path

#Use list.files() to get a vector of all CSV file names within the specified directory. 
#The full.names = TRUE argument ensures that the full file paths are returned, which is necessary for reading the files.

csv_files <- list.files(path = files_path, pattern = "\\.csv$", full.names = TRUE)

#Employ map_dfr() from the purrr package (part of tidyverse) along with read_csv() to read each CSV file into a data frame and then combine them into a single data frame. 
#map_dfr() is particularly useful as it automatically binds the rows of the resulting data frames.

combined_df <- csv_files %>% map_dfr(read_csv)

