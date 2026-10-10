# The purpose of this script is to combine the existing data files for 2018-2020 calendar years, which exist as three separate Excel files


# 0. Setup ----
library(janitor)
library(tidyverse)
library(readxl)
library(naniar)
library(hms)
library(auk)

# load ebird taxonomy
ebird_taxonomy <- auk::ebird_taxonomy

# read in guild list
guilds <- read_csv("data/guilds/bird_spp_guild_assignments.csv")

# Read in survey data ----

# use guess_max equal to or greater than the number of observations to prevent read_xlsx()
# from guessing a logical column type (based on all NAs in the first 1000 rows, which is the default)

cl_surveys_2018 <- read_xlsx("data/2018/CL_BirdSurveyData_2018.xlsx", 
                             guess_max = 3000) |> 
  clean_names()

# 35 variables

cl_surveys_2019 <- read_xlsx("data/2019/CL_BirdSurveyData_2019.xlsx",
                             guess_max = 3000) |> 
  clean_names()

# 34 variables

cl_surveys_2020 <- read_xlsx("data/2020/CL_BirdSurveyData_2020.xlsx",
                             guess_max = 3000) |> 
  clean_names()

# 32 variables

# Compare data sets before trying to combine ----

# Compare columns
column_comparison <- janitor::compare_df_cols(cl_surveys_2018, cl_surveys_2019, cl_surveys_2020) |> 
  # filter to any column name not present in all of the data frames
  filter(if_any(everything(), is.na))

# okay so after 2018 a lot of the weather fields got renamed

# other issues:

# ebird group was not assigned in 2020
# globalid missing for 2019
# scientific name not present in 2020 (but blank in the other dfs, anyway)
# weather_note not in 2020 (just a couple notes in 2019)

mismatched_types <- janitor::compare_df_cols(cl_surveys_2018, cl_surveys_2019, cl_surveys_2020, return = "mismatch")

# There are also a handful of issues with discrepancies in column types, presumably due to all NAs

# Prep (clean) to combine ----

cl_surveys_2018_clean <- cl_surveys_2018 |> 
  # rename weather columns (new_name = old_name)
  rename(
    starting_wind_speed = wind_speed_start,
    starting_wind_direction = wind_direction_start,
    starting_time = time_start,
    starting_temp_f = temp_start_f,
    starting_rain = rain_start,
    starting_percent_cloud_cover = cloud_cover_percent_start,
    starting_cloud_height = cloud_height_start,
    ending_wind_speed = wind_speed_end,
    ending_wind_direction = wind_direction_end,
    ending_time = time_end,
    ending_temp_f = temp_end_f ,
    ending_rain = rain_end,
    ending_percent_cloud_cover = cloud_cover_percent_end,
    ending_cloud_height = cloud_height_end
  ) |> 
  select(-c(scientific_name, e_bird_group)) |> 
  mutate(weather_note = as.character(weather_note))

cl_surveys_2019_clean <- cl_surveys_2019 |> 
  select(-c(scientific_name, e_bird_group)) |> 
  mutate(global_id = NA,
         ending_rain = as.character(ending_rain),
         ending_wind_direction = as.character(ending_wind_direction),
         global_id = as.character(global_id))

cl_surveys_2020_clean <- cl_surveys_2020 |> 
  mutate(weather_note = NA) |> 
  mutate(weather_note = as.character(weather_note),
         ending_wind_direction = as.character(ending_wind_direction),
         starting_rain = as.character(starting_rain),
         starting_wind_direction = as.character(starting_wind_direction))

# recheck column names
janitor::compare_df_cols(cl_surveys_2018_clean, cl_surveys_2019_clean, cl_surveys_2020_clean) |> 
  # filter to any column name not present in all of the data frames
  filter(if_any(everything(), is.na))

# recheck column types
janitor::compare_df_cols(cl_surveys_2018_clean, cl_surveys_2019_clean, cl_surveys_2020_clean, return = "mismatch")

# Combine years ----
cl_surveys_2018_2020 <- bind_rows(cl_surveys_2018_clean, 
                                  cl_surveys_2019_clean, 
                                  cl_surveys_2020_clean) 

# declutter
remove(cl_surveys_2018, 
       cl_surveys_2019, 
       cl_surveys_2020, 
       cl_surveys_2018_clean,
       cl_surveys_2019_clean,
       cl_surveys_2020_clean)


# more wrangling
cl_surveys_2018_2020_clean <- cl_surveys_2018_2020 |> 
  # create column that is a date only (not date time)
  mutate(date_survey = date(date),
         year = year(date_survey),
         month = month(date_survey),
         time_only = as_hms(time)) 


# integrate missing survey ----

# checks below flagged that this survey was missing from the 2019 annual data
# the file on Box lacked geographic coordinates, so re-exported data from AGOL
cl_2019_05 <- read_xlsx("data/2019/monthly_files_as_downloaded_from_Box/CL_Bird_Observations_20190520_redownloaded.xlsx", guess_max = 3000) |> 
  clean_names() |> 
  rename(common_name = species,
         longitude = x,
         latitude = y
  ) |> 
  mutate(date = as.Date("2019-05-20"),
         time = NA,
         year = 2019,
         month = 5) |>
  mutate(date_survey = date(date)) |> 
  # remove water level column (from NCOS survey template)
  select(-water_level) |> 
  # replace NAs for species with observation notes
  mutate(common_name = case_when(
    is.na(common_name) ~ observation_notes,
    .default = common_name
  )) |> 
  mutate(common_name = case_when(
    common_name == "warbling vireo" ~ "Western Warbling Vireo",
    common_name == "yellow breasted chat" ~ "Yellow-breasted Chat",
    common_name == "willow flycatcher" ~ "Willow Flycatcher",
    common_name == "western and Baucus cross" ~ "Western x Glaucous-winged Gull (hybrid)",
    .default = common_name)
  ) |> 
  mutate(count = case_when(
    is.na(count) & common_name == "Western Warbling Vireo" ~ 1,
    .default = count
  ))
  

# compare columns to combined data
comp_2019_05 <- janitor::compare_df_cols(cl_2019_05, cl_surveys_2018_2020_clean) |> 
  # filter to any column name not present in all of the data frames
  filter(if_any(everything(), is.na))

cl_surveys_2018_2020_complete <- bind_rows(cl_surveys_2018_2020_clean, cl_2019_05)

# Checks ----

# List number of distinct survey dates by year (or year and month)
summary_n_surveys <- cl_surveys_2018_2020_clean |> 
  # also group by month to find missing surveys
  group_by(year, month) |> 
  summarize(n_distinct(date_survey))

# there is one survey from December 2017
# there are only 11 survey dates in 2019 instead of 12
# May 2019 is missing


## Missing data ----
# visualize missing data
vis_miss(cl_surveys_2018_2020_complete)

## Check missing species ----

# subset rows where common_name itself is missing
chk_missing_species <- cl_surveys_2018_2020_complete %>% 
  filter(is.na(common_name))

# only ran this once
# write_csv(chk_missing_species, "data/cleaning_materials/2018_2020_missing_species.csv")


# 67 rows missing common_name
# some indicate the start or end of the survey
# in others the species was indicated in the observation_notes column

lookup_table_missing_species <- read_csv("data/cleaning_materials/2018_2020_missing_species_lookup_table.csv") |> 
  select(global_id, date, common_name) |> 
  rename(common_name_fix = common_name) |> 
  mutate(common_name = NA)

# lookup table to species names to fix
lookup_species_names <- read_csv("data/cleaning_materials/2018_2020_species_name_fixes_lookup.csv")


cl_2018_2020_missing_sp_fixed <- cl_surveys_2018_2020_complete |>
  # join lookup table into clean data based on global_id, date, and common name (NA)
  left_join(lookup_table_missing_species, by = join_by(global_id, date, common_name)) |> 
  # then use common_name_fix to fill in missing common_name values
  mutate(common_name = coalesce(common_name_fix, common_name)
         ) |> 
  select(-common_name_fix) |>
  mutate(common_name = case_when(
    common_name == "NOT LISTED" ~ observation_notes,
    .default = common_name
  )) |> 
  mutate(common_name = replace_values(common_name, from = lookup_species_names$common_name, to = lookup_species_names$common_name_replacement))
  
# recheck for missing species
chk_missing_species <- cl_2018_2020_missing_sp_fixed |>  
  filter(is.na(common_name))

# good, 11 observations where there was no information about species (two of those were mammals)

## Check missing counts ----

# subset rows where count is NA but common_name is present
chk_missing_counts <- cl_2018_2020_missing_sp_fixed |>  
  filter(is.na(count)) |>  
  filter(!is.na(common_name)) |> 
  # use this as a lookup table
  select(objectid:count) |> 
  # assume missing counts are 1s
  mutate(count_fix = 1)
  
# join and coalesce
cl_2018_2020_missing_count_fixed <- cl_2018_2020_missing_sp_fixed |>
  left_join(chk_missing_counts, by = join_by(objectid, global_id, date, time, common_name, count)) |> 
  mutate(count = coalesce(count, count_fix)) |> 
  # remove extra column
  select(-count_fix)
  
# recheck missing counts
chk_missing_counts <- cl_2018_2020_missing_count_fixed |>  
  filter(is.na(count)) |>  
  filter(!is.na(common_name))

# look at vis_miss again
vis_miss(cl_2018_2020_missing_count_fixed)

# Observation notes ----
chk_notes <- cl_2018_2020_missing_count_fixed |> 
  filter(!is.na(observation_notes))

# Check species list ----

# Arguably taxonomic harmonization should happen after combining years, but this gives a preview
sp_2018_2020 <- cl_2018_2020_missing_count_fixed |> 
  distinct(common_name) |> 
  left_join(ebird_taxonomy, by = join_by(common_name)) |> 
  arrange(taxonomic_order)

# check what didn't match
anti_join(sp_2018_2020, ebird_taxonomy, by = join_by(common_name)) |> 
  arrange(common_name)

# wrote list of non-matching species into the cleaning_materials subfolder 
# write_csv(sp_match_fail, "data/cleaning_materials/2018_2020_species_name_fixes.csv")

sp_2018_2020_fixed <- cl_2018_2020_missing_count_fixed |> 
  distinct(common_name) |> 
  left_join(ebird_taxonomy, by = join_by(common_name)) |>
  arrange(taxonomic_order)

# see if any fail to match to guild list
sp_missing_guild <- anti_join(sp_2018_2020_fixed, guilds)

# join guilds into survey data 
cl_2018_2020_w_guilds <- cl_2018_2020_missing_count_fixed |> 
  # join ebird_taxonomy
  left_join(ebird_taxonomy) |> 
  # koin guilds (joins by ebird_taxonomy fields)
  left_join(guilds)

# only did this once:
# write_csv(sp_missing_guild, "data/cleaning_materials/2018_2020_sp_missing_guild.csv")

# check distribution of timestamps ----
ggplot(cl_2018_2020_w_guilds, aes(x = as.POSIXct(format(time, format = "%H:%M:%S"), format = "%H:%M:%S"))) +
  geom_histogram(bins = 24, fill = "darkgreen", color = "white") +
  labs(title = "Time Distribution", x = "Time of Day", y = "Count")

# write revised combined file to file ---
write_csv(cl_2018_2020_w_guilds, "data/Combined_2018_2020.csv")
