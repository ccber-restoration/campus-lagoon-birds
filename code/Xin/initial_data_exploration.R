# The purpose of the script is to demonstrate some basic ways of working with spatial data in R and to begin to explore the bird data set

# initially drafted by Francis Joyce, 2026-10-08

# 1. Set up ----

## 1.1 load packages ----

library(tidyverse) # general use

library(sf) # simple features package for vector data

library(mapview) # simple interactive maps

# 2. Load data ----


# load combined data for 2018-2020

survey_data_2018_2020 <- read_csv("data/Combined_2018_2020.csv") |> 
  filter(!is.na(latitude))

survey_data_2018_2020_sf <- st_as_sf(survey_data_2018_2020, 
                                     coords = c("longitude", "latitude"),
                                     crs = 4326 ) |> 
  # example filtering by species
  filter(common_name == "Spotted Sandpiper")

mapview(survey_data_2018_2020_sf)

# Next steps:

# filter out flyovers!

# filter by  species functional group:

## (1) shorebirds
## (2) waders (herons, egrets)
## (3) waterfowl (ducks, geese, and other swimming birds)


# Clip points to the perimeter of the lagoon (exclude upland observations), with a buffer (distance TBD, but maybe ~ 5 m). This will require projecting first.

# Then, once you have a filtered point data set, you could rasterize the data or do some interpolation (kriging) to make inferences about the entire study area.





