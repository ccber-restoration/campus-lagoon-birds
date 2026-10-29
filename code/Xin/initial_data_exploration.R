# The purpose of the script is to demonstrate some basic ways of working with spatial data in R and to begin to explore the bird data set

# initially drafted by Francis Joyce, 2026-10-08

# 1. Set up ----

## 1.1 load packages ----

library(tidyverse) # general use

library(sf) # simple features package for vector data

library(mapview) # simple interactive maps

#installing more packages!
install.packages(c("gstat", "stars"))

library(gstat)

library(stars)


# 2. Load data ----


# load combined data for 2018-2020

survey_data_2018_2020 <- read_csv("data/Combined_2018_2020.csv") |> 
  filter(!is.na(latitude))

# this converts the dataframe to a spatial (simple features) object
survey_data_2018_2020_sf <- st_as_sf(survey_data_2018_2020, 
                                     coords = c("longitude", "latitude"),
                                     crs = 4326 )

# Filtering out flyovers
survey_data_no_flyovers_2018_2020_sf <- survey_data_2018_2020_sf %>% 
  # filtering out 
  filter(substrate != "Flyover",substrate !="Air")
#^ In assuming air as a flyover

# Filter to just Shorebirds
shorebirds_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Shorebirds")

# Filter to just Waders: Herons, Egrets, Ibis
waders_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Herons, Egrets, Ibis")

# Filter to just Waterfowl (Ducks, Geese, and Other Swimming Birds)
waterfowl_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Waterfowl & Friends")

# Filter to just Waterfowl (Ducks, Geese, and Other Swimming Birds)
waterfowl_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Waterfowl & Friends")

# Filter to just Insectivores - Aerial
# Filter to just Insectivores - Terrestrial
# Filter to just Seed & Fruit Eaters
# Filter to just Cormorants
# Filter to just Kingfishers
# Filter to just Gulls & Terns
# Filter to just Omnivores
# Filter to just Raptors
# Filter to just Hummingbirds


#mapview
mapview(shorebirds_2018_2020_sf)

# Next steps:

# filter out flyovers!

# filter by  species functional group:

## (1) shorebirds
## (2) waders (herons, egrets)
## (3) waterfowl (ducks, geese, and other swimming birds)


# Clip points to the perimeter of the lagoon (exclude upland observations), with a buffer (distance TBD, but maybe ~ 5 m). This will require projecting first.

# Then, once you have a filtered point data set, you could rasterize the data or do some interpolation (kriging) to make inferences about the entire study area.

#checking stuff
names(survey_data_2018_2020)

glimpse(survey_data_2018_2020)

summary(shorebirds_2018_2020_sf)




