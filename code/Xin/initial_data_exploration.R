# The purpose of the script is to demonstrate some basic ways of working with spatial data in R and to begin to explore the bird data set

# initially drafted by Francis Joyce, 2026-10-08

# 1. Set up ----

## 1.1 load packages ----

library(tidyverse) # general use

library(sf) # simple features package for vector data

library(mapview) # simple interactive maps

# installing more packages!
# install.packages(c("gstat", "stars"))

library(gstat) 

library(stars) 


# 2. Load data ----


# load combined data for 2018-2020
survey_data_2018_2020 <- read_csv("data/Combined_2018_2020.csv") |> 
  filter(!is.na(latitude))

# checking stuff

# check column names 
names(survey_data_2018_2020)

# see column types and a sample of the data values
glimpse(survey_data_2018_2020)


# this converts the dataframe to a spatial (simple features) object
survey_data_2018_2020_sf <- st_as_sf(survey_data_2018_2020, 
                                     coords = c("longitude", "latitude"),
                                     crs = 4326 )


# summarize observations by substrate

summary_substrate <- survey_data_2018_2020_sf |> 
  group_by(substrate) |> 
  summarize(n = n()) |> 
  arrange(n)

# Filtering out flyovers
survey_data_no_flyovers_2018_2020_sf <- survey_data_2018_2020_sf %>% 
  # filtering out flyovers and "air," assuming most "air" were actually flyovers
  filter(is.na(substrate) | substrate != "Flyover" & substrate !="Air") 
         
    
# after filtering out flyover and "air" observations, re-check which substrates are included:
unique(survey_data_no_flyovers_2018_2020_sf$substrate)

# check on map 
mapview(survey_data_no_flyovers_2018_2020_sf, map.type = "Esri.WorldImagery")

#Projecting the Bird observations
survey_data_projection <- survey_data_no_flyovers_2018_2020_sf |>
  st_transform(crs = 32611)

# Check the coordinate system
st_crs(survey_data_projection)

# Filter to just Shorebirds
shorebirds_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Shorebirds")

mapview(shorebirds_2018_2020_sf, map.type = "Esri.WorldImagery")
# ~3 shorebirds in middle of lagoon, but had not been recorded with substrate = flyover or air

# check shorebird subset summary
summary(shorebirds_2018_2020_sf)


# Filter to just Waders: Herons, Egrets, Ibis
waders_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Herons, Egrets, Ibis")

# check map
mapview(waders_2018_2020_sf, map.type = "Esri.WorldImagery")

# Filter to just Waterfowl (Ducks, Geese, and Other Swimming Birds)
waterfowl_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Waterfowl & Friends")

mapview(waterfowl_2018_2020_sf, map.type = "Esri.WorldImagery")

# Filter to just Cormorants
cormorants_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Cormorants")

mapview(cormorants_2018_2020_sf, map.type = "Esri.WorldImagery")

# Filter to just Kingfishers
kingfishers_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Kingfishers")

mapview(kingfishers_2018_2020_sf, map.type = "Esri.WorldImagery")

# Filter to just Gulls & Terns
gullandterns_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Gulls & Terns")

mapview(gullandterns_2018_2020_sf, map.type = "Esri.WorldImagery")

# Next steps:

#Management Steps
management_area <- read_sf("data/management_areas/CCBER_Work_Areas.gpkg")

mapview(management_area, map.type = "Esri.WorldImagery")

#Polygon -> Projecting the Management Area

cl_polygon <- management_area |> 
  filter(Site_Name == "Campus_Lagoon")

cl_polygon_projection <- st_transform(cl_polygon, crs = 32611) 

mapview(cl_polygon_projection, map.type = "Esri.WorldImagery")

# Creating a 5m buffer
cl_buffer <- cl_polygon_projection |>
  st_zm() |>
  st_buffer(dist = 5)

# View the buffered lagoon boundary
mapview(cl_buffer, map.type = "Esri.WorldImagery")

# polygon currently excludes the quarry.
#reproject survey data, upstream before filtering the guilds, reproject all of the survey data then filter. 

# Clip points to the perimeter of the lagoon (exclude upland observations), with a buffer (distance TBD, but maybe ~ 5 m). This will require projecting first.

#Project it 

# Then, once you have a filtered point data set, you could rasterize the data or do some interpolation (kriging) to make inferences about the entire study area.

#epsg.io

#Clipping bird observation to the lagoon 

# Keep observations within the buffered lagoon
birds_lagoon <- survey_data_projection |>
  st_filter(cl_buffer)

#Checking
mapview(cl_buffer, map.type = "Esri.WorldImagery")

mapview(birds_lagoon, map.type = "Esri.WorldImagery")
  
# Filtering to shorebirds within the lagoon.
shorebirds_lagoon <- birds_lagoon |>
  filter(general_type == "Shorebirds")
  
# Checking and Visualizing
mapview(cl_buffer, alpha.regions = 0.2) +
  mapview(shorebirds_lagoon)

# Creating a raster template for the lagoon
template_grid <- st_as_stars(
  st_bbox(cl_buffer),
  dx = 10,
  dy = 10,
  values = 0
)


# Convert shorebird counts to raster cells
shorebird_raster <- st_rasterize(
  shorebirds_lagoon["count"],
  dx= 10,
  dy= 10
)

#need to figure out what "count" actually is. is it 60 across 3 years of monthly data?

# Plot the raster
mapview(shorebird_raster, map.type = "Esri.WorldImagery")


#st_rasterize()

#  st_rasterize() st = to specify what attribute

#template raster just covering extent of polygon
# st_as_stars() |> 

#Size of the rasteration, what is the grid cell resolution, 10m to start
# shift command M : |> 


