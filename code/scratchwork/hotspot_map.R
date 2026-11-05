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

library(sfhotspot) # for hotspots


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


# Filter to specific guilds ----

# Filter to just Shorebirds
shorebirds_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Shorebirds")

mapview(shorebirds_2018_2020_sf)
# ~3 shorebirds in middle of lagoon, but had not been recorded with substrate = flyover or air

# check shorebird subset summary
summary(shorebirds_2018_2020_sf)


# Filter to just Waders: Herons, Egrets, Ibis
waders_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Herons, Egrets, Ibis")

# check map
mapview(waders_2018_2020_sf)

# Filter to just Waterfowl (Ducks, Geese, and Other Swimming Birds)
waterfowl_2018_2020_sf <- survey_data_no_flyovers_2018_2020_sf %>% 
  # example filtering by species
  filter(general_type == "Waterfowl & Friends")

mapview(waterfowl_2018_2020_sf, map.type = "Esri.WorldImagery")

# TODO- Of these, probably the most relevant are Cormorants, Kingfishers, and Gulls & Terns (but Gulls and Terns are often flying)

# Filter to just Insectivores - Aerial
# Filter to just Insectivores - Terrestrial
# Filter to just Seed & Fruit Eaters
# Filter to just Cormorants
# Filter to just Kingfishers
# Filter to just Gulls & Terns
# Filter to just Omnivores
# Filter to just Raptors
# Filter to just Hummingbirds

# Next steps:

# Clip points to the perimeter of the lagoon (exclude upland observations), with a buffer (distance TBD, but maybe ~ 5 m). This will require projecting first.

# Then, once you have a filtered point data set, you could rasterize the data or do some interpolation (kriging) to make inferences about the entire study area.

# Next steps:

# read in a polygon for the study area
# create a template raster with desired resolution for that study area
# rasterize 

shorebird_raster <- st_rasterize(shorebirds_2018_2020_sf["count"])



plot(shorebird_raster)

work_areas <- st_read("data/management_areas/CCBER_Work_Areas.gpkg")

mapview(work_areas)

cl_polygon <- work_areas |> 
  filter(Site_Name == "Campus_Lagoon")
  
cl_polygon_projected <- st_transform(cl_polygon, crs = 32611)

# buffer by 5 m
cl_polygon_buffered <- cl_polygon_projected |> 
  # drop z dimension
  st_zm() |> 
  # then buffer by 5 units (= 5 m)
  st_buffer(dist = 5)

mapview(cl_polygon_buffered, map.type = "Esri.WorldImagery")

# test hotspot approach:

# Shorebird hotspots ----

# 1. Kernel Density Estimation
sf_kde <- hotspot_kde(shorebirds_2018_2020_sf, grid_type = "hex")
autoplot(sf_kde)

# 2. Getis-Ord Gi* Hotspot Analysis
# sf_gistar <- hotspot_gistar(sf_points)
# autoplot(sf_gistar)

# 2. Getis-Ord Gi* Hotspot Analysis
sf_gistar <- hotspot_gistar(shorebirds_2018_2020_sf, grid_type = "hex", weights = count)
autoplot(sf_gistar)

hotspot_count(shorebirds_2018_2020_sf, weights = count, grid_type = "hex")

# mapview

#  Write to a single-layer GeoPackage
st_write(sf_gistar, "data/cl_2018_2020_shorebird_hotspot_hex.gpkg", layer = "cl_2018_2020_shorebird_hotspot")

# Wader hotspots ----
sf_gistar_waders <- hotspot_gistar(waders_2018_2020_sf, grid_type = "hex", weights = count)
autoplot(sf_gistar_waders)

#  Write to a single-layer GeoPackage
st_write(sf_gistar_waders, "data/cl_2018_2020_wader_hotspot.gpkg", layer = "cl_2018_2020_wader_hotspot")


# Waterfowl hotspots ----
sf_gistar_waterfowl <- hotspot_gistar(waterfowl_2018_2020_sf, grid_type = "hex", weights = count)
autoplot(sf_gistar_waterfowl)

#  Write to a single-layer GeoPackage
st_write(sf_gistar_waterfowl, "data/cl_2018_2020_waterfowl_hotspot.gpkg", layer = "cl_2018_2020_waterfowl_hotspot")


