# The purpose of this script is to read in the Cheadle Center Work Areas shapefile and 
# filter to polygons relevant to Campus Lagoon (in particular Campus Point and Lagoon Island, where oaks were planted)
# These can be used to filter bird observations (using xy coordinates) from the monthly bird surveys to only those from the areas of interest

library(tidyverse)


# for working with vector data
library(sf)
library(mapview)

# read in work area shapefile

ccber_work_areas <- st_read(dsn = "data/management_areas/CCBER_Work_Areas")

#filter to just areas planted with oaks
aoi_oaks <- ccber_work_areas %>% 
  filter(Site_Label %in% c("Lagoon Island", "Campus Point")) 

# write as geopackage (only do this once)
# sf::st_write(aoi, "data/management_areas/cl_oak_areas.gpkg")

# rm(ccber_work_areas, aoi)

aoi_oaks <- st_read(dsn = "data/management_areas/cl_oak_areas.gpkg") %>% 
  sf::st_cast() %>% 
  # drop z dimension
  sf::st_zm()

mapview(aoi_oaks, map.type = "Esri.WorldImagery")

# filter to just lagoon itself

aoi_lagoon <- ccber_work_areas |> 
  filter(Site_Label == "Campus Lagoon") |> 
  # drop z dimension
  sf::st_zm()

mapview(aoi_lagoon, map.type = "Esri.WorldImagery")
