library(tidyverse)
library(sf)
library(bruneimap)
theme_set(theme_bw())

brd <- st_read("data/hotosm_brn/hotosm_brn_roads_lines_shp/")
brd <- read_sf("data/hotosm_brn/hotosm_brn_roads_lines_geojson/hotosm_brn_roads_lines_geojson.geojson")

brd$highway 
# https://wiki.openstreetmap.org/wiki/Tag:highway%3Dprimary

brd |>
  filter(grepl("Lebuhraya", name)) |>
  sf::st_transform(4326) |>
  ggplot() +
  geom_sf() +
  geom_sf(data = bruneimap::dis_sf, fill = "transparent", color = "red") 

ggplot() +
  geom_sf(data = bruneimap::dis_sf) +
  geom_sf(data = brd, aes(col = highway))

# How to zoom in on Brunei muara? st_intersection?
bm <- filter(dis_sf, name == "Brunei Muara") 
brd_bm <- st_intersection(brd, bm)
brd_bm <- filter(brd_bm, st_geometry_type(brd_bm) == "LINESTRING")
  
ggplot() +
  geom_sf(data = bm) +
  geom_sf(data = brd_bm, aes(col = highway)) +
  scale_colour_viridis_d(direction = -1)

