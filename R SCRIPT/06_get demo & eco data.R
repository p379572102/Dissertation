# Aim: To get demographic and economical data

### Clear memory
rm(list = ls())

### Load Packages 

library(ggplot2)
library(sf)
library(tmap)
tmap_mode("view")

### Load Data 
lsoa_Camb <- st_read("Data/05_lsoa_Camb.gpkg")
lines_minor <- st_read("Data/05_lines_minor.gpkg")
demo_eco <- readRDS("Data/00_demo_eco.RDS")


### Generate the road buffer
lsoa_Camb <- left_join(lsoa_Camb,st_drop_geometry(demo_eco[,c("geo_code", "cars_percap_2018", 
                                             "pop_2018", "Employment")]), 
                       by = c("geo_code" = "geo_code"))

minor_cent <- st_centroid(lines_minor) #fix this function!!!
minor_cent <- st_join(minor_cent, lsoa_Camb[,c("cars_percap_2018", 
                                               "pop_2018", "Employment")])
                   #avoid the situation that one road cross more than one lsoa

lines_minor$cars <- minor_cent$cars_percap_2018[match(lines_minor$way_id,
                                                minor_cent$way_id)]
lines_minor$pop <- minor_cent$pop_2018[match(lines_minor$way_id,
                                                 minor_cent$way_id)]
lines_minor$employ <- minor_cent$Employment[match(lines_minor$way_id,
                                                minor_cent$way_id)]
st_write(lines_minor, "Data/06_lines_minor.gpkg", delete_dsn = TRUE)
