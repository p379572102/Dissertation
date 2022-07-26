# Aim: To get demographic and economical data

### Clear memory
rm(list = ls())

### Load Packages 

library(ggplot2)
library(sf)
library(dplyr)
library(tmap)
library(stplanr)
tmap_mode("view")

### Load Data 
#lsoa_Camb <- st_read("Data/05_lsoa_Camb.gpkg")
lines_minor <- st_read("Data/05_lines_minor.gpkg")
demo_eco <- readRDS("Data/00_demo_eco.RDS")
bound<-st_read("Data/00_bound_buf.gpkg")

#lsoa_Camb <- left_join(lsoa_Camb,st_drop_geometry(demo_eco[,c("geo_code", "cars_percap_2018", 
#                                             "pop_2018", "Employment")]), 
#                       by = c("geo_code" = "geo_code"))

### Get the original area of each LSOA
demo_eco$orig_area_km2 <- as.numeric(st_area(demo_eco)%>% units::set_units(km^2))

### Allocate the LSOA data to the minor road  
minor_cent <- line_midpoint(lines_minor) %>% st_as_sf()
minor_cent$id <-  1:nrow(minor_cent)
minor_cent <- st_join(minor_cent, demo_eco[,c("cars_percap_2018", "pop_2018", 
                                              "Employment", "orig_area_km2")])
summary(is.na(minor_cent$cars_percap_2018))
                   #avoid the situation that one road cross more than one lsoa
qtm(demo_eco)+qtm(minor_cent[is.na(minor_cent$cars_percap_2018), ])
                   # these three points located at the blank area of LSOA, 
                   # so the road need to be cut by lsoa boundary!!!

lines_minor$orig_area_km2 <- minor_cent$orig_area_km2[match(lines_minor$id,
                                                                  minor_cent$id)]
lines_minor$cars_percap_2018 <- minor_cent$cars_percap_2018[match(lines_minor$id,
                                                minor_cent$id)]
lines_minor$pop_2018 <- minor_cent$pop_2018[match(lines_minor$id,
                                                 minor_cent$id)]
lines_minor$employ_2018 <- minor_cent$Employment[match(lines_minor$id,
                                                minor_cent$id)]
summary(is.na(lines_minor$cars))
head(lines_minor)

### Calculate the data for each minor road
lines_minor$self_road_km <- as.numeric(st_length(lines_minor)) / 1000
head(lines_minor)

lines_minor$km_percent <-lines_minor$self_road_km / lines_minor$road_km
lines_minor$area_percent <- lines_minor$area_km2 / lines_minor$orig_area_km2 

#Based on the assumption that 1km road length serve certain amount of pop and employ
lines_minor$pop <- lines_minor$km_percent * lines_minor$pop_2018 * lines_minor$area_percent
lines_minor$employ <- lines_minor$km_percent * lines_minor$employ_2018 * lines_minor$area_percent
lines_minor$cars <- lines_minor$pop * lines_minor$cars_percap_2018
     # need to solve some small part with high values!!!去05里面加初始lsoa面积

### Map the results
tm1 <- qtm(bound) + qtm(lines_minor, lines.col = "pop", lines.lwd = 3)
tmap_save(tm1, filename = "Plot/06_pop.png")

tm2 <- qtm(bound) + qtm(lines_minor, lines.col = "employ", lines.lwd = 3)
tmap_save(tm2, filename = "Plot/06_employ.png")

tm3 <- qtm(bound) + qtm(lines_minor, lines.col = "cars", lines.lwd = 3)
tmap_save(tm3, filename = "Plot/06_cars.png")


st_write(lines_minor, "Data/06_lines_minor.gpkg", delete_dsn = TRUE)
