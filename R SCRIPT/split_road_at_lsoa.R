library(lwgeom)
library(sf)
library(tmap)
library(dplyr)

tmap_mode("view")

lines <- st_read("Data/01_network.gpkg")
lsoa <- readRDS("Data/LSOA_Cambridgeshire.Rds")

lines$length <- as.numeric(st_length(lines))
summary(lines$length)

lines2 <- lines[lines$length > 1000,]
lines2 <- lines[lines$osm_id == "3576162", ]

qtm(lsoa) + qtm(lines2)

lsoa2 <- st_combine(lsoa)


lines3 <- st_split(lines2, lsoa2)
lines4 <- st_collection_extract(lines3, "LINESTRING")
