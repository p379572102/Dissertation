# 01 Get road data
# Aim this script get the OSM data for an area, cleans it and obtain the
# road network and junction

# ################################################################# #
#### LOAD LIBRARIES AND DEFINE CORE SETTINGS                     ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load libraries
library(sf)
library(tmap)
library(osmdata)
library(dplyr)
library(dismo)
library(deldir)

tmap_mode("view")


# ################################################################# #
#### PREP The Road Network                                       ####
# ################################################################# #

osm_raw<-readRDS("Data/00_osm_raw.RDS")
bound<-st_read("Data/00_bound_buf.gpkg")


### Return line, points, polys layer component respectively
lines <- osm_raw$osm_lines
points <- osm_raw$osm_points
polys <- osm_raw$osm_polygons # needed for roundabouts

rm(osm_raw)

### Change CRS to British national grid 27700
lines <- st_transform(lines, 27700)
points <- st_transform(points, 27700)
polys <- st_transform(polys, 27700)

qtm(lines[sample(1:nrow(lines), 1000), ])

lines <- lines[bound, ]
points <- points[bound, ]
polys <- polys[bound, ]


### Filter the items that highway type is in line with one of the 13 types,
### since only interested in roads not paths
road_types <- c("motorway","motorway_link","trunk","trunk_link","living_street","primary",
              "primary_link","residential","secondary", "secondary_link",
              "tertiary",  "tertiary_link", "unclassified")
polys <- polys[polys$highway %in% road_types, ]

lines <- lines[lines$highway %in% road_types, ]


### Cast sf object polys into Linestring and only save 7 variables
polys <- st_cast(polys, "LINESTRING")
col_names<-c("osm_id","name","ref","highway",
             "junction","maxspeed","geometry")
polys <- polys[, col_names]
lines <- lines[, col_names]

### Put two layers that contain road types together
lines <- rbind(lines, polys) 

### Return the osmdata within the boundary
qtm(lines[sample(1:nrow(lines), 1000), ])

st_write(lines,"Data/01_network.gpkg", delete_dsn = TRUE)

rm(polys,col_names,road_types)

# ################################################################# #
#### PREP The Road Junction                                      ####
# ################################################################# #

### OSM Points are both nodes that make up lines/polygons, and objects 
### e.g. shops.
### remove points that are not nodes on the line
### node points on the line have no tags

### Get column names other than osm_id, and highway which is just for junction
### types, and crossing info which can be junction between cycle way and road
col.names <- names(points)[!names(points) %in% c("osm_id","highway", "crossing",
                                                 "crossing_ref","geometry")]
points.sub <- points
points <- points[, c("osm_id","highway")]

### change it into data.frame then can be calculated, and discard the geometry 
### to increase processing efficiency
points.sub <- as.data.frame(points.sub) 
points.sub$geometry <- NULL 
points.sub <- points.sub[, col.names]

### Find the points with tags and change the amount of tags into integer
rowsum <- as.integer(rowSums(!is.na(points.sub))) 

rm(points.sub, col.names)

### Remove points with any tags
points <- points[rowsum == 0, ] 

### Check highway tag to remove things like traffic lights
points <- points[is.na(points$highway) | points$highway %in%
                   c("mini_roundabout","motorway_junction"), ] # | means or
points <- points[,c("osm_id","geometry")] # Delete unnecessary column timely

st_write(points,"Data/01_all_points.gpkg",delete_dsn = TRUE)

### Looking for points that intersect lines
inter <- st_intersects(points,lines) #return the value of points
len <- lengths(inter)
points <- points[len >= 2, ] # Only keep points that intersect at least 2 lines
                            # i.e. a junction

### Remove any duplicated points
points <- points[!duplicated(points$geometry), ]
rm(len, rowsum, inter)

#qtm(lines, lines.col = "blue") + qtm(points)
qtm(points[sample(1:nrow(points), 1000), ])

st_write(points, "Data/01_junctions.gpkg", delete_dsn = TRUE)
