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
#### GET ROAD OSM DATA                                           ####
# ################################################################# #

### Find the bounding box(bb) and Using Overpass query to get Roads within the bb
q = opq(getbb("Cambridgeshire, UK")) %>%
  add_osm_feature(key = "highway")

### Return an OSM Overpass query as an osmdata object in sf format
osm_raw = osmdata_sf(q = q)

saveRDS(osm_raw,"osm_raw.RDS")

# ################################################################# #
#### PREP INPUT DATA                                             ####
# ################################################################# #

### Return line, points,polys layer component respectively
lines <- osm_raw$osm_lines
points <- osm_raw$osm_points
polys <- osm_raw$osm_polygons # needed for roundabouts

### Change CRS to British national grid 27700
lines <- st_transform(lines, 27700)
points <- st_transform(points, 27700)
polys <- st_transform(polys, 27700)

### Filter the items that highway type is in line with one of the 9 types,
###since only intrested in roads not paths
polys <- polys[polys$highway %in% c("motorway","living_street","primary","primary_link",
                                    "residential","secondary", "secondary_link",
                                    "tertiary",  "tertiary_link", "unclassified"),]

lines <- lines[lines$highway %in% c("living_street","primary","primary_link",
                                    "residential","secondary", "secondary_link",
                                    "tertiary",  "tertiary_link", "unclassified"),]

### Cast sf object polys into Linestring and only save 7 variables
polys <- st_cast(polys, "LINESTRING")
polys <- polys[,c("osm_id","name","ref","highway",
                  "junction","maxspeed","geometry")]
lines <- lines[,c("osm_id","name","ref","highway",
                  "junction","maxspeed","geometry")]


### Put two layers that contain road types together
lines <- rbind(lines, polys)
rm(osm_raw, polys)
qtm(lines)


### Find Junctions, OSM Points are both nodes that make up lines/polygons, and
### objects e.g. shops
### remove points that are not nodes on the line
### node points have no tags

### Get column names other than osm_id, and highway which is just for junction
### types, and crossing info which can be junction between cycle way and road

col.names <- names(points)[!names(points) %in% c("osm_id","highway", "crossing",
                                                 "crossing_ref","geometry")]
points.sub <- points
points <- points[,c("osm_id","highway")]
points.sub <- as.data.frame(points.sub) #???Don't quiet understand
points.sub$geometry <- NULL #increase the processing effiency
points.sub <- points.sub[,col.names]
rowsum <- as.integer(rowSums(!is.na(points.sub))) #Find the points with tags and describe with integer

rm(points.sub, col.names)

points <- points[rowsum == 0,] #Remove points with any tags

### Check highway tag to remove things like traffic lights—??????How can I know which to delete
points <- points[is.na(points$highway) | points$highway %in%
                   c("mini_roundabout","motorway_junction"), ] # | means or
points <- points[,c("osm_id","geometry")] # Delete unnecessary column timely

### Look for points that intersect lines
inter <- st_intersects(points,lines)
len <- lengths(inter)
points <- points[len >= 2,] # Only keep points that intersec at least 2 lines
                            # i.e. a junction

### Remove any duplicated points
points <- points[!duplicated(points$geometry),]
rm(len, rowsum, inter)
