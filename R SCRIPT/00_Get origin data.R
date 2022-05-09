# 00 Aim to get the the origin data


# ################################################################# #
#### GET Boundary                                                ####
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

###Loading the boundary of Cambridgeshire and simplify it
# bound<- read_sf(dsn = "Data/BoundaryData/england_ct_2011.shp")
# bound_simp<-st_simplify(bound, dTolerance = 5000)
# bound_buf<-st_buffer(bound_simp,3000)
# qtm(bound)+qtm(bound_buf)
# 
# st_write(bound_buf, "Data/00_bound_buf.gpkg")


# ################################################################# #
#### GET ROAD OSM DATA                                           ####
# ################################################################# #

### Find the bounding box(bb) and Using Overpass query to get Roads within the bb
q = opq(getbb("Cambridgeshire, UK")) %>%
  add_osm_feature(key = "highway")

### Return an OSM Overpass query as an osmdata object in sf format
osm_raw = osmdata_sf(q = q)

saveRDS(osm_raw,"Data/00_osm_raw.RDS")


# ################################################################# #
#### GET Traffic count data                                      ####
# ################################################################# #
traffic<-read.csv(file="Data/dft_aadf_local_authority_id_97.csv",header=TRUE)

#change the lon and lat to the 4326 crs
traffic<-st_as_sf(traffic,coords = c("longitude","latitude"),crs=4326) 
traffic<-traffic[traffic$estimation_method=="Counted",]
traffic_2018<-traffic[traffic$year==2018,] #filter the count obtained in 2018
traffic_2018<-traffic_2018[,c("road_name","road_type","all_motor_vehicles")] 

names(traffic_2018)<-c("road_name","road_type","aadt","geometry")
traffic_2018<-st_transform(traffic_2018,27700)

saveRDS(traffic_2018,"Data/00_traffic_camb_2018.RDS")


# Make Bounds
bound_buf<- st_convex_hull(st_union(traffic_2018)) # Make Polygon Around Traffic Data
bound_buf <- st_buffer(bound_buf, 1000) # Buffer Polygon by 1km
st_write(bound_buf, "Data/00_bound_buf.gpkg", delete_dsn = TRUE)


# ################################################################# #
#### GET Economic and Demographic Data                           ####
# ################################################################# #


