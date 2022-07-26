# 00 Aim to get the the origin data


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

saveRDS(osm_raw,"Data/00_osm_raw.RDS")


# ################################################################# #
#### GET Traffic count data                                      ####
# ################################################################# #
traffic1<-read.csv(file="Data/dft_aadf_local_authority_id_97.csv", header=TRUE)
traffic2<-read.csv(file="Data/dft_aadf_local_authority_id_129.csv", header=TRUE)

### change the lon and lat to the 4326 crs
traffic1<-st_as_sf(traffic1,coords = c("longitude","latitude"),crs=4326) 
traffic2<-st_as_sf(traffic2,coords = c("longitude","latitude"),crs=4326)

### traffic<-traffic[traffic$estimation_method=="Counted",]
traffic1_2018<-traffic1[traffic1$year==2018,] #filter the count obtained in 2018
traffic1_2018<-traffic1_2018[,c("road_name","road_type","all_motor_vehicles")] 

names(traffic1_2018)<-c("road_name","road_type","aadt","geometry")
traffic1_2018 <- st_transform(traffic1_2018,27700)

traffic2_2018 <- traffic2[traffic2$year==2018,] #filter the count obtained in 2018
traffic2_2018 <- traffic2_2018[,c("road_name","road_type","all_motor_vehicles")] 

names(traffic2_2018) <- c("road_name","road_type","aadt","geometry")
traffic2_2018 <- st_transform (traffic2_2018,27700)

traffic_2018 <- rbind (traffic1_2018,traffic2_2018)


# ################################################################# #
#### GET Boundary                                                ####
# ################################################################# #

### Make Bounds and save the traffic count within the bound
bound_buf <- st_convex_hull(st_union(traffic_2018)) # Make Polygon Around Traffic Data
bound_buf <- st_buffer(bound_buf, 1000) # Buffer Polygon by 1km

qtm(bound_buf, fill = NULL, lines.col = "red")
qtm(traffic_2018,dots.col = "aadt")

traffic_2018 <- traffic_2018[bound_buf,]

st_write(bound_buf, "Data/00_bound_buf.gpkg", delete_dsn = TRUE)
saveRDS(traffic_2018,"Data/00_traffic_camb_2018.RDS")


# ################################################################# #
#### GET Economic and Demographic Data                           ####
# ################################################################# #

### Load LSOA boundary
lsoa <- read_sf("E:/R_language/Dissertation/Data/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp")
lsoa <- lsoa[substr(lsoa$geo_code,1,1) == "E",]
st_write(lsoa, "Data/00_LSOA_England.gpkg", delete_dsn = TRUE)

lsoa_Camb <- lsoa[bound_buf,]
qtm(bound_buf) + qtm(lsoa_Camb, fill = NULL)

st_write(lsoa_Camb, "Data/00_LSOA_Cambridgeshire.gpkg", delete_dsn = TRUE)
# lsoa <- readRDS("Data/LSOA_Cambridgeshire.Rds")


### Load population and car ownership original data
pop_carown <- read.csv (file="Data/PBCC_LSOA_data.csv",header = TRUE)
pop_carown <- pop_carown[pop_carown$LSOA11 %in% lsoa_Camb$geo_code, ]
pop_carown <- pop_carown[,c("LSOA11","cars_percap_2018","pop_2018", "T2W_CarOrVan","km_CarOrVan","T2S_car")]

demo_eco <- left_join(lsoa_Camb, pop_carown, by = c("geo_code" = "LSOA11"))
tm_shape(demo_eco) +
  tm_fill("cars_percap_2018", breaks = c(0,0.2,0.4,0.6,1,2,70))+


### Load employment original data
employ <- read.csv (file = "Data/employment_Camb_2018.csv", header = TRUE)
# employ_empt <- demo_eco[!demo_eco$geo_label %in% employ$geo_name,]
demo_eco <- left_join(demo_eco, employ, by = c("geo_code" = "mnemonic"))
tm_shape(demo_eco) +
  tm_fill("Employment", breaks = c(0,200,400,600,1000,2000,5000))                    

saveRDS(demo_eco, "Data/00_demo_eco.RDS")
