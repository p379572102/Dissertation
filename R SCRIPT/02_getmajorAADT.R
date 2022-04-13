# 01 Get AADT count
# Aim this script get the AADT data of the selected network and assigns
# AADT values to the corresponding roads and junctions

# ################################################################# #
#### LOAD LIBRARIES AND DEFINE CORE SETTINGS                     ####
# ################################################################# #

### Load libraries
library(sf)
library(tmap)
library(osmdata)
library(dplyr)
library(dismo)
library(deldir)

tmap_mode("view")

# ################################################################# #
#### GET AADT COUNTS                                             ####
# ################################################################# #

### Bounds and traffic
bounds <- readRDS("E:/R_language/AADT/data/bounds.Rds")
traffic <- readRDS("E:/R_language/AADT/data/traffic.Rds")
traffic <- traffic[,c("road","aadt","ncycles")] #delete unnecessary columns
trafficsub <- traffic[bounds,] #filter the items within the bounds

### Subset major and minor roads
lines <- lines[bounds,]
lines_major <- lines[lines$highway %in% c("motorway","motorway_link","primary",
                                    "primary_link","trunk","trunk_link"),]
lines_minor <- lines[!lines$highway %in% c("motorway","motorway_link","primary",
                                     "primary_link","trunk","trunk_link"),]

### Get the centroid of each major lines
lines_major_cents <- st_coordinates(st_centroid(lines_major))

### Find the nearest centroid within 20 metres, and find potentially duplicated ones
nn = RANN::nn2(lines_major_cents, k = 20) ### TRY TO FIX THIS LOOP ###

for(k in 1:2){
  for(i in 1:nrow(lines_major)){
    if(is.na(lines_major$ref[i])){
      for(j in 2:20){
        idx <- nn$nn.idx[i,j]
        if(lines_major$highway[idx] == lines_major$highway[i]){
          if(!is.na(lines_major$ref[idx])){
            lines_major$ref[i] <- lines_major$ref[idx]
            break
          }
        }
      }
    }
  }
}

qtm(lines_major, lines.col = "ref", lines.lwd = 3)
#rm(nn,lines_major_cents)

lines <- rbind(lines_major, lines_minor)

# ############################################ #
####   Assign Traffic Counts to the roads   ####
# ############################################ #

### Custom a Function
get.aadt.class <- function(e){
  #message(paste0("doing ",e))
  traffic.sub <- traffic.class[traffic.class$road == roadnames[e],]
  traffic.sub <- traffic.sub[!duplicated(traffic.sub$geometry),]
  lines.sub <- lines.nona[lines.nona$ref == roadnames[e],]

  ### need at least 2 points to make voronoi polygons, a cell consisting of
  ### all points of the plane closer to a certain seed than to any other
  if(nrow(traffic.sub) > 1){
    ### Make voronoi polygons and convert to SF
    # voronoi <- dismo::voronoi(xy = st_coordinates(traffic.sub),
    #                           ext = st_bbox(lines.sub))
    # voronoi <- as(voronoi, "sf")
    # st_crs(voronoi) <- st_crs(traffic.sub)
    voronoi <- st_voronoi(st_combine(traffic.sub))
    voronoi <- st_collection_extract(voronoi)
    voronoi <- st_as_sf(voronoi)

  }else{
    ### Make a big buffer around the point
    voronoi <- st_buffer(traffic.sub, 500)
  }

  voronoi <- st_join(voronoi, traffic.sub)

  ### Find Intersections of roads with veronoi polygons
  inter <- st_intersects(lines.sub,voronoi)
  ### Get aadt and ncycle values
  lines.sub$aadt <- lapply(1:nrow(lines.sub),function(x)
    {as.numeric(round(mean(traffic.sub$aadt[inter[[x]]])),0)})

  ### Remove Unneeded Data
  lines.sub <- as.data.frame(lines.sub)
  lines.sub <- lines.sub[,c("osm_id","aadt")]

  return(lines.sub)
}


### Separate Classified and Unclassified Roads
traffic.class <- traffic[!substr(traffic$road,1,1) %in% c("U","C"),]
traffic.unclass <- traffic[substr(traffic$road,1,1) %in% c("U","C"),]


### Start with the classified
roadnames <- unique(traffic.class$road)
roadnames <- roadnames[roadnames %in% lines$ref]
lines.nona <- lines[!is.na(lines$ref),] #Create a working dataset without nas
lines.nona <- lines.nona[,c("osm_id","ref")] #Dump unneeded data
res.class <- lapply(1:length(roadnames),get.aadt.class)
             #apply a function over a list or vector
res.class <- do.call("rbind",res.class) #rbind the data.frame
res.class <- res.class[!is.na(res.class$osm_id),]
rm(lines.nona,roadnames)


### remove any duplicates
res.class <- res.class[!duplicated(res.class$osm_id),]
res.class$aadt <- as.numeric(res.class$aadt)

### Join onto the original lines data
lines <- left_join(lines,res.class, by = c("osm_id" = "osm_id"))
rm(res.class)

qtm(lines[lines$highway %in% c("motorway","motorway_link","primary","primary_link",
                           "trunk","trunk_link"),]
    , lines.lwd = 3, lines.col = "aadt")


### Save these for later ----------------------------------------------------

st_write(lines, "E:/R_language/AADT/data/iow_lines_all.gpkg", delete_dsn = TRUE)
st_write(points, "E:/R_language/AADT/data/iow_points.gpkg", delete_dsn = TRUE)
st_write(traffic, "E:/R_language/AADT/data/iow_traffic_points.gpkg", delete_dsn = TRUE)
