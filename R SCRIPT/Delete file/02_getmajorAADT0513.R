# 02 Get majot road AADT count
# Aim this script get the AADT data of the selected network and assigns
# AADT values to the corresponding roads and junctions

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
#### Load data                                                   ####
# ################################################################# #

bound<-st_read("Data/00_bound_buf.gpkg")
traffic_2018<-readRDS("Data/00_traffic_camb_2018.RDS")
lines<-st_read("Data/01_network.gpkg")
points <- st_read("Data/01_junctions.gpkg")


# ################################################################# #
#### GET AADT COUNTS                                             ####
# ################################################################# #
major_ref <- c("motorway","motorway_link","primary",
               "primary_link","trunk","trunk_link")

lines_major <- lines[lines$highway %in% major_ref,]
lines_minor <- lines[!lines$highway %in% major_ref,]


### Get the centroid of each major lines

lines_major_cents <- st_coordinates(st_centroid(lines_major))

### Find the 30 of nearest neighbour centroids , and find potentially duplicated ones
nn = RANN::nn2(lines_major_cents, k = 30) ### k value depends on the network###

lines_major$highway[lines_major$highway == "motorway_link"] <-  "motorway"
lines_major$highway[lines_major$highway == "primary_link"] <-  "primary"
lines_major$highway[lines_major$highway == "trunk_link"] <-  "trunk"

for(k in 1:2){
  for(i in 1:nrow(lines_major)){
    if(is.na(lines_major$ref[i])){ #find the item whose ref equals to NA
      for(j in 2:30){ #starts from 2 because j=1 is the point itself
        idx <- nn$nn.idx[i,j] # allocate the row number of the nearest centroid 
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



# manually check the ref and fix the unreasonable ones
qtm(lines_major, lines.col = "ref", lines.lwd = 2)
lines_major$ref[lines_major$osm_id == "148927689"] <-  "A1"

# line_major_sub<-st_intersection(lines_major,bound)
qtm(bound)+qtm(lines_major, lines.col = "ref", 
               lines.lwd = 2) + qtm(traffic_2018,dots.col = "aadt")

lines <- rbind(lines_major, lines_minor)
rm(nn,lines_major_cents)

# ############################################ #
####   Assign Traffic Counts to the roads   ####
# ############################################ #

### Custom a Function
get.aadt.class <- function(e){
  #message(paste0("doing ",e))
  traffic.sub <- traffic_2018.class[traffic_2018.class$road_name == roadnames[e],]
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
    ### Make a 500 buffer around the point
    voronoi <- st_buffer(traffic.sub, 500)
  }

  voronoi <- st_join(voronoi, traffic.sub) #based on the geom

  ### Find Intersections of roads with voronoi polygons
  inter <- st_intersects(lines.sub,voronoi)
  ### Get aadt and ncycle values 1:nrow(lines.sub)
  lines.sub$aadt <- lapply(1:nrow(lines.sub),function(x)
    {as.numeric(round(mean(traffic.sub$aadt[inter[[x]]])),0)})

  ### Remove Unneeded Data
  lines.sub <- as.data.frame(lines.sub)
  lines.sub <- lines.sub[,c("osm_id","aadt")]

  return(lines.sub)
}

### find the road_name of AADT different with the line$ref and fix them
roadnames <- unique(traffic_2018$road_name)
roadnames.sub <- roadnames[!roadnames %in% lines$ref] 
traffic_2018$road_name[traffic_2018$road_name == "A14(M)"] <-  "A14"
traffic_2018$road_type[traffic_2018$road_name == "A6118"] <-  "Minor"
rm(roadnames.sub)

### Separate Major and Minor Roads of the AADT Data
traffic_2018.class <- traffic_2018[traffic_2018$road_type=="Major",]
traffic_2018.unclass <- traffic_2018[traffic_2018$road_type=="Minor",]


### Start with the classified!!!modify!!!
roadnames <- unique(traffic_2018.class$road_name)
lines.nona <- lines_major[!is.na(lines_major$ref),] 
                  #Create a working dataset without nas
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

qtm(bound) + qtm(lines[lines$highway %in% c("motorway","motorway_link","primary","primary_link",
                           "trunk","trunk_link"),]
    , lines.lwd = 3, lines.col = "aadt") + qtm(traffic_2018.class, dots.col="aadt")

### Find Junctions between minor and major roads ----------------------------
minor_int <- st_intersects(points, lines_minor)
major_int <- st_intersects(points, lines_major)

minor_int = lengths(minor_int)
major_int = lengths(major_int)
both_int = ifelse(minor_int > 0 & major_int > 0, TRUE, FALSE)

junc_majmi = points[both_int,]

### Match Major Road AADT onto junctions
junc_majmi <- st_join(junc_majmi, lines[,"aadt"])
junc_majmi <- junc_majmi[!duplicated(junc_majmi$geom),]
qtm(junc_majmi, dots.col = "aadt")

st_write(lines_minor, "Data/02_lines_minor.gpkg", delete_dsn = TRUE)
st_write(lines, "Data/02_lines.gpkg", delete_dsn = TRUE)
st_write(junc_majmi, "Data/02_junctions.gpkg", delete_dsn = TRUE)
