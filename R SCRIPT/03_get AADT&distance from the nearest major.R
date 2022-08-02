# Aim: To calculate AADT for minor roads
# This script assumes the data has already been cleaned and the AADT for 
# major roads assigned
# Rework of original file dropping the splitting an buffering stage

### Clear memory
rm(list = ls())

### Load Packages -----------------------------------------------------------
# Process spatial objects
library(sf)
# Make dodgr graph
library(dodgr)
# Map view
library(tmap)
# Data frame manipulation
library(dplyr)

tmap_mode("view")

### Load Data ---------------------------------------------------------------

bound<-st_read("Data/00_bound_buf.gpkg")
lines_minor <- st_read("Data/02_lines_minor.gpkg")
lines_major <- st_read("Data/02_lines_major.gpkg")
lines <- st_read("Data/01_network.gpkg")
junc_majmi <- st_read("Data/02_junctions.gpkg")

# ################################################################# #
####                Assign AADT to minor road                    ####
# ################################################################# #

### Convert to 4326 for dodgr
lines_minor <- st_transform(lines_minor, 4326)
junc_majmi <- st_transform(junc_majmi, 4326)


### Get mid-point of minor roads, i.e. centroid on the line
point_minor <- as.data.frame(st_coordinates(lines_minor))
point_minor <- group_by(point_minor, L1) %>% # Based on L1 to group the junc_minor
  summarise(X = X[round(n()/2)],
            Y = Y[round(n()/2)]) 
                                  # use the n/2 item as the group represent

### Make dodgr graph of minor roads
graph <- weight_streetnet(lines_minor, wt_profile = "motorcar")
                             # use motorcar mode to weight the minor road
graph_ids <- graph[ ,c("from_id","from_lon","from_lat")]
                 # focus on the certain start point rather than the end points
graph_ids <- unique(graph_ids)  

junc_majmi <- cbind(junc_majmi, st_coordinates(junc_majmi))
junc_majmi <- left_join(junc_majmi, graph_ids, by = c("X" = "from_lon",
                                                      "Y" = "from_lat"))
                            # link the start point which is the majmi junction to the data.frame
point_minor <- left_join(point_minor, graph_ids, by = c("X" = "from_lon",
                                                        "Y" = "from_lat"))
                            # link the start point which is the minor junction to the data.frame


### For each minor road , find the nearest (in time) junction
dists <- dodgr_times(graph,
                     from = junc_majmi$from_id,
                     to = point_minor$from_id,
                     shortest = FALSE)
notna <- colSums(!is.na(dists))
notna <- as.data.frame(notna)
summary(notna$notna==0)
         #The result shows 217 minor roads don't acquire any value

saveRDS(dists,"Data/03_dists_matrix.Rds")

nearest_junc <- list()
dist_junc <- list()
for(i in 1:ncol(dists)){
  sub <- dists[,i]
  sub <- sub[!is.na(sub)]
  if(length(sub) == 0){
    nearest_junc[[i]] <- NA
    dist_junc[[i]] <- NA
  } else {
    min_name <- names(sub)[sub == min(sub, na.rm = TRUE)]
    min_dist <- min(sub, na.rm = TRUE)
            # find the minimum value and return the name of the minimum value
    if(length(min_name) == 0){
      min_name <- NA
      min_dist <- NA
            # in case that the name of the junction lost
    }
    if(length(min_name) > 1){ 
      min_name <- min_name[1]
      min_dist <- min_dist[1] 
            # consider the condition that have more than one same min value
    }
    nearest_junc[[i]] <- min_name
    dist_junc[[i]] <- min_dist
  }
  
}
nearest_junc <- unlist(nearest_junc)
dist_junc <- unlist(dist_junc)

lines_minor$nearest_junc <- nearest_junc
lines_minor$nearest_junc_dist <- dist_junc
lines_minor$major_flow <- junc_majmi$traffic_flow[match(lines_minor$nearest_junc,
                                                junc_majmi$from_id)]

# plot each road colored by the AADT on the nearest (in time) major road

lines_minor <- st_transform(lines_minor, 27700)
lines_minor_na <- lines_minor[is.na(lines_minor$major_flow), ]

tm1 <- qtm(bound) + qtm(lines_minor, lines.col = "major_flow", lines.lwd = 1)+
  qtm(lines_major, line.col = "ref" )  
tmap_save(tm1, "Plot/03_major_flow.png")

tm2 <-qtm(bound) + qtm(lines_minor, lines.col = "nearest_junc_dist", lines.lwd = 1)+
  qtm(lines_major, line.col = "ref" ) 
tmap_save(tm2, "Plot/03_major_distance.png")

st_write(lines_minor, "Data/03_lines_minor.gpkg", delete_dsn = TRUE)
st_write(point_minor, "Data/03_point_minor.gpkg", delete_dsn = TRUE)
st_write(junc_majmi, "Data/03_junc_majmi.gpkg", delete_dsn = TRUE)


