# Aim: To calculate AADT for minor roads
# This script assumes the data has already been cleaned and the AADT for major roads assigned
# Rework of original file dropping the splitting an buffering stage

### Clear memory
rm(list = ls())

### Load Packages -----------------------------------------------------------

library(ggplot2)
library(sf)
library(dodgr)
library(tmap)
library(dplyr)
library(concaveman)

tmap_mode("view")

### Load Data ---------------------------------------------------------------

lines_minor <- st_read("Data/02_lines_minor.gpkg")
lines <- st_read("Data/02_lines.gpkg")
junc_majmi <- st_read("Data/02_junctions.gpkg")

# ################################################################# #
####                Assign AADT to minor road                    ####
# ################################################################# #

### Convert to 4326 for dodgr
lines_minor <- st_transform(lines_minor, 4326)
junc_majmi <- st_transform(junc_majmi, 4326)

### Get mid-point of minor roads, i.e. centroid on the line
minor_cent <- as.data.frame(st_coordinates(lines_minor))
minor_cent <- group_by(minor_cent, L1) %>% # Based on L1 to group the minor_cent
  summarise(X = nth(X, n()/2),
            Y = nth(Y, n()/2)) # use the underlying median item as the group representative

### Make dodgr graph of minor roads
graph <- weight_streetnet(lines_minor, wt_profile = "motorcar")
graph_ids <- graph[,c("from_id","from_lon","from_lat")]
graph_ids <- unique(graph_ids)

junc_majmi <- cbind(junc_majmi, st_coordinates(junc_majmi))
junc_majmi <- left_join(junc_majmi, graph_ids, by = c("X" = "from_lon",
                                                      "Y" = "from_lat"))
minor_cent <- left_join(minor_cent, graph_ids, by = c("X" = "from_lon",
                                                      "Y" = "from_lat"))

# For each minor road centroid, find the nearest (in time) junction  --------
dists <- dodgr_times(graph,
                     from = junc_majmi$from_id,
                     to = minor_cent$from_id,
                     shortest = FALSE)

saveRDS(dists,"Data/03_dists_matrix.Rds")

nearst_junction <- list()
for(i in 1:ncol(dists)){
  sub <- dists[,i]
  sub <- sub[!is.na(sub)]
  if(length(sub) == 0){
    nearst_junction[[i]] <- NA
  } else {
    mindist <- names(sub)[sub == min(sub, na.rm = TRUE)] 
            #find the minimum value and return the name of the minimum value
    if(length(mindist) == 0){
      mindist <- NA
    }
    if(length(mindist) > 1){ 
      mindist <- mindist[1]
    }
    nearst_junction[[i]] <- mindist
  }
  
}
nearst_junction <- unlist(nearst_junction)

lines_minor$nearst_junction <- nearst_junction
lines_minor$major_aadt <- junc_majmi$aadt[match(lines_minor$nearst_junction,
                                                junc_majmi$from_id)]
# plot each road colored by the AADT on the nearest (in time) major road

lines_minor <- st_transform(lines_minor, 27700)
#lines_minor_sub<-st_intersection(lines_minor,bound)
qtm(lines_minor, lines.col = "major_aadt", lines.lwd = 2)


st_write(lines_minor, "Data/03_lines_minor.gpkg", delete_dsn = TRUE)
st_write(minor_cent, "Data/03_minor_cent.gpkg", delete_dsn = TRUE)
st_write(junc_majmi, "Data/03_junction_major_minor.gpkg", delete_dsn = TRUE)


