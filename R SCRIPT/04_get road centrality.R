# Aim: To calculate road centrality
# Rework of original file dropping the splitting an buffering stage

### Clear memory
rm(list = ls())

### Load Packages 
# Process spatial objects
library(sf)
# Make dodgr graph
library(dodgr)
# Data frame manipulation
library(dplyr)
# Map view
library(tmap)
#library(ggplot2)
#library(concaveman)

tmap_mode("view")

### Load Data 
bound<-st_read("Data/00_bound_buf.gpkg")
#lines <- st_read("Data/01_network.gpkg") 
lines_major <- st_read("Data/02_lines_major.gpkg")
lines_minor <- st_read("Data/03_lines_minor.gpkg")
#midpo_minor <- st_read("Data/03_point_minor.gpkg")
#midpo_minor <- st_as_sf(midpo_minor, coords = c("X","Y"), crs = 4326)

# ################################################################# #
####   Make subgraphs                                            ####
# ################################################################# #

road_cut <- st_cast(lines_major$geom, "LINESTRING") 
                            # used for the bound for the subgraphs
minor_points <- st_cast(lines_minor$geom, "POINT")
minor_points <- st_transform(minor_points, 27700)


### make a whole concaveman containing all the points
# all_points <- st_cast(lines$geom, "POINT")
# minor_hull <- concaveman::concaveman(st_as_sf(all_points), concavity = 2)
                            
# minor_hull <- st_make_valid(minor_hull)
# minor_hull <- st_collection_extract(minor_hull)

### genarete the zones split by major road
zones <- lwgeom::st_split(bound, road_cut)
                            # use major road to cut the study bound
zones <- st_collection_extract(zones)
                            # extract the zones as a data frame
zones <- st_as_sf(zones)

zones_inter <- st_contains_properly(zones, minor_points)
                  # count the number of the minor road points that within subzones 
zones$npoints <- lengths(zones_inter)
zones <- zones[zones$npoints > 5, ] 
                  # use 5 as the threshold to filter the analyse zones
colSums(st_drop_geometry(zones))
                  # the result is 223813, which is less than the original point number
zones$id <- 1:nrow(zones)

tm1 <- qtm(zones, fill = "id")  # plot the zones created
tmap_save(tm1, filename = "Plot/04_zones.png",)

### Free up memory
rm(road_cut, zones_inter, all_points )
gc()


# ################################################################# #
####   Calculate road centrality                                 ####
# ################################################################# #

### prepare the crs for dodgr
zones <- st_transform(zones, 4326)
#midpo_minor <- st_as_sf(midpo_minor, coords = c("X","Y"), crs = 4326,
                        #remove = FALSE)
lines_minor <- st_transform(lines_minor, 4326)
minor_points <- st_transform(minor_points,4326)

### Loop over each zone and find the centrality of the minor road 
graphs <- list()

for(i in zones$id){
  message(paste0("Doing Zone ",i))
  zone_sub <- zones[zones$id == i, ]
  zone_sub <- st_transform(st_buffer(st_transform(zone_sub, 27700), 0.0001), 4326)
  lines_sub <- lines_minor[zone_sub, , op = st_within]
  #lines_sub2 <- st_intersection(zone_sub, lines_minor)
  #midpo_sub <- midpo_minor[zone_sub, , op = st_within]
  #summary(nrow(lines_sub) == nrow(midpo_sub))
    #qtm(zone_sub, fill = NULL) + qtm(lines_sub2, lines.col = "major_aadt") 
  if(nrow(lines_sub) > 0){
    graph_sub <- weight_streetnet(lines_sub, wt_profile = "motorcar")
    graph_sub <- dodgr_centrality(graph_sub)
    graph_sub <- merge_directed_graph(graph_sub)
    clear_dodgr_cache()
    graph_sub <- dodgr_to_sf(graph_sub)
    graph_sub$std_centrality <- graph_sub$centrality / zone_sub$npoints
    #summary(unique(graph_sub$way_id) %in% unique(lines_sub$osm_id))
    #summary(duplicated(graph_sub$way_id))
    #print(summary(graph_sub$centrality))
    graphs[[i]] <- graph_sub
  }


}

graphs <- bind_rows(graphs)
saveRDS(graphs,"Data/04-graphs.RDS")

# Plot the centrality of all minor roads
summary(!is.na(graphs$centrality))

tm2 <- qtm(bound) +
  tm_shape(graphs) +
  tm_lines(col = "std_centrality", lwd = 2, style = "fisher") + 
  qtm(lines_major, line.col = "ref" )
tmap_save(tm2, filename = "Plot/04_centrality.png")

# ################################################################# #
####   Match Up centrality with minor road                       ####
# ################################################################# #

summary(unique(graphs$way_id) %in% unique(lines_minor$osm_id))
                      # check if all the minor road get road centrality
summary(unique(lines_minor$osm_id) %in% unique(graphs$way_id))

summary(duplicated(graphs$way_id)) 
                      # some osm_ids have been split

#lines_minor2 <- st_cast(lines_minor$geom,"LINESTRING")
#lines_minor2 <- lwgeom::st_split(lines_minor2, road_cut)
#saveRDS(lines_minor2,"Data/04-lines_minor.RDS")

lines_minor <- left_join(graphs[,c("way_id" ,"centrality", "std_centrality")],
                       st_drop_geometry(lines_minor[ ,c("osm_id", "highway", 
                                                        "nearest_junc_dist", 
                                                        "major_flow")]),
                       by = c("way_id" = "osm_id"))

qtm(bound, fill = NULL) + qtm(lines_minor, lines.col = "centrality", lines.lwd = 2, )

st_write(lines_minor, "Data/04_lines_minor.gpkg", delete_dsn = TRUE)





