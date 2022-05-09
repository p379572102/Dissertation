# Aim: To calculate road centrality
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
lines<-st_read("Data/02_iow_lines_all.gpkg")
lines_major<-st_read("Data/03_lines_major.gpkg")
lines_minor<-st_read("Data/03_lines_minor.gpkg")
minor_cent<-st_read("Data/03_minor_cent.gpkg")
### Split the minor roads into zones divided by the major road network 

### Make subgraphs
road_cut <- st_cast(lines_major$geom, "LINESTRING")

minor_points <- st_cast(lines_minor$geom, "POINT")
minor_points <- st_transform(minor_points, 27700)
all_points <- st_cast(lines$geom, "POINT")

minor_hull <- concaveman::concaveman(st_as_sf(all_points), concavity = 2)
minor_hull <- st_make_valid(minor_hull)
minor_hull <- st_collection_extract(minor_hull)

zones <- lwgeom::st_split(minor_hull, road_cut)
zones <- st_collection_extract(zones)
zones <- st_as_sf(zones)
zones_inter <- st_contains_properly(zones, minor_points)
zones$npoints <- lengths(zones_inter)
zones <- zones[zones$npoints > 5, ]
zones$id <- 1:nrow(zones)
zones <- st_transform(zones, 4326)

# plot the zones created
qtm(zones, fill = "id")

minor_cent <- st_as_sf(minor_cent, coords = c("X","Y"), crs = 4326,
                       remove = FALSE)

# Free up memory
rm(road_cut, points, zones_inter, both_int,minor_int, minor_hull,
   minor_points, all_points, dists, graph, graph_ids, nearst_junction,
   major_int, mindist, sub)
gc()


# Loop over each zone and find the centrality of the minor road ne --------
graphs <- list()

for(i in zones$id){
  message(paste0("Doing Zone ",i))
  zone_sub <- st_buffer(zones[zones$id == i, ], 0.0001)
  lines_sub <- lines_minor[zone_sub, , op = st_within]
  cents_sub <- minor_cent[zone_sub, , op = st_within]
  # qtm(zone_sub) +
  #   qtm(osm_sub) +
  #   qtm(cents_sub)
  if(nrow(lines_sub) > 0){
    graph_sub <- weight_streetnet(lines_sub, wt_profile = "motorcar")
    graph_sub <- dodgr_centrality(graph_sub)
    graph_sub <- merge_directed_graph(graph_sub)
    clear_dodgr_cache()
    graph_sub <- dodgr_to_sf(graph_sub)
    #graph_sub <- dodgr_central(graph_sub)
    graphs[[i]] <- graph_sub
  }


}

saveRDS(graphs,"Data/04-graphs.RDS")

graphs <- bind_rows(graphs)

# Plot the centrality of all minor roads
summary(graphs$centrality)

tm_shape(graphs) +
  tm_lines(col = "centrality", lwd = 3, style = "jenks")



# Match Up centrality with major aadt -----------------------------------------
summary(unique(graphs$way_id) %in% unique(osm_minor$osm_id))
summary(duplicated(graphs$way_id)) # some osm_ids have been split

graphs <- left_join(graphs,
                       st_drop_geometry(osm_minor[,c("osm_id","major_aadt")]),
                       by = c("way_id" = "osm_id"))

# Graphs is a sf df of minor roads with a major_aadt and centrality --------
head(graphs)

