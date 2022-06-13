# Aim: To calculate road centrality
# Rework of original file dropping the splitting an buffering stage

### Clear memory
rm(list = ls())

### Load Packages 

library(ggplot2)
library(sf)
library(dodgr)
library(tmap)
library(dplyr)
library(concaveman)
tmap_mode("view")

### Load Data 
lines<-st_read("Data/02_lines.gpkg") 
lines_major<-st_read("Data/03_lines_major.gpkg")
lines_minor<-st_read("Data/03_lines_minor.gpkg")
minor_cent<-st_read("Data/03_minor_cent.gpkg")


### Make subgraphs
road_cut <- st_cast(lines_major$geom, "LINESTRING") # used for the bound for the subgraphs

minor_points <- st_cast(lines_minor$geom, "POINT")
minor_points <- st_transform(minor_points, 27700)
all_points <- st_cast(lines$geom, "POINT")

minor_hull <- concaveman::concaveman(st_as_sf(all_points), concavity = 2)
        # make a whole concaveman containing all the points
minor_hull <- st_make_valid(minor_hull)
minor_hull <- st_collection_extract(minor_hull)

zones <- lwgeom::st_split(minor_hull, road_cut)
        #use major road to cut the generated concaveman
zones <- st_collection_extract(zones)
        #extract the zones as a data frame
zones <- st_as_sf(zones)
zones_inter <- st_contains_properly(zones, minor_points)
        #calculate the number of the minor road points that contained in subzones 
zones$npoints <- lengths(zones_inter)
zones <- zones[zones$npoints > 5, ] # use 5 as the threshold to filter the analyse zones
zones$id <- 1:nrow(zones)
zones <- st_transform(zones, 4326)

# plot the zones created
qtm(zones, fill = "id")

minor_cent <- st_as_sf(minor_cent, coords = c("X","Y"), crs = 4326,
                       remove = FALSE)

# Free up memory
rm(road_cut, zones_inter,  minor_hull, minor_points, all_points )
gc()

lines_minor <- st_transform(lines_minor, 4326)
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

graphs <- bind_rows(graphs)
saveRDS(graphs,"Data/04-graphs.RDS")

# Plot the centrality of all minor roads
summary(graphs$centrality)

tm_shape(graphs) +
  tm_lines(col = "centrality", lwd = 3, style = "fisher")



# Match Up centrality with minor aadt -----------------------------------------
summary(unique(graphs$way_id) %in% unique(lines_minor$osm_id))
summary(duplicated(graphs$way_id)) # some osm_ids have been split

graphs <- left_join(graphs,
                       st_drop_geometry(lines_minor[,c("osm_id", "nearst_junc_dist", 
                                                       "major_aadt")]),
                       by = c("way_id" = "osm_id"))


st_write(graphs, "Data/04_lines_minor.gpkg", delete_dsn = TRUE)


