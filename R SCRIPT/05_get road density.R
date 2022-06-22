# Aim: To calculate road density

### Clear memory
rm(list = ls())


### Load Packages 
library(ggplot2)
library(sf)
library(tmap)
library(dplyr)
library(stplanr)
tmap_mode("view")


### Load Data 
lsoa_Camb <- st_read("Data/00_LSOA_Cambridgeshire.gpkg")
lines_minor <- st_read("Data/04_lines_minor.gpkg")
bound<-st_read("Data/00_bound_buf.gpkg")


### Calculate the lsoa area
bound_cut <- st_cast(bound$geom, "LINESTRING") 
lines_minor <- st_transform(lines_minor, 27700)

lsoa_Camb <- lwgeom::st_split(lsoa_Camb, bound_cut)
lsoa_Camb <- st_collection_extract(lsoa_Camb)
lsoa_Camb <- st_as_sf(lsoa_Camb)
                      # split the lsoa within and not within the bound

lsoa_cent <- st_centroid(lsoa_Camb)
cent_inter <- as.data.frame(st_intersects(bound, lsoa_cent))
lsoa_Camb <- lsoa_Camb[cent_inter$col.id, ]
                      # filter the lsoa not within the bound

lines_inter <- st_intersects(lsoa_Camb,lines_minor)

lsoa_Camb_sub <- list()
for (i in 1:nrow(lsoa_Camb)){
  if (length(lines_inter[[i]])>0){
    lsoa_Camb_sub[[i]] <- lsoa_Camb[i,]
  } 
}

lsoa_Camb <- bind_rows(lsoa_Camb_sub)
#geo_code <- unique(lsoa_Camb$geo_code)
#geo_code.sub <- geo_code[!geo_code %in% lsoa_Camb_sub$geo_code]

lsoa_Camb$area <- st_area(lsoa_Camb) %>% units::set_units(km^2)

qtm(bound) + qtm(lsoa_Camb,fill = NULL)
rm(bound_cut, lsoa_Camb_sub, lines_inter, lsoa_cent, cent_inter)

st_write(lsoa_Camb, "Data/05_lsoa_Camb.gpkg", delete_dsn = TRUE)


### Calculate road density for each lsoa
lsoa_Camb$id <- 1:nrow(lsoa_Camb)

density_lsoa <- list()
for(i in lsoa_Camb$id){
  message(paste0("Doing lsoa ",i))
  lsoa_sub <- lsoa_Camb[i,]
  lines_sub <- st_intersection(lsoa_sub, lines_minor)

  #qtm(bound) + qtm(lsoa_sub,fill = NULL) + qtm(lines_sub, lines.col = "blue")
  
  road_length <- sum(as.numeric(st_length(lines_sub))) / 1000
  lsoa_area <- as.numeric(lsoa_sub$area)
  
  res <- data.frame(lsoa = lsoa_sub$geo_code,
                    road_km = road_length,
                    area_km2 = lsoa_area,
                    road_density = road_length / lsoa_area)
  
  
  # lines_sub <- st_union(lines_sub)
  # road_length <- st_length(lines_sub)%>% units::set_units(km)
  # 
  # lsoa_sub$length <- road_length
  # lsoa_sub$density <- lsoa_sub$length / lsoa_sub$area
  # 
  # density_lsoa[[i]] <- lsoa_sub
  
  density_lsoa[[i]] <- res
}

density_lsoa <- bind_rows(density_lsoa)

lsoa_Camb <- left_join(lsoa_Camb, density_lsoa, by = c("geo_code" = "lsoa"))

st_write(lsoa_Camb, "Data/05_density_lsoa.gpkg", delete_dsn = TRUE)

qtm(lsoa_Camb, fill = "road_density")

### Allocate road density to the minor road
#lines_cut <- st_cast(lsoa_Camb$geom, "LINESTRING") 

#lines_minor.sub <- lines_minor[lines_cut, ,op = st_intersects]
#lines_break <- lines_minor.sub$geom
#lsoa_break <- lsoa_Camb$geom
#lines_minor.break <- line_breakup(lines_break, lsoa_break) !!!take so long !!!
#lines_minor.sub <- lwgeom::st_split(lines_minor.sub, lines_cut) !!!take so long !!!
#lines_minor.sub <- st_collection_extract(lines_minor.sub)
#lsoa_Camb <- st_as_sf(lsoa_Camb)



line_segment <- function(l, n_segments, segment_length = NA) {
  if (!is.na(segment_length)) {
    l_length <- as.numeric(sf::st_length(l))
    n_segments <- ceiling(l_length / segment_length)
  }
  # browser() # tests
  # first_linestring = lwgeom::st_linesubstring(x = l, from = 0, to = 0.2)
  from_to_sequence = seq(from = 0, to = 1, length.out = n_segments + 1)
  line_segment_list = lapply(seq(n_segments), function(i)
    lwgeom::st_linesubstring(
      x = l,
      from = from_to_sequence[i],
      to = from_to_sequence[i + 1]
    )
  )
  do.call(rbind, line_segment_list)
}


lines_minor$id <- 1:nrow(lines_minor)
lines_minor <- lines_minor %>%
  group_by(id) %>%
  group_split()


library(pbapply)

cl <- parallel::makeCluster(7)
parallel::clusterExport(cl, "line_segment", envir = environment())
lines_minor2 <- pblapply(lines_minor, function(x){
  line_segment(x, segment_length = 1000)
}, cl = cl)
parallel::stopCluster(cl)

lines_minor3 <- bind_rows(lines_minor2)

write_sf(lines_minor3, "Data/05_minor_roads_split_1km.gpkg")

# t1 <- Sys.time()
# lines_minor2 <- lapply(lines_minor[1:100], function(x){
#   line_segment(x, segment_length = 1000)
# })
# t2 <- Sys.time()
# difftime(t2, t1)

lines_minor3$id <- 1:nrow(lines_minor3)

foo <- st_coordinates(lines_minor3)
foo <- as.data.frame(foo)
foo2 <- foo %>% 
  group_by(L1) %>%
  summarise(X = X[round(n()/2)],
            Y = Y[round(n()/2)])
midpo_minor <- st_as_sf(foo2, coords = c("X","Y"), crs = 27700)
st_crs(midpo_minor) <- 27700

#midpo_minor <- line_midpoint(lines_minor3) %>% st_as_sf()#fix this function!!!
midpo_minor <- st_join(midpo_minor, lsoa_Camb[,c("geo_code","area_km2","road_km", "road_density")])
midpo_minor$id <-  1:nrow(midpo_minor)

                     #avoid the situation that one road cross more than one lsoa
st_crs(midpo_minor)
qtm(lines_minor3[1:1000, ],lines.col = "blue") + 
  qtm(midpo_minor[1:1000, ])
  
#lines_minor4 <- st_join(lines_minor3, midpo_minor[,c("area_km2","road_km", "road_density")])

lines_minor4 <- left_join(lines_minor3, st_drop_geometry(midpo_minor), by = "id")

qtm(lines_minor4[1:10000,], lines.col = "geo_code")

st_write(lines_minor4, "Data/05_lines_minor.gpkg", delete_dsn = TRUE)
