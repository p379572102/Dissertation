# Aim: To calculate road density

### Clear memory
rm(list = ls())


### Load Packages 
# Process spatial objects
library(sf)
# Map view
library(tmap)
# Data frame manipulation
library(dplyr)
# Split roads
library(stplanr)
#library(maptools)

tmap_mode("view")


### Load Data 
lsoa_Camb <- st_read("Data/00_LSOA_Cambridgeshire.gpkg")
lines_minor <- st_read("Data/04_lines_minor.gpkg")
bound<-st_read("Data/00_bound_buf.gpkg")


###############################################
### Find the lsoa within the bound          ###
###############################################

bound_cut <- st_cast(bound$geom, "LINESTRING") 
lines_minor <- st_transform(lines_minor, 27700)

### split the lsoa by the bound
lsoa_Camb <- lwgeom::st_split(lsoa_Camb, bound_cut)
lsoa_Camb <- st_collection_extract(lsoa_Camb) 
lsoa_Camb <- st_as_sf(lsoa_Camb)
                      
### filter the lsoa that are within the bound
lsoa_cent <- st_centroid(lsoa_Camb)
cent_inter <- as.data.frame(st_intersects(bound, lsoa_cent))
lsoa_Camb <- lsoa_Camb[cent_inter$col.id, ]
summary(duplicated(lsoa_Camb$geo_code))
                            # to check if there are some lsoa with more than one zone
lsoa_Camb_undup <- lsoa_Camb[!duplicated(lsoa_Camb$geo_code), ]
lsoa_Camb_dup <-lsoa_Camb[duplicated(lsoa_Camb$geo_code), ]

for (i in 1:nrow(lsoa_Camb_dup)){
  geo_code <- lsoa_Camb_dup$geo_code[i]
  for (k in 1:nrow(lsoa_Camb_undup)){
    if (lsoa_Camb_undup$geo_code[k] == lsoa_Camb_dup$geo_code[i]){
      lsoa_Camb_undup$geom[k] <- st_union(lsoa_Camb_undup$geom[k], lsoa_Camb_dup$geom[i])
      # qtm(lsoa_Camb_undup$geom[k])
      }
   }
}


qtm(bound) + qtm(lsoa_Camb_undup, fill = NULL)
rm(bound_cut, lsoa_cent, cent_inter, lsoa_Camb_dup, lsoa_Camb)

st_write(lsoa_Camb_undup, "Data/05_lsoa_Camb.gpkg", delete_dsn = TRUE)


#############################################
### Calculate road density                ###
#############################################
lsoa_Camb$id <-1:nrow(lsoa_Camb)
density_lsoa <- list()
for(i in lsoa_Camb$id){
  message(paste0("Doing lsoa ",i))
  lsoa_sub <- lsoa_Camb[i,]
  lines_sub <- st_intersection(lsoa_sub, lines_minor)

  #qtm(bound) + qtm(lsoa_sub,fill = NULL) + qtm(lines_sub, lines.col = "blue")
  
  road_length <- sum(as.numeric(st_length(lines_sub))) / 1000
  lsoa_area <- as.numeric(st_area(lsoa_sub)%>% units::set_units(km^2))
  
  res <- data.frame(id = lsoa_sub$id,
                    road_km = road_length,
                    area_km2 = lsoa_area,
                    road_density = road_length / lsoa_area)
  
  density_lsoa[[i]] <- res
}

density_lsoa <- bind_rows(density_lsoa)
summary(is.na(density_lsoa$road_density))
summary(unique(density_lsoa$id) %in% unique(lsoa_Camb$id))
lsoa_Camb <- left_join(lsoa_Camb, density_lsoa, by = c("id" = "id"))

st_write(lsoa_Camb, "Data/05_density_lsoa.gpkg", delete_dsn = TRUE)

tm1 <- qtm(lsoa_Camb, fill = "road_density")
tmap_save(tm1, filename = "Plot/05_lsoa_road_density.png")


#########################################################
### Allocate road density to the minor road           ###
#########################################################

#lines_cut <- st_cast(lsoa_Camb$geom, "LINESTRING") 
#lines_minor.sub <- lines_minor[lines_cut, ,op = st_intersects]
#lines_break <- lines_minor.sub$geom
#lsoa_break <- lsoa_Camb$geom
#lines_minor.break <- line_breakup(lines_break, lsoa_break)
#lines_minor.sub <- lwgeom::st_split(lines_minor.sub, lines_cut) 
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

#foo <- st_coordinates(lines_minor3)
#foo <- as.data.frame(foo)
#foo2 <- foo %>% 
#  group_by(L1) %>%
#  summarise(X = X[round(n()/2)],
#            Y = Y[round(n()/2)])
#midpo_minor <- st_as_sf(foo2, coords = c("X","Y"), crs = 27700)
#st_crs(midpo_minor) <- 27700

midpo_minor <- line_midpoint(lines_minor3) %>% st_as_sf()
midpo_minor <- st_join(midpo_minor, lsoa_Camb[,c("geo_code","area_km2","road_km", "road_density")])
length(st_intersects(midpo_minor, lsoa_Camb))
summary(is.na(midpo_minor$road_density)) #3 minor midpoint cannot get data

qtm(lines_minor3[1:1000, ],lines.col = "blue") + 
  qtm(midpo_minor[1:1000, ])

midpo_minor$id <-  1:nrow(midpo_minor)
lines_minor4 <- left_join(lines_minor3, st_drop_geometry(midpo_minor), by = "id")
summary(is.na(lines_minor4$road_density)) #3 minor road cannot get data

tm2 <- qtm(bound) + qtm(lines_minor4, lines.col = "road_density", lines.lwd = 3)
tmap_save(tm2, filename = "Plot/05_road_density.png")

st_write(lines_minor4, "Data/05_lines_minor.gpkg", delete_dsn = TRUE)
