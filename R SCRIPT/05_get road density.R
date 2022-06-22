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

  #qtm(bound) + qtm(lsoa_sub,fill = NULL) + qtm(line_sub, lines.col = "blue")
  
  lines_sub <- st_union(lines_sub)
  road_length <- st_length(lines_sub)%>% units::set_units(km)

  lsoa_sub$length <- road_length
  lsoa_sub$density <- lsoa_sub$length / lsoa_sub$area
  
  density_lsoa[[i]] <- lsoa_sub
}

density_lsoa <- bind_rows(density_lsoa)

st_write(density_lsoa, "Data/05_density_lsoa.gpkg", delete_dsn = TRUE)


### Allocate road density to the minor road
#lines_cut <- st_cast(lsoa_Camb$geom, "LINESTRING") 

#lines_minor.sub <- lines_minor[lines_cut, ,op = st_intersects]
#lines_break <- lines_minor.sub$geom
#lsoa_break <- lsoa_Camb$geom
#lines_minor.break <- line_breakup(lines_break, lsoa_break) !!!take so long !!!
#lines_minor.sub <- lwgeom::st_split(lines_minor.sub, lines_cut) !!!take so long !!!
#lines_minor.sub <- st_collection_extract(lines_minor.sub)
#lsoa_Camb <- st_as_sf(lsoa_Camb)


midpo_minor <- line_midpoint(lines_minor) %>% st_as_sf()#fix this function!!!
midpo_minor <- st_join(midpo_minor, density_lsoa[,c("area","length", "density")])
                     #avoid the situation that one road cross more than one lsoa
st_crs(midpo_minor)
qtm(lines_minor[1:1000, ],lines.col = "blue") + 
  qtm(midpo_minor[1:1000, ])
  
lines_minor <- st_join(lines_minor, midpo_minor[,c("area","length", "density")])


st_write(lines_minor, "Data/05_lines_minor.gpkg", delete_dsn = TRUE)
