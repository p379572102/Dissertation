lsoa <- read_sf("E:/Users/earmmor/OneDrive - University of Leeds/Data/OA Bounadries/GB_LSOA_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp")
lsoa <- lsoa[substr(lsoa$geo_code,1,1) == "E",]

st_write(lsoa, "Data/LSOA_England.gpkg")
saveRDS(lsoa, "Data/LSOA_England.Rds")

#buff <- read_sf("Data/00_bound_buf.gpkg", delete_dsn = TRUE)
buff <- readRDS("Data/00_osm_raw.RDS")
buff <- buff$osm_lines


qtm(buff[sample(1:nrow(buff), 100),])

buff <- st_convex_hull(st_combine(buff))
qtm(buff)

buff <- st_transform(buff, 27700)

lsoa2 <- lsoa[buff,]
qtm(lsoa2)

saveRDS(lsoa2, "Data/LSOA_Cambridgeshire.Rds")
