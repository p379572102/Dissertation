# Aim: To calculate AADT for minor roads
# This script assumes the data has already been cleaned and the AADT for major roads assigned
# Rework of original file dropping the splitting an buffering stage

### Clear memory-----------------------------------------------------------
rm(list = ls())

# Load Packages -----------------------------------------------------------

library(ggplot2)
library(sf)
library(tmap)
library(corrplot)
tmap_mode("view")

### Load Data 
traffic_2018.minor <- readRDS("Data/02-traffic_2018.minor.RDS")
lines_minor <- st_read("Data/06_lines_minor.gpkg")
bound<-st_read("Data/00_bound_buf.gpkg")

###Get minor road AADT
lines_minor_buf <- st_buffer(lines_minor, 6)
lines_minor_buf <- st_join(lines_minor_buf, traffic_2018.minor[,c("aadt")])
summary(is.na(lines_minor_buf$aadt))
              # Check if the traffic counts number is right

lines_minor$aadt <-  lines_minor_buf$aadt[match(lines_minor$id,
                                                lines_minor_buf$id)]
summary(is.na(lines_minor$aadt))# Check if the traffic counts number is right, need to fix!!
tm1 <- qtm(bound) + qtm(lines_minor, lines.wid =1) + qtm(traffic_2018.minor, dots.col = "aadt") 
tmap_save(tm1, filename = "Plot/07_minor_aadt.png")

saveRDS(lines_minor, "Data/07_lines_minor.RDS")
