# Aim: To calculate road density

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
lines_minor <- st_read( "Data/04_lines_minor.gpkg")


### Calculate the road density
lines_minor_cent <- st_centroid(lines_minor)
cent_buf <- st_buffer(lines_minor_cent,dist = 1000)

for(i in 1:nrow(cent_buf)){
  cent_buf.sub <- cent_buf[i,]
  inter_cent <- lines_minor[cent_buf.sub, , op=st_intersects]

