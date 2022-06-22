# Aim: Geographically Weighted Regression
# This script assumes the data has already been cleaned and the AADT for major roads assigned
# Rework of original file dropping the splitting an buffering stage

### Clear memory
rm(list = ls())

### Load Packages
# Data manipulation, transformation and visualisation
library(tidyverse)
# Nice tables
library(kableExtra)
library(sf)
# Spatial objects conversion
library(sp) 
# Fitting geographically weighted regression models
library(spgwr)
# Thematic maps
library(ggplot2)
library(tmap)
# Obtain correlation coefficients
library(corrplot)
# Assess multicollinearity
library(car)
# Predict the GWmodel
library(GWmodel)
tmap_mode("view")

### Load Data 
lines_minor <- readRDS("Data/07_lines_minor.RDS")
lines_minor_test <- lines_minor[!is.na(lines_minor$aadt), ]
col_name <- c("centrality", "nearest_junc_dist", "major_aadt", "density", "cars", "pop",
              "employ", "aadt")
lines_minor_test <- lines_minor_test[ ,col_name] 
lines_minor_test[(is.na(lines_minor_test))] <- 0
### Descriptive analysis


### obtain a matrix of pearson correlation coefficients
minor_cortest <- st_set_geometry(lines_minor_test, NULL)
cormat <- cor(minor_cortest, use = "complete.obs", method = "pearson")

# significance test
sig1 <- corrplot::cor.mtest(minor_cortest, conf.level = .95) 

#creat a correlogram
corrplot::corrplot(cormat, type = "lower", 
                   method = "circle",
                   order = "original",
                   ti.cex = 0.7,
                   p.mat = sig1$p, sig.level = 0.05,
                   col = viridis::viridis(100, option = "plasma"),
                   diag = FALSE)


##################################################
### OLS model                                  ###
##################################################

### OLS model equation 1
eq1 <- aadt ~ centrality + major_aadt + nearest_junc_dist + density
model1 <- lm(formula = eq1, data = lines_minor_test)
summary(model1)
# 0.1589   0.1382
plot(lines_minor_test$aadt[!is.na(lines_minor_test$aadt)], predict(model1),
     main = "Model1 observed vs predicted results",
     ylab = "Predicted AADT",
     xlab= "Observed AADT")
abline(0,1, col = "red")
cor(lines_minor_test$aadt[!is.na(lines_minor_test$centrality)], predict(model1))

# Assess multicollinearity
vif(model1)
       # The VIFs are below 10 indicating that multicollinearity is not highly problematic.

#predicts the future values
lines_minor$ols_model1_aadt <- predict(model1, newdata = lines_minor)


### OLS model equation 2
eq2 <- aadt ~ centrality + major_aadt + nearest_junc_dist + cars +pop +employ
model2 <- lm(formula = eq2, data = lines_minor_test)
summary(model2)
# 0.1855   0.1551 
plot(lines_minor_test$aadt[!is.na(lines_minor_test$aadt)], predict(model2),
     main = "Model2 observed vs predicted results",
     ylab = "Predicted AADT",
     xlab= "Observed AADT")
abline(0,1, col = "red")
cor(lines_minor_test$aadt[!is.na(lines_minor_test$centrality)], predict(model2))

# Assess multicollinearity
vif(model2)

#predicts the future values
lines_minor$ols_model2_aadt <- predict(model2, newdata = lines_minor)

##################################################
### GWR model                                  ###
##################################################
cents_test <- st_centroid(lines_minor_test)
coords_test <- as.data.frame(st_coordinates(cents_test))
cents_test$X <- coords_test$X
cents_test$Y <- coords_test$Y


### GWR model equation 1 with fixed bandwidth
# find optimal kernel bandwidth using cross validation
fbw1 <- gwr.sel(eq1, 
               data = cents_test, 
               coords= cbind(cents_test$X, cents_test$Y),
               longlat = FALSE,
               adapt=FALSE, 
               gweight = gwr.Gauss, 
               verbose = FALSE)

# view selected bandwidth
fbw1

# fit a gwr based on fixed bandwidth
fb_gwr1 <- gwr(eq1, 
              data = cents_test,
              coords = cbind(cents_test$X, cents_test$Y),
              longlat = FALSE,
              bandwidth = fbw1, 
              gweight = gwr.Gauss,
              hatmatrix=TRUE, 
              se.fit=TRUE)

fb_gwr1
0.3925431 

# write gwr output into a data frame
fb_gwr1_out <- as.data.frame(fb_gwr1$SDF)

cents_test$fmb1_localR2 <- fb_gwr1_out$localR2

# map


#predicts the future values
lines_minor_sp <- as(lines_minor, "Spatial")
gwr.predict(fb_gwr1, data = nc_sp, fbw1, kernel = "gaussian")



### GWR model equation 2 with fixed bandwidth
# find optimal kernel bandwidth using cross validation
fbw2 <- gwr.sel(eq2, 
               data = cents_test, 
               coords= cbind(cents_test$X, cents_test$Y),
               longlat = FALSE,
               adapt=FALSE, 
               gweight = gwr.Gauss, 
               verbose = FALSE)

# view selected bandwidth
fbw2

# fit a gwr based on fixed bandwidth
fb_gwr2 <- gwr(eq2, 
              data = cents_test,
              coords = cbind(cents_test$X, cents_test$Y),
              longlat = FALSE,
              bandwidth = fbw2, 
              gweight = gwr.Gauss,
              hatmatrix=TRUE, 
              se.fit=TRUE)

fb_gwr2
0.3871969 

### GWR model equation 1 with adaptive bandwidth
# find optimal kernel bandwidth using cross validation
abw1 <- gwr.sel(eq1, 
               data = cents_test, 
               coords = cbind(cents_test$X, cents_test$Y),
               longlat = FALSE,
               adapt = TRUE, 
               gweight = gwr.Gauss, 
               verbose = FALSE)

# view selected bandwidth
abw1

# fit a gwr based on adaptive bandwidth
ab_gwr1 <- gwr(eq1, 
              data = cents_test,
              coords = cbind(cents_test$X, cents_test$Y),
              longlat = TRUE,
              adapt = abw1, 
              gweight = gwr.Gauss,
              hatmatrix=TRUE, 
              se.fit=TRUE)

ab_gwr1
0.4917993


### GWR model equation 2 with adaptive bandwidth
# find optimal kernel bandwidth using cross validation
abw2 <- gwr.sel(eq2, 
                data = cents_test, 
                coords = cbind(cents_test$X, cents_test$Y),
                longlat = FALSE,
                adapt = TRUE, 
                gweight = gwr.Gauss, 
                verbose = FALSE)

# view selected bandwidth
abw2

# fit a gwr based on adaptive bandwidth
ab_gwr2 <- gwr(eq2, 
               data = cents_test,
               coords = cbind(cents_test$X, cents_test$Y),
               longlat = TRUE,
               adapt = abw2, 
               gweight = gwr.Gauss,
               hatmatrix=TRUE, 
               se.fit=TRUE)

ab_gwr2
0.2993182
