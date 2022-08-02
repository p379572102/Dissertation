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
# 
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

### Load and prepare the data 
lines_minor <- readRDS("Data/07_lines_minor.RDS")
lines_minor_test <- lines_minor[!is.na(lines_minor$aadt), ]
col_name <- c("centrality", "nearest_junc_dist", "major_aadt", "road_density", "cars", "pop",
              "employ", "aadt")
lines_minor_test <- lines_minor_test[ ,col_name] 
lines_minor_test[(is.na(lines_minor_test))] <- 0


### Descriptive analysis
summary(lines_minor_test$aadt)
summary(is.na(lines_minor_test))


### obtain a matrix of pearson correlation coefficients
minor_cortest <- st_set_geometry(lines_minor_test, NULL)
cormat <- cor(minor_cortest, use = "complete.obs", method = "pearson")

# obtain the correlation coefficients 
corrplot(cormat, method = 'number') # colorful number

# significance test
sig1 <- corrplot::cor.mtest(minor_cortest, conf.level = .95) 

# create a correlogram
corrplot::corrplot(cormat, type = "lower", 
                   method = "circle",
                   order = "original",
                   ti.cex = 0.7,
                   p.mat = sig1$p, sig.level = 0.05,
                   col = viridis::viridis(100, option = "plasma"),
                   diag = FALSE)

##################################################
### Statistical model                          ###
##################################################

### Simple model equation 1
eq1 <- aadt ~ centrality + nearest_junc_dist + major_aadt 
model1 <- lm(formula = eq1, data = lines_minor_test)
summary(model1)
# 0.1526   0.1375
# with the existance of major_aadt, the adjusted R-squared is lower!!!
plot(lines_minor_test$aadt[!is.na(lines_minor_test$aadt)], predict(model1),
     main = "Model1 observed vs predicted results",
     ylab = "Predicted AADT",
     xlab= "Observed AADT")
abline(0,1, col = "red")
cor(lines_minor_test$aadt, predict(model1))

# Assess multicollinearity
vif(model1)
       # The VIFs are below 10 indicating that multicollinearity is not highly problematic.

#predicts the future values
lines_minor$ols_model1_aadt <- predict(model1, newdata = lines_minor)


### Logit model equation 2
eq2 <- aadt ~ log(centrality) + log(major_aadt) + log(nearest_junc_dist) 
model2 <- lm(formula = eq2, data = lines_minor_test)
summary(model2)
# 0.3101   0.2978 
plot(lines_minor_test$aadt, predict(model2),
     main = "Model2 observed vs predicted results",
     ylab = "Predicted AADT",
     xlab= "Observed AADT")
abline(0,1, col = "red")
cor(lines_minor_test$aadt, predict(model2))

# Assess multicollinearity
vif(model2)

#predicts the future values
lines_minor$ols_model2_aadt <- predict(model2, newdata = lines_minor)


### Logit model equation 3
eq3 <- aadt ~ log(centrality) + log(major_aadt) + log(nearest_junc_dist) + road_density 
model3 <- lm(formula = eq3, data = lines_minor_test)
summary(model3)
# 0.3105   0.294 
# with the existance of road_density, the adjusted R-squared is lower!!!
plot(lines_minor_test$aadt, predict(model3),
     main = "Model3 observed vs predicted results",
     ylab = "Predicted AADT",
     xlab= "Observed AADT")
abline(0,1, col = "red")
cor(lines_minor_test$aadt, predict(model3))

# Assess multicollinearity
vif(model3)


### Logit model equation 4
eq4 <- aadt ~ log(centrality) + log(major_aadt) + log(nearest_junc_dist) + pop + employ 
model4 <- lm(formula = eq4, data = lines_minor_test)
summary(model4)
# 0.3646   0.3455 
# with the existance of road_density, the adjusted R-squared is lower!!!
plot(lines_minor_test$aadt, predict(model3),
     main = "Model3 observed vs predicted results",
     ylab = "Predicted AADT",
     xlab= "Observed AADT")
abline(0,1, col = "red")
cor(lines_minor_test$aadt, predict(model4))

# Assess multicollinearity
vif(model3)


### Logit model equation 5
eq5 <- aadt ~ log(centrality) + log(major_aadt) + log(nearest_junc_dist) + employ +cars
model5 <- lm(formula = eq5, data = lines_minor_test)
summary(model5)
# 0.3582   0.3388 
# with the existance of road_density, the adjusted R-squared is lower!!!
plot(lines_minor_test$aadt, predict(model5),
     main = "Model5 observed vs predicted results",
     ylab = "Predicted AADT",
     xlab= "Observed AADT")
abline(0,1, col = "red")
cor(lines_minor_test$aadt, predict(model4))

# Assess multicollinearity
vif(model3)

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
0.3184031 


# write gwr output into a data frame
fb_gwr1_out <- as.data.frame(fb_gwr1$SDF)

cents_test$fmb1_localR2 <- fb_gwr1_out$localR2

# map


#predicts the future values
cents_lines_minor <- st_centroid(lines_minor)
cents_lines_sp <- as(cents_lines_minor, "Spatial")
gc()
gwr.predict(eq1, data = cents_lines_sp, bw = fbw1, kernel = "gaussian")


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
0.4135193 


### GWR model equation 3 with fixed bandwidth
# find optimal kernel bandwidth using cross validation
fbw3 <- gwr.sel(eq4, 
                data = cents_test, 
                coords= cbind(cents_test$X, cents_test$Y),
                longlat = FALSE,
                adapt=FALSE, 
                gweight = gwr.Gauss, 
                verbose = FALSE)

# view selected bandwidth
fbw3

# fit a gwr based on fixed bandwidth
fb_gwr3 <- gwr(eq4, 
               data = cents_test,
               coords = cbind(cents_test$X, cents_test$Y),
               longlat = FALSE,
               bandwidth = fbw2, 
               gweight = gwr.Gauss,
               hatmatrix=TRUE, 
               se.fit=TRUE)

fb_gwr3
0.4850732 


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
0.3031827


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
0.3441905


### GWR model equation 3 with adaptive bandwidth
# find optimal kernel bandwidth using cross validation
abw3 <- gwr.sel(eq4, 
                data = cents_test, 
                coords = cbind(cents_test$X, cents_test$Y),
                longlat = FALSE,
                adapt = TRUE, 
                gweight = gwr.Gauss, 
                verbose = FALSE)

# view selected bandwidth
abw3

# fit a gwr based on adaptive bandwidth
ab_gwr3 <- gwr(eq4, 
               data = cents_test,
               coords = cbind(cents_test$X, cents_test$Y),
               longlat = TRUE,
               adapt = abw2, 
               gweight = gwr.Gauss,
               hatmatrix=TRUE, 
               se.fit=TRUE)

ab_gwr3
0.4187791

##################################################
### GWPR model                                  ###
##################################################

# find optimal kernel bandwidth using cross validation
cents_test_sp <- as(cents_test, "Spatial")
DM<-gw.dist(dp.locat = cbind(cents_test$X, cents_test$Y))

##错误的地方，得仔细找一下原因，当变成自适应的时候，这条代码就没有问题
gfbw1 <- bw.ggwr(eq1,
                 data = cents_test_sp,
                 family = "poisson",
                 approach = "AICc",
                 kernel = "gaussian", 
                 adaptive = FALSE,
                 dMat = DM)
gfbw1


# fit a gwpr based on fixed bandwidth
fb_gwpr1 <- ggwr.basic(eq1, 
                       data = as(cents_test, "Spatial"), 
                       bw = gfbw1, 
                       family = "poisson",
                       kernel = "gaussian",#可以再确认一下用哪种
                       adaptive = FALSE)

fb_gwpr1
0.3622436/0.2125138
