# Aim: To calculate AADT for minor roads
# This script assumes the data has already been cleaned and the AADT for major roads assigned
# Rework of original file dropping the splitting an buffering stage

# Load Packages -----------------------------------------------------------

library(ggplot2)
library(sf)
library(dodgr)
library(tmap)
library(dplyr)
library(concaveman)
tmap_mode("view")


# Compare measured AADT and centrality ------------------------------------
traffic_minor <- traffic[traffic$road %in% c("C","U"),]

# Buffer as traffic points are away from roads
traffic_minor <- st_buffer(traffic_minor, 33)
traffic_minor <- st_transform(traffic_minor, 4326)

traffic_minor <- st_join(traffic_minor, graphs)
summary(is.na(traffic_minor$centrality))

# some simple plots
plot(traffic_minor$centrality, traffic_minor$aadt)
abline(0,1, col = "red")
plot(traffic_minor$major_aadt, traffic_minor$aadt)

traffic_minor %>% ggplot(aes(x = aadt, y = centrality)) +
  geom_point(aes(colour = highway)) +
  geom_smooth(method="lm", se=F, fullrange=FALSE, level=0.95)

traffic_minor %>% ggplot(aes(x = aadt, y = major_aadt)) +
  geom_point(aes(color = highway)) +
  geom_smooth(method="lm", se=F, fullrange=FALSE, level=0.95)

traffic_minor %>% ggplot(aes(x = centrality, y = major_aadt)) +
  geom_point(aes(color = highway)) +
  geom_smooth(method="lm", se=F, fullrange=FALSE, level=0.95)

cor(traffic_minor$centrality, traffic_minor$aadt)
cor(traffic_minor$major_aadt, traffic_minor$aadt)

# simple model
m1 <- lm(aadt ~ centrality + major_aadt, data = traffic_minor)
summary(m1)
plot(traffic_minor$aadt[!is.na(traffic_minor$aadt)], predict(m1),
     main = "Model m1 observed vs predicted results",
     ylab = "Predicted AADT",
     xlab= "Observed AADT")
abline(0,1, col = "red")
cor(traffic_minor$aadt[!is.na(traffic_minor$centrality)], predict(m1))

# log model
m2 <- lm(aadt ~ log(centrality) + log(major_aadt), data = traffic_minor)
summary(m2)
plot(traffic_minor$aadt[!is.na(traffic_minor$aadt)], predict(m2),
     main = "Model m2 observed vs predicted results",
     ylab = "Predicted AADT",
     xlab= "Observed AADT")
abline(0,1, col = "red")
cor(traffic_minor$aadt[!is.na(traffic_minor$centrality)], predict(m2))

# Assign area type (urban and rural) ---------------------------------------

# Import Strategi shp

strategi <- st_read("data/strategi/urban_region.shp")
strategi <- st_transform(strategi, 4326)

# Classify as urban or rural

traffic_areatype <- st_join(traffic_minor, strategi)
traffic_areatype <- traffic_areatype %>%
  mutate(areatype = ifelse(LEGEND %in% c("Large Urban Area polygon"), "urban",
                           "rural"))

# log model with added variable
m2a <- lm(log(aadt) ~ log(centrality) + log(major_aadt) + areatype,
          data = traffic_areatype)
summary(m2a)
plot(traffic_areatype$aadt[!is.na(traffic_areatype$centrality)],
     exp(predict(m2a)),
     main = "Model m2a observed vs predicted results",
     ylab = "Predicted AADT",
     xlab= "Observed AADT")
abline(0,1, col = "red")
cor(traffic_areatype$aadt[!is.na(traffic_areatype$centrality)],
    exp(predict(m2a)))

table(traffic_areatype$areatype)

# log model poisson
hist(traffic_areatype$aadt, breaks = 20,
     main = "AADT distribution",
     xlab = "AADT")

m3 <- glm(aadt ~ log(centrality) + log(major_aadt) + areatype,
          data = traffic_areatype, family = "poisson")
summary(m3)
plot(traffic_areatype$aadt[!is.na(traffic_areatype$centrality)],
     exp(predict(m3)),
     main = "Model m3 observed vs predicted results",
     ylab = "Predicted AADT",
     xlab= "Observed AADT")
abline(0,1, col = "red")
cor(traffic_areatype$aadt[!is.na(traffic_areatype$centrality)],
    exp(predict(m3)))

rmse3 <- sum(sqrt((exp(predict(m3))-traffic_minor$aadt)^2))/22

data.frame(exp(predict(m3)), traffic_minor$aadt)

par(mfrow = c(1, 1))
plot(density(resid(m3, type='pearson')))
plot(density(rstandard(m3, type='pearson')))

plot(density(resid(m3, type='deviance')))

scatter.smooth(predict(m3, type='response'), rstandard(mleed, type='deviance'), col='gray')

par(mfrow = c(2, 2))
plot(m3)
