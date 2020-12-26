library(spatstat)
library(sf)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ape)
library(caret)

zone1 <- read.csv("zone1.txt")

unique(zone1$class)

################## ZONE 1 ##########################

zone1.church <- subset(zone1, class == "church", select = c("long", "lat", "class"))
zone1.sportbuilding <-  subset(zone1, class == "sport_building", select = c("long", "lat", "class"))

zone1.garage <-
  subset(zone1, class == "garage", select = c("long", "lat", "class"))

zone1.singlehouse <-
  subset(zone1, class == "single_house", select = c("long", "lat", "class"))

zone1.collectivehouse <-
  subset(zone1,
         class == "collective_house",
         select = c("long", "lat", "class"))

zone1.commercialbuilding <-
  subset(zone1,
         class == "commercial_building",
         select = c("long", "lat", "class"))

zone1.lightbuilding <-
  subset(zone1,
         class == "light_building",
         select = c("long", "lat", "class"))

ggplot(zone1, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum() + ggtitle("Zone 1 Buildings")

# commercial buildings
summary(zone1.commercialbuilding)
zone1.commercialbuilding.ppp <-
  ppp(
    zone1.commercialbuilding$long,
    zone1.commercialbuilding$lat,
    c(7.7, 7.8),
    c(48.6, 48.7),
    marks = factor(zone1.commercialbuilding$class)
  )

K_cb <- envelope(zone1.commercialbuilding.ppp, Kest, correction = "Ripley", nsim=100)
plot(K_cb, main="Ripley's K-function of Zone 1 Commercial Buildings")



# collective houses
summary(zone1.collectivehouse)
zone1.collectivehouse.ppp <-
  ppp(
    zone1.collectivehouse$long,
    zone1.collectivehouse$lat,
    c(7.7, 7.8),
    c(48.6, 48.7),
    marks = factor(zone1.collectivehouse$class)
  )

K_lb <- envelope(zone1.collectivehouse.ppp, Kest, correction = "Ripley", nsim=100)
plot(K_lb, main="Ripley's K-function of Zone 1 Collective Houses")  
