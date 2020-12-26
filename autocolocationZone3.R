library(spatstat)
library(sf)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ape)
library(caret)

zone3 <- read.delim("zone3.txt")

unique(zone3$class)

################## ZONE 1 ##########################

zone3.church <- subset(zone3, class == "church", select = c("long", "lat", "class"))
zone3.sportbuilding <-  subset(zone3, class == "sport_building", select = c("long", "lat", "class"))

zone3.garage <-
  subset(zone3, class == "garage", select = c("long", "lat", "class"))

zone3.singlehouse <-
  subset(zone3, class == "single_house", select = c("long", "lat", "class"))

zone3.collectivehouse <-
  subset(zone3,
         class == "collective_house",
         select = c("long", "lat", "class"))

zone3.commercialbuilding <-
  subset(zone3,
         class == "commercial_building",
         select = c("long", "lat", "class"))

zone3.lightbuilding <-
  subset(zone3,
         class == "light_building",
         select = c("long", "lat", "class"))

ggplot(zone3, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum() + ggtitle("Zone 3 Buildings")

# commercial buildings
summary(zone3.commercialbuilding)
zone3.commercialbuilding.ppp <-
  ppp(
    zone3.commercialbuilding$long,
    zone3.commercialbuilding$lat,
    c(7.6, 7.8),
    c(48.5, 48.61),
    marks = factor(zone3.commercialbuilding$class)
  )

K_cb <- envelope(zone3.commercialbuilding.ppp, Kest, correction = "Ripley", nsim=100)
plot(K_cb, main="Ripley's K-function of Zone 3 Commercial Buildings")

# collective houses
summary(zone3.collectivehouse)
zone3.collectivehouse.ppp <-
  ppp(
    zone3.collectivehouse$long,
    zone3.collectivehouse$lat,
    c(7.7, 7.8),
    c(48.5, 48.61),
    marks = factor(zone3.collectivehouse$class)
  )

K_lb <- envelope(zone3.collectivehouse.ppp, Kest, correction = "Ripley", nsim=100)
plot(K_lb, main="Ripley's K-function of Zone 3 Collective Houses")  

