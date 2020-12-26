library(spatstat)
library(sf)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ape)
library(caret)

zone2 <- read.csv("zone2.txt")

unique(zone2$class)

################## ZONE 2 ##########################

zone2.church <- subset(zone2, class == "church", select = c("long", "lat", "class"))
zone2.sportbuilding <-  subset(zone2, class == "sport_building", select = c("long", "lat", "class"))

zone2.garage <-
  subset(zone2, class == "garage", select = c("long", "lat", "class"))

zone2.singlehouse <-
  subset(zone2, class == "single_house", select = c("long", "lat", "class"))

zone2.collectivehouse <-
  subset(zone2,
         class == "collective_house",
         select = c("long", "lat", "class"))

zone2.commercialbuilding <-
  subset(zone2,
         class == "commercial_building",
         select = c("long", "lat", "class"))

zone2.lightbuilding <-
  subset(zone2,
         class == "light_building",
         select = c("long", "lat", "class"))

ggplot(zone2, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum() + ggtitle("Zone 2 Buildings")

# commercial buildings
summary(zone2.commercialbuilding)
zone2.commercialbuilding.ppp <-
  ppp(
    zone2.commercialbuilding$long,
    zone2.commercialbuilding$lat,
    c(7.6, 7.7),
    c(48.5, 48.6),
    marks = factor(zone2.commercialbuilding$class)
  )

K_cb <- envelope(zone2.commercialbuilding.ppp, Kest, correction = "Ripley", nsim=100)
plot(K_cb, main="Ripley's K-function of Zone 2 Commercial Buildings")



# collective houses
summary(zone2.collectivehouse)
zone2.collectivehouse.ppp <-
  ppp(
    zone2.collectivehouse$long,
    zone2.collectivehouse$lat,
    c(7.6, 7.7),
    c(48.5, 48.6),
    marks = factor(zone2.collectivehouse$class)
  )

K_lb <- envelope(zone2.collectivehouse.ppp, Kest, correction = "Ripley", nsim=100)
plot(K_lb, main="Ripley's K-function of Zone 2 Collective Houses")  
