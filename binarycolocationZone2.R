library(spatstat)
library(sf)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ape)
library(caret)

zone2 <- read.csv("zone2.txt")

summary(zone2)
# zone2.ppp <-
#   ppp(
#     zone2$long,
#     zone2$lat,
#     c(7.7, 7.8),
#     c(48.6, 48.7),
#     unitname = c("degree", "degrees"),
#     marks = factor(zone2$class)
#   )
# # zone2.ppp
# plot(zone2.ppp)

# coords <- SpatialPoints(zone2[1:2], proj4string = CRS("+proj=longlat"))
# summary(coords)
#
# Z1 <- SpatialPointsDataFrame(coords, zone2)
#
# plot(Z1)
#
# summary(zone2.ppp)
#
# K <- Kest(zone2.ppp, correction = "Ripley")
# plot(K)
# E <- envelope(zone2.ppp, Kest, nsim = 39)
# plot(E)
#
# plot(alltypes(zone2.ppp, "K"))



################## Zone 2 ##########################
zone2.garage <-
  subset(zone2, class == "garage", select = c("long", "lat", "class"))
zone2.garage.norm <-
  as.data.frame(apply(zone2.garage[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone2.garage.norm$class = zone2.garage$class

zone2.singlehouse <-
  subset(zone2, class == "single_house", select = c("long", "lat", "class"))
zone2.singlehouse.norm <-
  as.data.frame(apply(zone2.singlehouse[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone2.singlehouse.norm$class = zone2.singlehouse$class

zone2.collectivehouse <-
  subset(zone2,
         class == "collective_house",
         select = c("long", "lat", "class"))
zone2.collectivehouse.norm <-
  as.data.frame(apply(zone2.collectivehouse[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone2.collectivehouse.norm$class = zone2.collectivehouse$class

zone2.commercialbuilding <-
  subset(zone2,
         class == "commercial_building",
         select = c("long", "lat", "class"))
zone2.commercialbuilding.norm <-
  as.data.frame(apply(zone2.commercialbuilding[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone2.commercialbuilding.norm$class = zone2.commercialbuilding$class

zone2.school <-
  subset(zone2, class == "school", select = c("long", "lat", "class"))
zone2.school.norm <-
  as.data.frame(apply(zone2.school[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone2.school.norm$class = zone2.school$class

zone2.lightbuilding <-
  subset(zone2,
         class == "light_building",
         select = c("long", "lat", "class"))
zone2.lightbuilding.norm <-
  as.data.frame(apply(zone2.lightbuilding[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone2.lightbuilding.norm$class = zone2.lightbuilding$class

# garages and single houses

zone2.gs <- rbind(zone2.garage, zone2.singlehouse)
zone2.gs.norm <- rbind(zone2.garage.norm, zone2.singlehouse.norm)

ggplot(zone2.gs, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

zone2gs.ppp <-
  ppp(zone2.gs.norm$long,
      zone2.gs.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.gs.norm$class))

# generate the random distribution
set.seed(1)
random.zone2gs.ppp <-
  rlabel(zone2gs.ppp, labels = factor(c("garage", "single_house")), permute =
           FALSE)

# df of the random ppp
random.zone2.gs <- as.data.frame(random.zone2gs.ppp)
summary(random.zone2gs.ppp)
summary(zone2gs.ppp)

k1 <- envelope(zone2gs.ppp, Kcross, nsim = 100, correction = "Ripley")

plot(k1,
     col = "blue",
     main = "Ripley's K-function of Zone 2 Garages and Single Houses")
# k1.random <- Kcross(random.zone2gs.ppp, correction = "Ripley")
# plot(k1.random,
#      legend = FALSE,
#      col = "red",
#      add = TRUE)

ggplot(random.zone2.gs, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()

# garages and collective houses
zone2.cg <- rbind(zone2.garage, zone2.collectivehouse)
zone2.cg.norm <-
  rbind(zone2.garage.norm, zone2.collectivehouse.norm)

ggplot(zone2.cg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone2.cg)
zone2cg.ppp <-
  ppp(zone2.cg.norm$long,
      zone2.cg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.cg.norm$class))

set.seed(2)
random.zone2cg.ppp <-
  rlabel(zone2cg.ppp, labels = factor(c("garage", "collective_house")), permute =
           FALSE)
random.zone2.cg <- as.data.frame(random.zone2cg.ppp)

summary(random.zone2cg.ppp)
summary(zone2cg.ppp)

#
k2.random <- envelope(random.zone2cg.ppp, Kcross, correction = "Ripley", nsim = 100)

k2 <- envelope(zone2cg.ppp, Kcross, correction = "Ripley", nsim = 100)

plot(k2,
     col = "blue",
     main = "Ripley's K-function of Zone 2 Garages and Collective Houses")
plot(k2.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 2 Garages and Collective Houses")


ggplot(random.zone2.cg, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()


# commercial buildings and light buildings
zone2.cl <- rbind(zone2.commercialbuilding, zone2.lightbuilding)
zone2.cl.norm <-
  rbind(zone2.commercialbuilding.norm, zone2.lightbuilding.norm)

ggplot(zone2.cl, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone2.cl)
zone2cl.ppp <-
  ppp(zone2.cl.norm$long,
      zone2.cl.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.cl.norm$class))

set.seed(3)
random.zone2cl.ppp <-
  rlabel(zone2cl.ppp, labels = factor(c("commercial_building", "light_building")), permute =
           FALSE)
random.zone2.cl <- as.data.frame(random.zone2cl.ppp)

summary(random.zone2cl.ppp)
summary(zone2cl.ppp)


k3.random <-
  envelope(random.zone2cl.ppp, Kcross,
           i = "commercial_building",
           j = "light_building",
           correction = "Ripley", nsim=100)
k3 <-
  envelope(
    zone2cl.ppp,
    i = "commercial_building",
    j = "light_building",
    Kcross,
    nsim = 100,
    correction = "Ripley"
  )

plot(k3,
     col = "blue",
     main = "Ripley's K-function of Commercial Buildings and Light Buildings")
plot(k3.random,
     col =  "red",
     main = "Ripley's K-function of the random distribution of Zone 2 Commercial Buildings and Light Buildings")

# plot the random distribution
ggplot(random.zone2.cl, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()

# commercial buildings and schools
zone2.cs <- rbind(zone2.commercialbuilding, zone2.school)
zone2.cs.norm <-
  rbind(zone2.commercialbuilding.norm, zone2.school.norm)

ggplot(zone2.cs, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone2.cs)
zone2cs.ppp <-
  ppp(zone2.cs.norm$long,
      zone2.cs.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.cs.norm$class))


set.seed(4)
random.zone2cs.ppp <-
  rlabel(zone2cs.ppp, labels = factor(c("commercial_building", "school")), permute =
           FALSE)
random.zone2.cs <- as.data.frame(random.zone2cs.ppp)

summary(random.zone2cl.ppp)
summary(zone2cl.ppp)


k4.random <- envelope(random.zone2cs.ppp,Kcross,nsim=100, correction = "Ripley")
k4 <-
  envelope(zone2cs.ppp, Kcross, nsim = 100, correction = "Ripley")
plot(k4,
     col = "blue",
     main = "Ripley's K-function of Zone 2 Commercial Buildings and Schools")
plot(k4.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 2 Commercial Buildings and Schools")

ggplot(random.zone2.cs, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()

# collective houses and garages
zone2.hg <- rbind(zone2.collectivehouse, zone2.garage)
zone2.hg.norm <-
  rbind(zone2.collectivehouse.norm, zone2.garage.norm)

ggplot(zone2.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone2.hg)
zone2hg.ppp <-
  ppp(zone2.hg.norm$long,
      zone2.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.hg.norm$class))

set.seed(5)
random.zone2hg.ppp <-
  rlabel(zone2hg.ppp, labels = factor(c("collective_house", "garage")), permute =
           FALSE)
random.zone2.hg <- as.data.frame(random.zone2hg.ppp)

summary(random.zone2hg.ppp)
summary(zone2hg.ppp)


k5.random <-
  envelope(
    random.zone2hg.ppp, Kcross, nsim=100,
    correction = "Ripley",
  )
k5 <-
  envelope(
    zone2hg.ppp,
    Kcross,
    nsim = 100,
    correction = "Ripley"
  )
plot(k5,
     col = "blue",
     main = "Ripley's K-function of Garages and Collective Houses")
plot(k5.random,
     legend = FALSE,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 2 Garages and Collective Houses")
#
ggplot(random.zone2.hg, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()



# collective houses and commercial buildings
zone2.hg <- rbind(zone2.collectivehouse, zone2.commercialbuilding)
zone2.hg.norm <-
  rbind(zone2.collectivehouse.norm, zone2.commercialbuilding.norm)

ggplot(zone2.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone2.hg)
zone2hg.ppp <-
  ppp(zone2.hg.norm$long,
      zone2.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.hg.norm$class))


set.seed(123)
random.zone2hg.ppp <-
  rlabel(zone2hg.ppp, labels = factor(c("commercial_building", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone2hg.ppp, nsim = 100,
    correction = "Ripley"
  )
k5 <-
  envelope(
    zone2hg.ppp,
    Kcross,
    nsim = 100,
    i = "collective_house",
    j = "commercial_building",
    correction = "Ripley"
  )
plot(k5,
     col = "blue",
     main = "Ripley's K-function of Commerical Buildings and Collective Houses")
plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribtion of Zone 2 Commerical Buildings and Collective Houses")

ggplot(random.zone2.hg, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()

# commercial buildings and single houses
zone2.hg <- rbind(zone2.singlehouse, zone2.commercialbuilding)
zone2.hg.norm <-
  rbind(zone2.singlehouse.norm, zone2.commercialbuilding.norm)

ggplot(zone2.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone2.hg)
zone2hg.ppp <-
  ppp(zone2.hg.norm$long,
      zone2.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.hg.norm$class))

set.seed(1234)
random.zone2hg.ppp <-
  rlabel(zone2hg.ppp, labels = factor(c("commercial_building", "single_house")), permute =
           FALSE)

k5 <-
  envelope(
    zone2hg.ppp,
    Kcross,
    nsim = 100,
    i = "single_house",
    j = "commercial_building",
    correction = "Ripley"
  )


k5.random <-
  envelope(
    random.zone2hg.ppp, nsim = 100,
    correction = "Ripley"
  )
plot(k5,
     col = "blue",
     main = "Ripley's K-function of Commerical Buildings and Single Houses")
plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 2 Commercial Buildings and Single Houses")

# commercial buildings and garages
zone2.hg <- rbind(zone2.garage, zone2.commercialbuilding)
zone2.hg.norm <-
  rbind(zone2.garage.norm, zone2.commercialbuilding.norm)

ggplot(zone2.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone2.hg)
zone2hg.ppp <-
  ppp(zone2.hg.norm$long,
      zone2.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.hg.norm$class))
set.seed(13)
random.zone2hg.ppp <-
  rlabel(zone2hg.ppp, labels = factor(c("commercial_building", "garage")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone2hg.ppp, nsim = 100,
    correction = "Ripley",
    i = "garage",
    j = "commercial building"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 2 Garages and Commercial Buildings")



k5 <-
  envelope(
    zone2hg.ppp,
    Kcross,
    nsim = 100,
    i = "garage",
    j = "commercial_building",
    correction = "Ripley"
  )
plot(k5,
     col = "blue",
     main = "Ripley's K-function of Commerical Buildings and Garages")


# commercial buildings and collective houses
zone2.hg <- rbind(zone2.collectivehouse, zone2.commercialbuilding)
zone2.hg.norm <-
  rbind(zone2.collectivehouse.norm, zone2.commercialbuilding.norm)

ggplot(zone2.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone2.hg)
zone2hg.ppp <-
  ppp(zone2.hg.norm$long,
      zone2.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.hg.norm$class))

k5 <-
  envelope(
    zone2hg.ppp,
    Kcross,
    nsim = 100,
    i = "collective_house",
    j = "commercial_building",
    correction = "Ripley"
  )
plot(k5,
     col = "blue",
     main = "Ripley's K-function of Commerical Buildings and Collective Houses")


set.seed(123)
random.zone2hg.ppp <-
  rlabel(zone2hg.ppp, labels = factor(c("commercial_building", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone2hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 2 Commercial Buidlings and Collective Houses")



# light buildings and collective houses
zone2.hg <- rbind(zone2.collectivehouse, zone2.lightbuilding)
zone2.hg.norm <-
  rbind(zone2.collectivehouse.norm, zone2.lightbuilding.norm)

ggplot(zone2.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone2.hg)
zone2hg.ppp <-
  ppp(zone2.hg.norm$long,
      zone2.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.hg.norm$class))

k5 <-
  envelope(
    zone2hg.ppp,
    Kcross,
    nsim = 100,
    i = "collective_house",
    j = "light_building",
    correction = "Ripley"
  )
plot(k5,
     col = "blue",
     main = "Ripley's K-function of Light Buildings and Collective Houses")

set.seed(1234)
random.zone2hg.ppp <-
  rlabel(zone2hg.ppp, labels = factor(c("light_building", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone2hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of the random distribution of Zone 2 Light Buildings and Collective Houses")




# schools and collective houses
zone2.hg <- rbind(zone2.collectivehouse, zone2.school)
zone2.hg.norm <-
  rbind(zone2.collectivehouse.norm, zone2.school.norm)

ggplot(zone2.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone2.hg)
zone2hg.ppp <-
  ppp(zone2.hg.norm$long,
      zone2.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.hg.norm$class))

k5 <-
  envelope(
    zone2hg.ppp,
    Kcross,
    nsim = 100,
    i = "collective_house",
    j = "school",
    correction = "Ripley"
  )
plot(k5,
     col = "blue",
     main = "Ripley's K-function of Schools and Collective Houses")

set.seed(123456)
random.zone2hg.ppp <-
  rlabel(zone2hg.ppp, labels = factor(c("school", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone2hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 2 Schools and Collective Houses")




# garages and collective houses
zone2.hg <- rbind(zone2.collectivehouse, zone2.garage)
zone2.hg.norm <-
  rbind(zone2.collectivehouse.norm, zone2.garage.norm)

ggplot(zone2.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone2.hg)
zone2hg.ppp <-
  ppp(zone2.hg.norm$long,
      zone2.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.hg.norm$class))

k5 <-
  envelope(
    zone2hg.ppp,
    Kcross,
    nsim = 100,
    i = "collective_house",
    j = "garage",
    correction = "Ripley"
  )
plot(k5,
     col = "blue",
     main = "Ripley's K-function of Garages and Collective Houses")

set.seed(66)
random.zone2hg.ppp <-
  rlabel(zone2hg.ppp, labels = factor(c("garage", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone2hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 2 Garages and Collective Houses")




# single houses and collective houses
zone2.hg <- rbind(zone2.collectivehouse, zone2.singlehouse)
zone2.hg.norm <-
  rbind(zone2.collectivehouse.norm, zone2.singlehouse.norm)

ggplot(zone2.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone2.hg)
zone2hg.ppp <-
  ppp(zone2.hg.norm$long,
      zone2.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone2.hg.norm$class))

k5 <-
  envelope(
    zone2hg.ppp,
    Kcross,
    nsim = 100,
    i = "collective_house",
    j = "single_house",
    correction = "Ripley"
  )
plot(k5,
     col = "blue",
     main = "Ripley's K-function of Single Houses and Collective Houses")

set.seed(1123)
random.zone2hg.ppp <-
  rlabel(zone2hg.ppp, labels = factor(c("single_house", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone2hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 2 Single Houses and Collective Houses")


