library(spatstat)
library(sf)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ape)
library(caret)

zone1 <- read.csv("zone1.txt")

summary(zone1)
# zone1.ppp <-
#   ppp(
#     zone1$long,
#     zone1$lat,
#     c(7.7, 7.8),
#     c(48.6, 48.7),
#     unitname = c("degree", "degrees"),
#     marks = factor(zone1$class)
#   )
# # zone1.ppp
# plot(zone1.ppp)

# coords <- SpatialPoints(zone1[1:2], proj4string = CRS("+proj=longlat"))
# summary(coords)
#
# Z1 <- SpatialPointsDataFrame(coords, zone1)
#
# plot(Z1)
#
# summary(zone1.ppp)
#
# K <- Kest(zone1.ppp, correction = "Ripley")
# plot(K)
# E <- envelope(zone1.ppp, Kest, nsim = 39)
# plot(E)
#
# plot(alltypes(zone1.ppp, "K"))



################## ZONE 1 ##########################
zone1.garage <-
  subset(zone1, class == "garage", select = c("long", "lat", "class"))
zone1.garage.norm <-
  as.data.frame(apply(zone1.garage[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone1.garage.norm$class = zone1.garage$class

zone1.singlehouse <-
  subset(zone1, class == "single_house", select = c("long", "lat", "class"))
zone1.singlehouse.norm <-
  as.data.frame(apply(zone1.singlehouse[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone1.singlehouse.norm$class = zone1.singlehouse$class

zone1.collectivehouse <-
  subset(zone1,
         class == "collective_house",
         select = c("long", "lat", "class"))
zone1.collectivehouse.norm <-
  as.data.frame(apply(zone1.collectivehouse[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone1.collectivehouse.norm$class = zone1.collectivehouse$class

zone1.commercialbuilding <-
  subset(zone1,
         class == "commercial_building",
         select = c("long", "lat", "class"))
zone1.commercialbuilding.norm <-
  as.data.frame(apply(zone1.commercialbuilding[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone1.commercialbuilding.norm$class = zone1.commercialbuilding$class

zone1.school <-
  subset(zone1, class == "school", select = c("long", "lat", "class"))
zone1.school.norm <-
  as.data.frame(apply(zone1.school[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone1.school.norm$class = zone1.school$class

zone1.lightbuilding <-
  subset(zone1,
         class == "light_building",
         select = c("long", "lat", "class"))
zone1.lightbuilding.norm <-
  as.data.frame(apply(zone1.lightbuilding[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone1.lightbuilding.norm$class = zone1.lightbuilding$class

# zone1.commercialbuildingsportive <-  subset(zone1,
#                                             class == "commercial_building_sportive",
#                                             select = c("long", "lat", "class"))
# zone1.commercialbuildingsportive.norm <- as.data.frame(apply(zone1.commercialbuildingsportive[, 1:2], 2, function(x)
#   (x - min(x)) / (max(x) - min(x))))
# zone1.commercialbuildingsportive.norm$class = zone1.commercialbuildingsportive$class

# garages and single houses

zone1.gs <- rbind(zone1.garage, zone1.singlehouse)
zone1.gs.norm <- rbind(zone1.garage.norm, zone1.singlehouse.norm)

ggplot(zone1.gs, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

zone1gs.ppp <-
  ppp(zone1.gs.norm$long,
      zone1.gs.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.gs.norm$class))

# generate the random distribution
set.seed(1)
random.zone1gs.ppp <-
  rlabel(zone1gs.ppp, labels = factor(c("garage", "single_house")), permute =
           FALSE)

# df of the random ppp
random.zone1.gs <- as.data.frame(random.zone1gs.ppp)
summary(random.zone1gs.ppp)
summary(zone1gs.ppp)

k1 <- envelope(zone1gs.ppp, Kcross, nsim = 100, correction = "Ripley")

plot(k1,
     col = "blue",
     main = "Ripley's K-function of Zone 1 Garages and Single Houses")
# k1.random <- Kcross(random.zone1gs.ppp, correction = "Ripley")
# plot(k1.random,
#      legend = FALSE,
#      col = "red",
#      add = TRUE)

ggplot(random.zone1.gs, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()

# garages and collective houses
zone1.cg <- rbind(zone1.garage, zone1.collectivehouse)
zone1.cg.norm <-
  rbind(zone1.garage.norm, zone1.collectivehouse.norm)

ggplot(zone1.cg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone1.cg)
zone1cg.ppp <-
  ppp(zone1.cg.norm$long,
      zone1.cg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.cg.norm$class))

set.seed(2)
random.zone1cg.ppp <-
  rlabel(zone1cg.ppp, labels = factor(c("garage", "collective_house")), permute =
           FALSE)
random.zone1.cg <- as.data.frame(random.zone1cg.ppp)

summary(random.zone1cg.ppp)
summary(zone1cg.ppp)

#
k2.random <- envelope(Kcross(random.zone1cg.ppp, correction = "Ripley", nsim = 100))

k2 <- envelope(zone1cg.ppp, Kcross, correction = "Ripley", nsim = 100)

plot(k2,
     col = "blue",
     main = "Ripley's K-function of Zone 1 Garages and Collective Houses")
plot(k2.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 1 Garages and Collective Houses")


ggplot(random.zone1.cg, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()


# commercial buildings and light buildings
zone1.cl <- rbind(zone1.commercialbuilding, zone1.lightbuilding)
zone1.cl.norm <-
  rbind(zone1.commercialbuilding.norm, zone1.lightbuilding.norm)

ggplot(zone1.cl, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone1.cl)
zone1cl.ppp <-
  ppp(zone1.cl.norm$long,
      zone1.cl.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.cl.norm$class))

set.seed(3)
random.zone1cl.ppp <-
  rlabel(zone1cl.ppp, labels = factor(c("commercial_building", "light_building")), permute =
           FALSE)
random.zone1.cl <- as.data.frame(random.zone1cl.ppp)

summary(random.zone1cl.ppp)
summary(zone1cl.ppp)


k3.random <-
  envelope(random.zone1cl.ppp, Kcross,
         i = "commercial_building",
         j = "light_building",
         correction = "Ripley", nsim=100)
k3 <-
  envelope(
    zone1cl.ppp,
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
     main = "Ripley's K-function of the random distribution of Zone 1 Commercial Buildings and Light Buildings")

# plot the random distribution
ggplot(random.zone1.cl, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()

# commercial buildings and schools
zone1.cs <- rbind(zone1.commercialbuilding, zone1.school)
zone1.cs.norm <-
  rbind(zone1.commercialbuilding.norm, zone1.school.norm)

ggplot(zone1.cs, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone1.cs)
zone1cs.ppp <-
  ppp(zone1.cs.norm$long,
      zone1.cs.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.cs.norm$class))


set.seed(4)
random.zone1cs.ppp <-
  rlabel(zone1cs.ppp, labels = factor(c("commercial_building", "school")), permute =
           FALSE)
random.zone1.cs <- as.data.frame(random.zone1cs.ppp)

summary(random.zone1cl.ppp)
summary(zone1cl.ppp)


k4.random <- envelope(random.zone1cs.ppp,Kcross,nsim=100, correction = "Ripley")
k4 <-
  envelope(zone1cs.ppp, Kcross, nsim = 100, correction = "Ripley")
plot(k4,
     col = "blue",
     main = "Ripley's K-function of Zone 1 Commercial Buildings and Schools")
plot(k4.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 1 Commercial Buildings and Schools")

ggplot(random.zone1.cs, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()

# collective houses and garages
zone1.hg <- rbind(zone1.collectivehouse, zone1.garage)
zone1.hg.norm <-
  rbind(zone1.collectivehouse.norm, zone1.garage.norm)

ggplot(zone1.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone1.hg)
zone1hg.ppp <-
  ppp(zone1.hg.norm$long,
      zone1.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.hg.norm$class))

set.seed(5)
random.zone1hg.ppp <-
  rlabel(zone1hg.ppp, labels = factor(c("collective_house", "garage")), permute =
           FALSE)
random.zone1.hg <- as.data.frame(random.zone1hg.ppp)

summary(random.zone1hg.ppp)
summary(zone1hg.ppp)


k5.random <-
  envelope(
    random.zone1hg.ppp, Kcross, nsim=100,
    i = "collective_house",
    j = "garage",
    correction = "Ripley",
  )
k5 <-
  envelope(
    zone1hg.ppp,
    Kcross,
    nsim = 100,
    i = "collective_house",
    j = "garage",
    correction = "Ripley"
  )
plot(k5,
     col = "blue",
     main = "Ripley's K-function of Garages and Collective Houses")
plot(k5.random,
     legend = FALSE,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 1 Garages and Collective Houses")
#
ggplot(random.zone1.hg, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()



# collective houses and commercial buildings
zone1.hg <- rbind(zone1.collectivehouse, zone1.commercialbuilding)
zone1.hg.norm <-
  rbind(zone1.collectivehouse.norm, zone1.commercialbuilding.norm)

ggplot(zone1.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone1.hg)
zone1hg.ppp <-
  ppp(zone1.hg.norm$long,
      zone1.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.hg.norm$class))


set.seed(123)
random.zone1hg.ppp <-
  rlabel(zone1hg.ppp, labels = factor(c("commercial_building", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone1hg.ppp, nsim = 100,
    i = "collective_house",
    j = "commercial_building",
    correction = "Ripley"
  )
k5 <-
  envelope(
    zone1hg.ppp,
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
     main = "Ripley's K-function of the random distribtion of Zone 1 Commerical Buildings and Collective Houses")

  ggplot(random.zone1.hg, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()

# commercial buildings and single houses
zone1.hg <- rbind(zone1.singlehouse, zone1.commercialbuilding)
zone1.hg.norm <-
  rbind(zone1.singlehouse.norm, zone1.commercialbuilding.norm)

ggplot(zone1.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone1.hg)
zone1hg.ppp <-
  ppp(zone1.hg.norm$long,
      zone1.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.hg.norm$class))

set.seed(1234)
random.zone1hg.ppp <-
  rlabel(zone1hg.ppp, labels = factor(c("commercial_building", "single_house")), permute =
           FALSE)

k5 <-
  envelope(
    zone1hg.ppp,
    Kcross,
    nsim = 100,
    i = "single_house",
    j = "commercial_building",
    correction = "Ripley"
  )


k5.random <-
  envelope(
    random.zone1hg.ppp, nsim = 100,
    correction = "Ripley"
  )
plot(k5,
     col = "blue",
     main = "Ripley's K-function of Commerical Buildings and Single Houses")
plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 1 Commercial Buildings and Single Houses")

# commercial buildings and garages
zone1.hg <- rbind(zone1.garage, zone1.commercialbuilding)
zone1.hg.norm <-
  rbind(zone1.garage.norm, zone1.commercialbuilding.norm)

ggplot(zone1.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone1.hg)
zone1hg.ppp <-
  ppp(zone1.hg.norm$long,
      zone1.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.hg.norm$class))
set.seed(13)
random.zone1hg.ppp <-
  rlabel(zone1hg.ppp, labels = factor(c("commercial_building", "garage")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone1hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 1 Garages and Commercial Buildings")



k5 <-
  envelope(
    zone1hg.ppp,
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
zone1.hg <- rbind(zone1.collectivehouse, zone1.commercialbuilding)
zone1.hg.norm <-
  rbind(zone1.collectivehouse.norm, zone1.commercialbuilding.norm)

ggplot(zone1.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone1.hg)
zone1hg.ppp <-
  ppp(zone1.hg.norm$long,
      zone1.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.hg.norm$class))

k5 <-
  envelope(
    zone1hg.ppp,
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
random.zone1hg.ppp <-
  rlabel(zone1hg.ppp, labels = factor(c("commercial_building", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone1hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 1 Garages and Collective Houses")



# light buildings and collective houses
zone1.hg <- rbind(zone1.collectivehouse, zone1.lightbuilding)
zone1.hg.norm <-
  rbind(zone1.collectivehouse.norm, zone1.lightbuilding.norm)

ggplot(zone1.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone1.hg)
zone1hg.ppp <-
  ppp(zone1.hg.norm$long,
      zone1.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.hg.norm$class))

k5 <-
  envelope(
    zone1hg.ppp,
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
random.zone1hg.ppp <-
  rlabel(zone1hg.ppp, labels = factor(c("light_building", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone1hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of the random distribution of Zone 1 Light Buildings and Collective Houses")




# schools and collective houses
zone1.hg <- rbind(zone1.collectivehouse, zone1.school)
zone1.hg.norm <-
  rbind(zone1.collectivehouse.norm, zone1.school.norm)

ggplot(zone1.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone1.hg)
zone1hg.ppp <-
  ppp(zone1.hg.norm$long,
      zone1.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.hg.norm$class))

k5 <-
  envelope(
    zone1hg.ppp,
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
random.zone1hg.ppp <-
  rlabel(zone1hg.ppp, labels = factor(c("school", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone1hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 1 Schools and Collective Houses")




# garages and collective houses
zone1.hg <- rbind(zone1.collectivehouse, zone1.garage)
zone1.hg.norm <-
  rbind(zone1.collectivehouse.norm, zone1.garage.norm)

ggplot(zone1.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone1.hg)
zone1hg.ppp <-
  ppp(zone1.hg.norm$long,
      zone1.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.hg.norm$class))

k5 <-
  envelope(
    zone1hg.ppp,
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
random.zone1hg.ppp <-
  rlabel(zone1hg.ppp, labels = factor(c("garage", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone1hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 1 Garages and Collective Houses")




# single houses and collective houses
zone1.hg <- rbind(zone1.collectivehouse, zone1.singlehouse)
zone1.hg.norm <-
  rbind(zone1.collectivehouse.norm, zone1.singlehouse.norm)

ggplot(zone1.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone1.hg)
zone1hg.ppp <-
  ppp(zone1.hg.norm$long,
      zone1.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone1.hg.norm$class))

k5 <-
  envelope(
    zone1hg.ppp,
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
random.zone1hg.ppp <-
  rlabel(zone1hg.ppp, labels = factor(c("single_house", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone1hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 1 Single Houses and Collective Houses")
