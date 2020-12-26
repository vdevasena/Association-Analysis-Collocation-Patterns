library(spatstat)
library(sf)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ape)
library(caret)

zone3 <- read.delim("zone3.txt")

summary(zone3)
# zone3.ppp <-
#   ppp(
#     zone3$long,
#     zone3$lat,
#     c(7.7, 7.8),
#     c(48.6, 48.7),
#     unitname = c("degree", "degrees"),
#     marks = factor(zone3$class)
#   )
# # zone3.ppp
# plot(zone3.ppp)

# coords <- SpatialPoints(zone3[1:2], proj4string = CRS("+proj=longlat"))
# summary(coords)
#
# Z1 <- SpatialPointsDataFrame(coords, zone3)
#
# plot(Z1)
#
# summary(zone3.ppp)
#
# K <- Kest(zone3.ppp, correction = "Ripley")
# plot(K)
# E <- envelope(zone3.ppp, Kest, nsim = 39)
# plot(E)
#
# plot(alltypes(zone3.ppp, "K"))



################## Zone 3 ##########################
zone3.garage <-
  subset(zone3, class == "garage", select = c("long", "lat", "class"))
zone3.garage.norm <-
  as.data.frame(apply(zone3.garage[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone3.garage.norm$class = zone3.garage$class

zone3.singlehouse <-
  subset(zone3, class == "single_house", select = c("long", "lat", "class"))
zone3.singlehouse.norm <-
  as.data.frame(apply(zone3.singlehouse[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone3.singlehouse.norm$class = zone3.singlehouse$class

zone3.collectivehouse <-
  subset(zone3,
         class == "collective_house",
         select = c("long", "lat", "class"))
zone3.collectivehouse.norm <-
  as.data.frame(apply(zone3.collectivehouse[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone3.collectivehouse.norm$class = zone3.collectivehouse$class

zone3.commercialbuilding <-
  subset(zone3,
         class == "commercial_building",
         select = c("long", "lat", "class"))
zone3.commercialbuilding.norm <-
  as.data.frame(apply(zone3.commercialbuilding[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone3.commercialbuilding.norm$class = zone3.commercialbuilding$class

zone3.school <-
  subset(zone3, class == "school", select = c("long", "lat", "class"))
zone3.school.norm <-
  as.data.frame(apply(zone3.school[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone3.school.norm$class = zone3.school$class

zone3.lightbuilding <-
  subset(zone3,
         class == "light_building",
         select = c("long", "lat", "class"))
zone3.lightbuilding.norm <-
  as.data.frame(apply(zone3.lightbuilding[, 1:2], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
zone3.lightbuilding.norm$class = zone3.lightbuilding$class

# garages and single houses

zone3.gs <- rbind(zone3.garage, zone3.singlehouse)
zone3.gs.norm <- rbind(zone3.garage.norm, zone3.singlehouse.norm)

ggplot(zone3.gs, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

zone3gs.ppp <-
  ppp(zone3.gs.norm$long,
      zone3.gs.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone3.gs.norm$class))

# generate the random distribution
set.seed(1)
random.zone3gs.ppp <-
  rlabel(zone3gs.ppp, labels = factor(c("garage", "single_house")), permute =
           FALSE)

# df of the random ppp
random.zone3.gs <- as.data.frame(random.zone3gs.ppp)
summary(random.zone3gs.ppp)
summary(zone3gs.ppp)

k1 <- envelope(zone3gs.ppp, Kcross, nsim = 100, correction = "Ripley")

plot(k1,
     col = "blue",
     main = "Ripley's K-function of Zone 3 Garages and Single Houses")
# k1.random <- Kcross(random.zone3gs.ppp, correction = "Ripley")
# plot(k1.random,
#      legend = FALSE,
#      col = "red",
#      add = TRUE)

ggplot(random.zone3.gs, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()

# garages and collective houses
zone3.cg <- rbind(zone3.garage, zone3.collectivehouse)
zone3.cg.norm <-
  rbind(zone3.garage.norm, zone3.collectivehouse.norm)

ggplot(zone3.cg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone3.cg)
zone3cg.ppp <-
  ppp(zone3.cg.norm$long,
      zone3.cg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone3.cg.norm$class))

set.seed(2)
random.zone3cg.ppp <-
  rlabel(zone3cg.ppp, labels = factor(c("garage", "collective_house")), permute =
           FALSE)
random.zone3.cg <- as.data.frame(random.zone3cg.ppp)

summary(random.zone3cg.ppp)
summary(zone3cg.ppp)

#
k2.random <- envelope(random.zone3cg.ppp, Kcross, correction = "Ripley", nsim = 100)

k2 <- envelope(zone3cg.ppp, Kcross, correction = "Ripley", nsim = 100)

plot(k2,
     col = "blue",
     main = "Ripley's K-function of Zone 3 Garages and Collective Houses")
plot(k2.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 3 Garages and Collective Houses")


ggplot(random.zone3.cg, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()


# commercial buildings and light buildings
zone3.cl <- rbind(zone3.commercialbuilding, zone3.lightbuilding)
zone3.cl.norm <-
  rbind(zone3.commercialbuilding.norm, zone3.lightbuilding.norm)

ggplot(zone3.cl, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone3.cl)
zone3cl.ppp <-
  ppp(zone3.cl.norm$long,
      zone3.cl.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone3.cl.norm$class))

set.seed(3)
random.zone3cl.ppp <-
  rlabel(zone3cl.ppp, labels = factor(c("commercial_building", "light_building")), permute =
           FALSE)
random.zone3.cl <- as.data.frame(random.zone3cl.ppp)

summary(random.zone3cl.ppp)
summary(zone3cl.ppp)


k3.random <-
  envelope(random.zone3cl.ppp, Kcross,
           i = "commercial_building",
           j = "light_building",
           correction = "Ripley", nsim=100)
k3 <-
  envelope(
    zone3cl.ppp,
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
     main = "Ripley's K-function of the random distribution of Zone 3 Commercial Buildings and Light Buildings")

# plot the random distribution
ggplot(random.zone3.cl, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()

# commercial buildings and schools
zone3.cs <- rbind(zone3.commercialbuilding, zone3.school)
zone3.cs.norm <-
  rbind(zone3.commercialbuilding.norm, zone3.school.norm)

ggplot(zone3.cs, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone3.cs)
zone3cs.ppp <-
  ppp(zone3.cs.norm$long,
      zone3.cs.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone3.cs.norm$class))


set.seed(4)
random.zone3cs.ppp <-
  rlabel(zone3cs.ppp, labels = factor(c("commercial_building", "school")), permute =
           FALSE)
random.zone3.cs <- as.data.frame(random.zone3cs.ppp)

summary(random.zone3cl.ppp)
summary(zone3cl.ppp)


k4.random <- envelope(random.zone3cs.ppp,Kcross,nsim=100, correction = "Ripley")
k4 <-
  envelope(zone3cs.ppp, Kcross, nsim = 100, correction = "Ripley")
plot(k4,
     col = "blue",
     main = "Ripley's K-function of Zone 3 Commercial Buildings and Schools")
plot(k4.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 3 Commercial Buildings and Schools")

ggplot(random.zone3.cs, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()

# collective houses and commercial buildings
zone3.hg <- rbind(zone3.collectivehouse, zone3.commercialbuilding)
zone3.hg.norm <-
  rbind(zone3.collectivehouse.norm, zone3.commercialbuilding.norm)

ggplot(zone3.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone3.hg)
zone3hg.ppp <-
  ppp(zone3.hg.norm$long,
      zone3.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone3.hg.norm$class))


set.seed(123)
random.zone3hg.ppp <-
  rlabel(zone3hg.ppp, labels = factor(c("commercial_building", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone3hg.ppp, nsim = 100,
    correction = "Ripley"
  )
k5 <-
  envelope(
    zone3hg.ppp,
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
     main = "Ripley's K-function of the random distribtion of Zone 3 Commerical Buildings and Collective Houses")

ggplot(random.zone3.hg, aes(x, y, color = factor(marks))) +
  geom_point(size = 1) +
  theme_ipsum()

# commercial buildings and single houses
zone3.hg <- rbind(zone3.singlehouse, zone3.commercialbuilding)
zone3.hg.norm <-
  rbind(zone3.singlehouse.norm, zone3.commercialbuilding.norm)

ggplot(zone3.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone3.hg)
zone3hg.ppp <-
  ppp(zone3.hg.norm$long,
      zone3.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone3.hg.norm$class))

set.seed(1234)
random.zone3hg.ppp <-
  rlabel(zone3hg.ppp, labels = factor(c("commercial_building", "single_house")), permute =
           FALSE)

k5 <-
  envelope(
    zone3hg.ppp,
    Kcross,
    nsim = 100,
    i = "single_house",
    j = "commercial_building",
    correction = "Ripley"
  )


k5.random <-
  envelope(
    random.zone3hg.ppp, nsim = 100,
    correction = "Ripley"
  )
plot(k5,
     col = "blue",
     main = "Ripley's K-function of Commerical Buildings and Single Houses")
plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 3 Commercial Buildings and Single Houses")

# commercial buildings and garages
zone3.hg <- rbind(zone3.garage, zone3.commercialbuilding)
zone3.hg.norm <-
  rbind(zone3.garage.norm, zone3.commercialbuilding.norm)

ggplot(zone3.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone3.hg)
zone3hg.ppp <-
  ppp(zone3.hg.norm$long,
      zone3.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone3.hg.norm$class))
set.seed(13)
random.zone3hg.ppp <-
  rlabel(zone3hg.ppp, labels = factor(c("commercial_building", "garage")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone3hg.ppp, nsim = 100,
    correction = "Ripley",
    i = "garage",
    j = "commercial building"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 3 Garages and Commercial Buildings")



k5 <-
  envelope(
    zone3hg.ppp,
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
zone3.hg <- rbind(zone3.collectivehouse, zone3.commercialbuilding)
zone3.hg.norm <-
  rbind(zone3.collectivehouse.norm, zone3.commercialbuilding.norm)

ggplot(zone3.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone3.hg)
zone3hg.ppp <-
  ppp(zone3.hg.norm$long,
      zone3.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone3.hg.norm$class))

k5 <-
  envelope(
    zone3hg.ppp,
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
random.zone3hg.ppp <-
  rlabel(zone3hg.ppp, labels = factor(c("commercial_building", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone3hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 3 Commercial Buidlings and Collective Houses")



# light buildings and collective houses
zone3.hg <- rbind(zone3.collectivehouse, zone3.lightbuilding)
zone3.hg.norm <-
  rbind(zone3.collectivehouse.norm, zone3.lightbuilding.norm)

ggplot(zone3.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone3.hg)
zone3hg.ppp <-
  ppp(zone3.hg.norm$long,
      zone3.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone3.hg.norm$class))

k5 <-
  envelope(
    zone3hg.ppp,
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
random.zone3hg.ppp <-
  rlabel(zone3hg.ppp, labels = factor(c("light_building", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone3hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of the random distribution of Zone 3 Light Buildings and Collective Houses")




# schools and collective houses
zone3.hg <- rbind(zone3.collectivehouse, zone3.school)
zone3.hg.norm <-
  rbind(zone3.collectivehouse.norm, zone3.school.norm)

ggplot(zone3.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone3.hg)
zone3hg.ppp <-
  ppp(zone3.hg.norm$long,
      zone3.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone3.hg.norm$class))

k5 <-
  envelope(
    zone3hg.ppp,
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
random.zone3hg.ppp <-
  rlabel(zone3hg.ppp, labels = factor(c("school", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone3hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 3 Schools and Collective Houses")




# garages and collective houses
zone3.hg <- rbind(zone3.collectivehouse, zone3.garage)
zone3.hg.norm <-
  rbind(zone3.collectivehouse.norm, zone3.garage.norm)

ggplot(zone3.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone3.hg)
zone3hg.ppp <-
  ppp(zone3.hg.norm$long,
      zone3.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone3.hg.norm$class))

k5 <-
  envelope(
    zone3hg.ppp,
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
random.zone3hg.ppp <-
  rlabel(zone3hg.ppp, labels = factor(c("garage", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone3hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 3 Garages and Collective Houses")




# single houses and collective houses
zone3.hg <- rbind(zone3.collectivehouse, zone3.singlehouse)
zone3.hg.norm <-
  rbind(zone3.collectivehouse.norm, zone3.singlehouse.norm)

ggplot(zone3.hg, aes(x = long, y = lat, color = class)) +
  geom_point(size = 1) +
  theme_ipsum()

summary(zone3.hg)
zone3hg.ppp <-
  ppp(zone3.hg.norm$long,
      zone3.hg.norm$lat,
      c(0, 1),
      c(0, 1),
      marks = factor(zone3.hg.norm$class))

k5 <-
  envelope(
    zone3hg.ppp,
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
random.zone3hg.ppp <-
  rlabel(zone3hg.ppp, labels = factor(c("single_house", "collective_house")), permute =
           FALSE)

k5.random <-
  envelope(
    random.zone3hg.ppp, nsim = 100,
    correction = "Ripley"
  )

plot(k5.random,
     col = "red",
     main = "Ripley's K-function of the random distribution of Zone 3 Single Houses and Collective Houses")


