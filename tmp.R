library(sf)
library(cartogram)
library(mapsf)

countries <- st_read("data/world/geom/countries.gpkg")
graticule <- st_read("data/world/geom/graticule.gpkg")
bbox <- st_read("data/world/geom/bbox.gpkg")
migr <- read.csv("data/world/fij/migr2019_T.csv")

crs <- "+proj=aeqd +lat_0=90 +lon_0=50 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
countries <- st_transform(x = countries, crs = crs)
graticule <- st_transform(x = graticule, crs = crs)
bbox <- st_transform(x = bbox, crs = crs)
land <- st_union(countries)

# -------------------------------------

ISO3 = "FRA"

mf_map(bbox, col = "#d5ebf2",border = NA, lwd = 0.5)
mf_map(graticule, col = "white", lwd = 0.5, add = TRUE)
mf_map(countries, col = "#baaba2",border = "white", lwd = 0.5, add = TRUE)
mf_map(land, col = NA,border = "#317691", lwd = 0.5, add = TRUE)

# Selection des flux vers ISO3
countr <- countries[,c("adm0_a3_is","label")]
migr <- migr[migr$j == ISO3,]
tot <- sum(migr$fij)
migr <- rbind.data.frame(migr, c(i = ISO3,j = ISO3,fij = tot))
migr$fij <- as.numeric(migr$fij)
countr <- merge(x = countr,y = migr, by.x = "adm0_a3_is", by.y = "i", all.x = TRUE)
countr <- countr[-3]
colnames(countr) <- c("id","label","fij","geometry")

# -----------------------------------------

library("packcircles")

k = 500000 # pour ajuster la taille des cercles
itermax = 10 # nombre d'iterations



dat.init = data.frame(st_coordinates(st_centroid(st_geometry(countr),
                                                 of_largest_polygon = TRUE)),
                      countr[,"fij"] %>% st_drop_geometry())
colnames(dat.init) = c("x","y","v")
dat.init <- dat.init[!is.na(dat.init$v),]
dat.init$v <- sqrt(dat.init$v * k)
simulation <- circleRepelLayout(x = dat.init, xysizecols = 1:3,
                                wrap = FALSE, sizetype = "radius",
                                maxiter = itermax, weights =1)$layout

circles <- st_buffer(sf::st_as_sf(simulation, coords =c('x', 'y'),
                                  crs = sf::st_crs(countr)), dist = simulation$radius)

mf_map(bbox, col = "#d5ebf2",border = NA, lwd = 0.5)
mf_map(graticule, col = "white", lwd = 0.5, add = TRUE)
mf_map(countries, col = "#baaba2",border = "white", lwd = 0.5, add = TRUE)
mf_map(land, col = NA,border = "#317691", lwd = 0.5, add = TRUE)
mf_map(circles, col = "#c75b9a",border = "#317691", lwd = 0.5, add = TRUE)
