library(ttt)
library(sf)
library(mapsf)

countries <- st_read("data/world/geom/countries.gpkg")
subregions <- st_read("data/world/geom/subregions.gpkg")
graticule <- st_read("data/world/geom/graticule.gpkg")
bbox <- st_read("data/world/geom/bbox.gpkg")
migr <- read.csv("data/world/subregions/migrantstocks2019.csv")
crs <- "+proj=aeqd +lat_0=90 +lon_0=50 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
countries <- st_transform(x = countries, crs = crs)
subregions <- st_transform(x = subregions, crs = crs)
graticule <- st_transform(x = graticule, crs = crs)
bbox <- st_transform(x = bbox, crs = crs)
land <- st_union(countries)





flows <- ttt_flowmapper(
  x = subregions,
  xid = "id",
  df = migr,
  dfid = c("i","j"),
  dfvar = "fij",
  plot = FALSE)


flows$links

#dev.off()
