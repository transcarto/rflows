library(sf)

countries <- st_read("data/world/geom/countries.gpkg")
colnames(countries)
countr <- countries[, c("adm0_a3_is", "label", "Code2","Label2")]
data = read.csv("data/world/pop.csv")

countr <-
  merge(
    x = countr,
    y = data,
    by.x = "adm0_a3_is",
    by.y = "id",
    all.x = TRUE
  )

countr[countr$adm0_a3_is == "ESH","pop2019"] <- 405210
countr[countr$adm0_a3_is == "AIA","pop2019"] <- 15094
countr[countr$adm0_a3_is == "GLP","pop2019"] <- 395700
countr[countr$adm0_a3_is == "WLF","pop2019"] <- 15289
countr[countr$adm0_a3_is == "TKL","pop2019"] <- 1411
countr[countr$adm0_a3_is == "SPM","pop2019"] <- 5888
countr[countr$adm0_a3_is == "SHN","pop2019"] <- 4255
countr[countr$adm0_a3_is == "REU","pop2019"] <- 859959
countr[countr$adm0_a3_is == "BES","pop2019"] <- 25987
countr[countr$adm0_a3_is == "COK","pop2019"] <- 17459
countr[countr$adm0_a3_is == "FLK","pop2019"] <- 2840
countr[countr$adm0_a3_is == "GUF","pop2019"] <- 294071
countr[countr$adm0_a3_is == "JEY","pop2019"] <- 164623
countr[countr$adm0_a3_is == "MSR","pop2019"] <- 4992 
countr[countr$adm0_a3_is == "MTQ","pop2019"] <- 376480 
countr[countr$adm0_a3_is == "MYT","pop2019"] <- 270372 
countr[countr$adm0_a3_is == "NIU","pop2019"] <- 1620 
countr[countr$adm0_a3_is == "VAT","pop2019"] <- 825 
View(countr)
colnames(countr) <- c("id","name","id_reg","name_reg","xxx","pop2019","geometry")
countr <- countr[, c("id","name","id_reg","name_reg","pop2019","geometry")]

# crs <-
#   "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

crs <-
  "+proj=aeqd +lat_0=90 +lon_0=50 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "

countr <- st_transform(x = countr, crs = crs)

st_write(countr,"world2.shp")


# --------------------------------------

countries <- st_read("data/world/cartogram/WorldPopulation.shp")
grid <- st_read("data/world/cartogram/grid.shp")
View(countries)

subregions <-
  aggregate(countries, by = list(countries$id_reg), FUN = head, 1)
subregions <- subregions[, c("id_reg", "name_reg")]
st_geometry(subregions) <-
  st_cast(subregions$geometry, "MULTIPOLYGON")
colnames(subregions) <- c("id", "label", "geometry")
```

plot(st_geometry(grid))
plot(st_geometry(countries), add= TRUE)
