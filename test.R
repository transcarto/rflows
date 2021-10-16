library('readxl')
library(sf)

countries <- st_read("https://raw.githubusercontent.com/transcarto/rflows/master/data/world/geom/countries.geojson")
graticule <- st_read("https://raw.githubusercontent.com/transcarto/rflows/master/data/world/geom/graticule.geojson")
bbox <- st_read("https://raw.githubusercontent.com/transcarto/rflows/master/data/world/geom/bbox.geojson")

crs <-
  "+proj=aeqd +lat_0=90 +lon_0=50 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
countries <- st_transform(x = countries, crs = crs)
graticule <- st_transform(x = graticule, crs = crs)
bbox <- st_transform(x = bbox, crs = crs)
land <- st_union(countries)

ctr <- countries[,2:4] %>% st_drop_geometry()
ctr <- ctr[order(ctr[,"label"], decreasing =FALSE),]
codes <- ctr$adm0_a3_is


data_url <-
  "https://raw.githubusercontent.com/transcarto/rflows/master/data/world/UN_MigrantStockByOriginAndDestination_2019.xlsx"
file <- "data/UN_MigrantStockByOriginAndDestination_2019.xlsx"
if (!file.exists(file)) {
  download.file(url = data_url, destfile = file)
} 

sheet <- "Table 1"
year <- 2019

migr <- data.frame(read_excel(file, skip = 15, sheet = sheet))
migr <- migr[migr[, 1] == year, ]

migr <- migr[!is.na(migr[, 6]), ]
migr <-
  subset(migr,
         select = -c(...1, ...2, ...5, ...4, ...6, Total, Other.North, Other.South))
colnames(migr)[1] <- "i"
migr <- migr[order(migr[, "i"], decreasing = FALSE), ]
for (i in 2:length(colnames(migr))) {
  migr[, i] <- as.numeric(migr[, i])
}

rownames(migr) <- codes
colnames(migr) <- c("i",codes)
migr <- migr[,-1]


View(migr)

# --------------------------------

library("cartograflow")
