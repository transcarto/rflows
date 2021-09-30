library(sf)
library(mapsf)
library(packcircles)

countries <- st_read("data/world/geom/countries.gpkg")
subregions <- st_read("data/world/geom/subregions.gpkg")
graticule <- st_read("data/world/geom/graticule.gpkg")
bbox <- st_read("data/world/geom/bbox.gpkg")
migr <- read.csv("data/world/fij/migr2019_T.csv")

crs <-
  "+proj=aeqd +lat_0=90 +lon_0=50 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
countries <- st_transform(x = countries, crs = crs)
subregions <- st_transform(x = subregions, crs = crs)
graticule <- st_transform(x = graticule, crs = crs)
bbox <- st_transform(x = bbox, crs = crs)
land <- st_union(countries)




col = "#ffc524"
credit = paste0(
  "Françoise Bahoken & Nicolas Lambert, 2021\n",
  "Source: United Nations, Department of Economic\n",
  "and Social Affairs, Population Division (2019)"
)
# theme = mf_theme(x = "default", bg = "white", tab = FALSE,
#                    pos = "center", line = 2, inner = FALSE,
#                    fg = "#9F204270", mar = c(0,0, 2, 0),cex = 1.9)

theme <- mf_theme(
  x = "default",
  bg = "#3b3b3b",
  fg = "#ffc524",
  mar = c(0, 0, 2, 0),
  tab = TRUE,
  pos = "left",
  inner = FALSE,
  line = 2,
  cex = 1.9,
  font = 3
)

template = function(title, file) {
  mf_export(
    countries,
    export = "png",
    width = 1000,
    filename = file,
    res = 96,
    theme = theme,
    expandBB = c(-.02, 0,-.02, 0)
  )
  mf_map(
    bbox,
    col = "#3b3b3b",
    border = NA,
    lwd = 0.5,
    add = TRUE
  )
  mf_map(graticule,
         col = "#FFFFFF50",
         lwd = 0.5,
         add = TRUE)
  mf_map(
    countries,
    col = "#4e4f4f",
    border = "#3b3b3b",
    lwd = 0.5,
    add = TRUE
  )
  # mf_map(links, col = NA,border = "#317691", lwd = 0.5, add = TRUE)
  mf_credits(
    txt = credit,
    pos = "bottomright",
    col = "#1a2640",
    cex = 0.7,
    font = 3,
    bg = "#ffffff30"
  )
  mf_title(title)
}

##########################################################################################################################################

ISO3 <- "FRA"
label = "France"
migrFRA <- migr[migr$j == ISO3,]
migrFRA$fij <- as.numeric(migrFRA$fij)
migrFRA <-
  rbind.data.frame(migrFRA, c(
    i = ISO3,
    j = ISO3,
    fij = sum(as.numeric(migrFRA$fij))
  ))
countr <- countries[, "adm0_a3_is"]
countr <-
  merge(
    x = countr,
    y = migrFRA,
    by.x = "adm0_a3_is",
    by.y = "i",
    all.x = TRUE
  )
colnames(countr) <- c("i", "j", "fij", "geometry")

# Circles

dots = countr
st_geometry(dots) <-
  st_centroid(sf::st_geometry(dots), of_largest_polygon = TRUE)
dots <- data.frame(dots$i, dots["fij"], st_coordinates(dots))
dots = dots[, c("dots.i", "X", "Y", "fij")]
colnames(dots) <- c("id", "x", "y", "v")
dots <- dots[!is.na(dots$v), ]

k = 700000 # pour ajuster la taille des cercles
itermax = 10 # nombre d'iterations
delta = 35000
dat.init <- dots[, c("x", "y", "v", "id")]
dat.init$v <- sqrt(as.numeric(dat.init$v) * k)
simulation <- circleRepelLayout(
  x = dat.init,
  xysizecols = 1:3,
  wrap = FALSE,
  sizetype = "radius",
  maxiter = itermax,
  weights = 1
)$layout
circles <- st_buffer(sf::st_as_sf(
  simulation,
  coords = c('x', 'y'),
  crs = sf::st_crs(countries)
),
dist = simulation$radius - delta)

circles$v = dots$v
circles$id = dots$id

# Links

dots$j = "FRA"

links <-
  mf_get_links(
    x = circles,
    df = migrFRA,
    x_id = "id",
    df_id = c("i", "j")
  )
links$fij = as.numeric(links$fij)

# Carte

template("test", "maps/test.png")

col2 = "#4e4f4f"

mf_map(
  land,
  col = "#4e4f4f",
  border = "#3b3b3b",
  lwd = 0.5,
  add = TRUE
)


mf_map(
  links,
  var = "fij",
  col = col,
  border = "#3b3b3b",
  type = "prop",
  lwd_max = 160,
  leg_pos = "n",
  add = TRUE
)

mf_map(
  circles[circles$id != ISO3, ],
  var = "fij",
  col = col,
  border = "#3b3b3b",
  lwd = 1.5,
  add = TRUE
)

mf_map(
  circles[circles$id == ISO3, ],
  var = "fij",
  col = col2,
  border = col,
  lwd = 2.5,
  add = TRUE
)

t =  circles[circles$id != ISO3, ]
mf_label(
  t,
  var = "id",
  halo = FALSE,
  cex = sqrt(as.numeric(t$v) / 1200000),
  col = col2,
  overlap = TRUE,
  lines = FALSE
)

t =  circles[circles$id == ISO3, ]
mf_label(
  t,
  var = "id",
  halo = FALSE,
  cex = sqrt(as.numeric(t$v) / 1200000),
  col = col,
  overlap = TRUE,
  lines = FALSE
)

dev.off()
