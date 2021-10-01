library(sf)
library(ttt)


subregions <- st_read(system.file("subregions.gpkg", package="flowmapper")) %>% st_transform(crs)
migr <- read.csv(system.file("migrantstocks2019.csv", package="flowmapper"))


threshold <- 1500
migr <- migr[migr$fij >= threshold, ]

intra <- migr[migr$i == migr$j, ]
intra <- intra[, c("i", "fij")]
colnames(intra) <- c("id", "nb")

flows <- ttt_flowmapper(
  x = subregions,
  xid = "id",
  df = migr,
  dfid = c("i", "j"),
  dfvar = "fij",
  size = "thickness",
  type = "arrows",
  decreasing = FALSE,
  add = TRUE,
  lwd = 1,
  col = col,
  border = "#424242",
  k = NULL,
  k2 = 60,
  df2 = intra,
  df2id = "id",
  df2var = "nb",
  col2 = "#3b3b3b",
  border2 = col
)