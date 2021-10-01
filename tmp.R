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
    expandBB = c(-.02, 0, -.02, 0)
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

#################################################################################

# Fabricarion des données au niveau subrégional à partir d'une clé d'aggrégations

head(countries[,c("adm0_a3_is", "label","Code2","Label2")])

# Géométries

subregions <- aggregate(countries, by=list(countries$Code2), FUN = head, 1)
subregions <- subregions[,c("Code2","Label2")]
st_geometry(subregions) <- st_cast(subregions$geometry, "MULTIPOLYGON")
colnames(subregions) <- c("id","label","geometry")

# Données attributaires

keys <- data.frame(countries[,c("adm0_a3_is","Code2")])
keys$geom <- NULL
migr <- merge(x = migr, y = keys, by.x = "i", by.y = "adm0_a3_is")
colnames(migr)[4] <- "subreg_i"
migr <- merge(x = migr, y = keys, by.x = "j", by.y = "adm0_a3_is")
colnames(migr)[5] <- "subreg_j"
migr$id <- paste0(migr$subreg_i,"_",migr$subreg_j)
flows <- aggregate(migr$fij, by=list(migr$id), FUN = sum)
flows$i <- sapply(strsplit(flows$Group.1, "_"), "[", 1)
flows$j <- sapply(strsplit(flows$Group.1, "_"), "[", 2)
flows <- flows[,c("i","j","x")]
colnames(flows)[3] <- "fij"
flows$fij <- round(flows$fij/1000,0)

migr2 = flows

# IntraRegional flows

flowsintra <- flows[flows$i == flows$j,c("i","fij")]
colnames(flowsintra) <- c("id","intra")
subregions <- merge(x = subregions, y = flowsintra, by = "id")


# #################################################################################

# Interactions : (A -> B) + (B -> A)

migr2 <- migr2[migr2$i != migr2$j, ] 

for (k in 1:length(migr2$i)) {
  val1 <- migr2$fij[k]
  val2 <- migr2[migr2$i == migr2$j[k] & migr2$j == migr2$i[k], "fij"]
  migr2$interaction[k] <- sum(val1, val2)
}

# Suppression des doublons

interactions = data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("i", "j", "interaction"))))
for (k in 1:length(migr2$i)) {
  idi = migr2$i[k]
  idj = migr2$j[k]
  test = length(interactions[(interactions$i == idi &
                  interactions$j == idj) | (interactions$i == idj & interactions$j == idi), "interaction"])
    if (test == 0) {
    interactions <- rbind(interactions, data.frame(i = idi, j = idj, interaction = migr2$interaction[k]))
  }
}

##########################################################################"

# On élimine les petits flux

threshold <- 2000
interactions <- interactions[interactions$interaction >= threshold,]

# Calcul des liens

links <-
  mf_get_links(
    x = subregions,
    df = interactions,
    x_id = "id",
    df_id = c("i", "j")
  )

head(links)
# Cartographie

template("L'Arique, un continent encore isolé dans la mondialisation", "maps/heran.png")

col2 = "#4e4f4f"

mf_map(
  subregions,
  col = "#4e4f4f",
  border = "#3b3b3b",
  lwd = 0.5,
  add = TRUE
)


mf_map(
  links,
  var = "interaction",
  col = col,
  border = "#3b3b3b",
  type = "prop",
  lwd_max = 25,
  leg_pos = "bottomleft",
  leg_title = paste0("Migratons INTER régionales (interactions)\n(A -> B) + (B -> A)\nSeuil : ",threshold, "\nen milliers de personnes"),
  add = TRUE
)

mf_map(
  subregions,
  var = "intra",
  col = "#3b3b3b",
  border = col,
  lwd = 1.5,
  type = "prop",
  symbol = "square",
  leg_pos = "topright",
  leg_title = "Migrations INTRA\nrégionale nen 2019\n(en milliers)",
  add = TRUE
)

mf_label(
  subregions,
  var = "intra",
  halo = FALSE,
  cex = sqrt(as.numeric(subregions$intra) / 12000),
  col = col,
  overlap = TRUE,
  lines = FALSE
)

mf_label(
  links,
  var = "interaction",
  halo = TRUE,
  cex = 0.5,
  col = col2,
  bg = col,
  r = 0.1,
  overlap = FALSE,
  lines = FALSE
)

dev.off()
