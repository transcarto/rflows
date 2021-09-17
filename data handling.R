
# -------------------------------------------
# 1 - NOMENCLATURE & "TAILOR-MADE" BASEMAPS 
# -------------------------------------------

# Packages

library("readxl")
library("rnaturalearth")
library("sf")
library("rmapshaper")
library("reshape2")

# Metadata

data_url <- "https://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByOriginAndDestination_2019.xlsx"
data_website <- "https://www.un.org/en/development/desa/population/migration/data/estimates2/estimates19.asp"
data_source <- "United Nations, Department of Economic and Social Affairs, Population Division (2019)"

# Download Data
if(!dir.exists("data")){dir.create("data")}
if(!dir.exists("data/input")){dir.create("data/input")}
file <- "data/input/UN_MigrantStockByOriginAndDestination_2019.xlsx"
download.file(url=data_url, destfile=file)

# Nomenclature

sheet <- "ANNEX"
nomenclature <- data.frame(read_excel(file, skip = 15, sheet = sheet))
nomenclature <- nomenclature[!is.na(nomenclature$Type.of.data),]

# Regional & Subregional Aggregates

fill <- function(nomenclature, min,max,Code1,Label1,Code2,Label2){
  n <- nomenclature
  n[n$Index >= min & n$Index <= max,"Code1"] <- Code1
  n[n$Index >= min & n$Index <= max,"Label1"] <- Label1 
  n[n$Index >= min & n$Index <= max,"Code2"] <- Code2
  n[n$Index >= min & n$Index <= max,"Label2"] <- Label2 
  nomenclature <- n
  }

nomenclature <- fill(nomenclature, 24,43,947,"Sub-Saharan Africa",910,"Eastern Africa")
nomenclature <- fill(nomenclature, 45,53,947,"Sub-Saharan Africa",911,"Middle Africa")
nomenclature <- fill(nomenclature, 55,59,947,"Sub-Saharan Africa",913,"Southern Africa")
nomenclature <- fill(nomenclature, 61,77,947,"Sub-Saharan Africa",914,"Western Africa")
nomenclature <- fill(nomenclature, 80,86,1833,"Northern Africa and Western Asia",912,"Northern Africa")
nomenclature <- fill(nomenclature, 88,105,1833,"Northern Africa and Western Asia",922,"Western Asia")
nomenclature <- fill(nomenclature, 108,112,921,"Central and Southern Asia",5500,"Central Asia")
nomenclature <- fill(nomenclature, 114,122,921,"Central and Southern Asia",5501,"Southern Asia")
nomenclature <- fill(nomenclature, 125,131,1832,"Eastern and South-Eastern Asia",906,"Eastern Asia")
nomenclature <- fill(nomenclature, 133,143,1832,"Eastern and South-Eastern Asia",920,"South-Eastern Asia")
nomenclature <- fill(nomenclature, 146,171,1830,"Latin America and the Caribbean",915,"Caribbean")
nomenclature <- fill(nomenclature, 173,180,1830,"Latin America and the Caribbean",916,"Central America")
nomenclature <- fill(nomenclature, 182,195,1830,"Latin America and the Caribbean",931,"South America")
nomenclature <- fill(nomenclature, 197,198,909,"Oceania",927,"Australia / New Zealand")
nomenclature <- fill(nomenclature, 201,205,909,"Oceania",928,"Melanesia")
nomenclature <- fill(nomenclature, 207,213,909,"Oceania",954,"Micronesia")
nomenclature <- fill(nomenclature, 215,223,909,"Oceania",957,"Polynesia")
nomenclature <- fill(nomenclature, 227,236,917,"Europe",923,"Eastern Europe")
nomenclature <- fill(nomenclature, 238,250,917,"Europe",924,"Northern Europe")
nomenclature <- fill(nomenclature, 252,267,917,"Europe",925,"Southern Europe")
nomenclature <- fill(nomenclature, 269,277,917,"Europe",926,"Western Europe")
nomenclature <- fill(nomenclature, 279,283,918,"Northern America",918,"Northern America")

# Geometries

world <- ne_countries(scale = 50, returnclass = "sf")
world <- world[,c("un_a3","adm0_a3_is","name_long","geometry")]
world$un_a3 <- as.numeric(world$un_a3)

# Split up the mainland and the islands (France, Netherlands, New Zaeland)

ctr <- c("France","Netherlands","New Zealand")
tmp <- ne_countries(scale = 50, type = "map_units", country = ctr, returnclass = "sf")
tmp <- tmp[,c("un_a3","adm0_a3_is","name_long","geometry")]
tmp[tmp$name_long=="France","un_a3"] <- 250
tmp[tmp$name_long=="Caribbean Netherlands","un_a3"] <- 535
tmp[tmp$name_long=="Caribbean Netherlands","name_long"] <- "Bonaire, Sint Eustatius and Saba"
world <- world[!world$name_long %in% ctr,]
world <- rbind(world,tmp)

# Merge several entities

world[world$name_long == "Jersey",1:3] <- c(830, "JEY","Channel Islands")
world[world$name_long == "Guernsey",1:3] <- c(830, "JEY","Channel Islands")
world[world$name_long == "Aland Islands",1:3] <- c(246, "FIN","Finland")
world[world$name_long == "Northern Cyprus",1:3] <- c(196, "CYP","Cyprus")
world[world$name_long == "Kosovo",1:3] <- c(688, "SRB","Serbia")
world[world$name_long == "Somaliland",1:3] <- c(706, "SOM","Somalia")
world[world$name_long == "Taiwan",1:3] <- c(156, "CHN","China")
world <- aggregate(world, by=list(world$un_a3), FUN = head, 1)
world <- world[,-1]
st_geometry(world) <- st_cast(world$geometry, "MULTIPOLYGON")

# Tiny Countries

tiny <- c("tuvalu","gibraltar")
tmp <- ne_countries(scale = 10, type = "map_units", geounit = tiny, returnclass = "sf")
tmp <- tmp[,c("un_a3","adm0_a3_is","name_long","geometry")]
world <- world[!world$name_long %in% tiny,]
world <- rbind(world,tmp)

# Westren Sahara

world[world$name_long=="Western Sahara","adm0_a3_is"] <- "ESH"

# Polygon Removal

world <- world[!is.na(world$un_a3),]

# Selection of usefull fields

world <- merge(world,nomenclature, by.x = "un_a3",by.y = "Code",all.Y = TRUE)
fields <- c("un_a3","adm0_a3_is","Region..subregion..country.or.area","Code1","Label1","Code2","Label2",
            "More.Developed.Regions","Less.Developed.Regions","Least.developed.countries",
            "High.income.Countries","Middle.income.Countries","Upper.middle.income.Countries",
            "Lower.middle.income.Countries","Low.income.Countries","No.income.group.available",
            "geometry")
world <- world[,fields]
colnames(world)[3] <- "label"

# Cartographic Generalization
  
world <- ms_simplify(world, keep_shapes = TRUE, snap = TRUE, keep = 0.2)

# Aggregations

regions <- aggregate(world, by=list(world$Code1), FUN = head, 1)
regions <- regions[,c("Code1","Label1")]
st_geometry(regions) <- st_cast(regions$geometry, "MULTIPOLYGON")
colnames(regions) <- c("id","label","geometry")

subregions <- aggregate(world, by=list(world$Code2), FUN = head, 1)
subregions <- subregions[,c("Code2","Label2")]
st_geometry(subregions) <- st_cast(subregions$geometry, "MULTIPOLYGON")
colnames(subregions) <- c("id","label","geometry")

# Export geopackage

if(!dir.exists("data/geom")){dir.create("data/geom")}
st_write(world,"data/geom/countries.gpkg")
st_write(regions,"data/geom/regions.gpkg")
st_write(subregions,"data/geom/subregions.gpkg")

# Other geometries

graticule <- ne_download(scale = "medium", type = "graticules_30", category = "physical", returnclass = "sf")
bbox <- ne_download(scale = "medium", type = "wgs84_bounding_box", category = "physical", returnclass = "sf")
st_write(graticule,"data/geom/graticule.gpkg")
st_write(bbox,"data/geom/bbox.gpkg")

# --------------------------------
# 2 - STATISTICAL DATA HANDLING
# --------------------------------

migfiles <- function(sheet,year){
  
  if(sheet == "Table 1"){gender <- "T"}
  if(sheet == "Table 2"){gender <- "M"}
  if(sheet == "Table 3"){gender <- "F"}
  
  migr <- data.frame(read_excel(file, skip = 15, sheet = sheet))
  migr <- migr[migr[,1]==year,]
  
  # Data Cleaning
  
  migr <- migr[!is.na(migr[,6]),]
  migr <- subset(migr, select=-c(...1,...2,...5,...4, ...6,Total,Other.North,Other.South))
  colnames(migr)[1] <- "i"
  migr <- migr[order(migr[,"i"], decreasing =FALSE),]
  for (i in 2:length(colnames(migr))){migr[,i] <- as.numeric(migr[,i])}
  
  # ISO codes
  
  countries <- data.frame(world[,1:3])
  countries$geometry <- NULL
  countries <- countries[order(countries[,"label"], decreasing =FALSE),]
  codes <- countries$adm0_a3_is
  ## Verif
  # countries$rows <- migr[,"i"]
  # countries$cols <- colnames(migr)[-1]
  # View(countries)
  rownames(migr) <- codes
  colnames(migr) <- c("i",codes)
  migr <- migr[,-1]

  # matrix transposition
  migr <- t(migr)
  
  # Export Matrix format
  
  if(!dir.exists("data/matrix")){dir.create("data/matrix")}
  filename <- paste0("data/matrix/migr",year,"_",gender,".csv")
  write.csv(migr,filename, na="")
  
  # Export i,j,fij format
  
  if(!dir.exists("data/fij")){dir.create("data/fij")}
  filename <- paste0("data/fij/migr",year,"_",gender,".csv")
  
  migr2 <- melt(migr)
  colnames(migr2) <- c("i","j","fij")
  migr2 <- migr2[!is.na(migr2$fij),]
  write.csv(migr2,filename, na="", row.names = FALSE)
  
}

# Creating Files

dates <- c(1990,1995,2000,2005,2010,2015,2019)

for(i in dates) {
  migfiles("Table 1",i)
  migfiles("Table 2",i)
  migfiles("Table 3",i)
}

###############
# world Stats
################

world <- data.frame(read_excel(file, skip = 15, sheet = "Table 1"))
world <- world[world$...3 == "WORLD", c("...1","Total")]
colnames(world) <- c("year","T")
M <- data.frame(read_excel(file, skip = 15, sheet = "Table 2"))
world <- cbind(world, M = M[M$...3 == "WORLD", "Total"])
F <- data.frame(read_excel(file, skip = 15, sheet = "Table 3"))
world <- cbind(world, F = F[F$...3 == "WORLD", "Total"])
write.csv(world,"data/fij/world.csv", na="", row.names = FALSE)
