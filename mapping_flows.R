# --------------------------
# FLOWMAP supérieur à un critère global (moyenne, quantile, etc.)
# --------------------------

#rm(list=ls())

# Packages
#----------------------------

library("sf")
library("dplyr")
#library("spatstat")
library("mapsf")
library("cartograflow")

# Import data
#----------------------------


#geom

countries <- st_read("data/world/geom/countries.gpkg")

pt<- read.csv2("data/world/geom/XYcountries.csv",
               header=TRUE,sep=",",
               stringsAsFactors=FALSE,
               encoding="UTF-8",dec=".",
               check.names=FALSE)

graticule <- st_read("data/world/geom/graticule.gpkg")
bbox <- st_read("data/world/geom/bbox.gpkg")


#flows

flow<- read.csv2("./data/world/fij/migr2019_T.csv",
                 header=TRUE,sep=",",
                 stringsAsFactors=FALSE,
                 encoding="UTF-8",dec=".", check.names=FALSE)

# Variable typing
countries$adm0_a3_is<-as.character(countries$adm0_a3_is)

flow$i<-as.character(flow$i)
flow$j<-as.character(flow$j)
flow$fij<-as.numeric(flow$fij)



# Map projection
#----------------------------

crs <- "+proj=aeqd +lat_0=90 +lon_0=50 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
countries <- st_transform(x = countries, crs = crs)
graticule <- st_transform(x = graticule, crs = crs)
bbox <- st_transform(x = bbox, crs = crs)
land <- st_union(countries)

st_as_sf(x = pt,coords = c("X", "Y"),crs = crs)


# Map Template
#----------------------------

col = "#9F204280"
credit = paste0("Françoise Bahoken & Nicolas Lambert, 2021\n",
                "Source: United Nations, Department of Economic\n",
                "and Social Affairs, Population Division (2019)")
theme = mf_theme(x = "default", bg = "white", tab = FALSE, 
                 pos = "center", line = 2, inner = FALSE, 
                 fg = "#9F204270", mar = c(0,0, 2, 0),cex = 1.9)
template = function(title, file){
  mf_export(
    countries,
    export = "png",
    width = 1000,
    filename = file,
    res = 96,
    theme = theme, 
    expandBB = c(-.02,0,-.02,0)
  )
  mf_map(bbox, col = "#d5ebf2",border = NA, lwd = 0.5, add = TRUE)
  mf_map(graticule, col = "white", lwd = 0.5, add = TRUE)
  mf_map(countries, col = "#baaba2",border = "white", lwd = 0.5, add = TRUE)
  mf_map(land, col = NA,border = "#317691", lwd = 0.5, add = TRUE)
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

#-------------------------------
# Map 1 : Migrations up to a global criterion
# e.g. mean, quantile filtering ...
#-------------------------------

# criterion selection

fij<-(flow$fij)

mean<-mean(flow$fij)     #as Tobler's said

Q3<-quantile(flow$fij,0.75)   #25% of the most important migrations
Q95<-quantile(flow$fij, 0.95) # 5% of the most important migrations
Q98<-quantile(flow$fij, 0.98) # 2% of the most important migrations

max<-max(flow$fij)

# Flowmapping 

source <- "United Nations, Department of Economic and Social Affairs, Population Division (2019)"
authors <- "Françoise Bahoken & Nicolas Lambert, 2021"

# Graphic parameters
par(mar=c(0,0,1,0))

sizes <- getFigDim(x = countries, width = 1500,mar = c(0,0,0,0), res = 150)
png("maps/flow_sup_mean.png", width = sizes[1], height = sizes[2], res = 150)

# Overlay a spatial background 
par(bg = "NA")

plot(st_geometry(countries), col=NA, border=NA)

plot(st_geometry(bbox), col="#d5ebf2", border=NA, add=T)
plot(st_geometry(graticule), col = "white",lwd=1, add=T)
plot(st_geometry(countries), col="#e3d0c1", border = "white", add=T)
plot(st_geometry(land), col=NA, border = "#317691", lwd = 1, add=T)

x <- -16300000 ; y <- 12250000

flowmap(tab=flow,
        origin.f = "i",
        destination.f = "j",
        nodes= pt,
        code="adm0_a3_is",
        nodes.X="X",
        nodes.Y="Y",
        filter=T,
        threshold=mean,  # plot flow value > mean value
        taille=15,     
        a.head=1,      
        a.length = 0.09,
        a.col="#636363",
        add=TRUE)


#cartography
library(cartography)

legendPropLines(pos="bottomright",
                title.txt="Number of migrants",
                title.cex=1,   
                cex=0.8,
                values.cex= 0.7,     
                var=c(Q95,max),  
                lwd=15,               
                frame = FALSE,
                col="#636363",
                values.rnd = 0
)


layoutLayer(title = "Flux de migrants supérieurs à la moyenne",
            author <- authors,
            sources <- source,
            tabtitle = T,
            frame = TRUE,
            col = "#636363") # coltitle ="#636363"

dev.off()





