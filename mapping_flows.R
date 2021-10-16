
# --------------------------
# FLOWMAP supérieur à un critère global (moyenne, quantile, etc.)
# --------------------------

#setwd("D:/R/github/transcarto/rflows")

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

flow<- read.csv2("./data/migr.csv",
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

#-------------------------------
# Map 1 : Migrations up to a global criterion
# e.g. mean, quantile filtering ...
#-------------------------------

# criterion selection

fij<-(flow$fij)

mean<-mean(flow$fij)     #as Tobler's said

#Q3<-quantile(flow$fij,0.75)   #25% of the most important migrations
#Q95<-quantile(flow$fij, 0.95) # 5% of the most important migrations
Q98<-quantile(flow$fij, 0.98) # 2% of the most important migrations

max<-max(flow$fij)

# Flowmapping 

source <- "United Nations, Department of Economic and Social Affairs, Population Division (2019)"
authors <- "Françoise Bahoken & Nicolas Lambert, 2021"

# Graphic parameters
par(mar=c(0,0,1,0))

sizes <- getFigDim(x = countries, width = 1500,mar = c(0,0,0,0), res = 150)
png("maps/flow_supmean.png", width = sizes[1], height = sizes[2], res = 150)

# Overlay a spatial background 
par(bg = "NA")

plot(st_geometry(countries), col=NA, border=NA, bg="#dfe6e1")

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
                var=c(mean,max),  
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


#-----



#----------------------
# MAP (2) bilateral Volum & 2%
#----------------------

# creating a single vector list of codes
liste<-countries%>%select(adm0_a3_is)
liste<-as.data.frame(liste$adm0_a3_is)

# Square the matrix

tabflow<-flowcarre(tab=flow,
                   liste=liste,
                   origin = "i", dest="j",valflow="fij",
                   format="L",
                   diagonale = TRUE,
                   empty.sq = FALSE
)


colnames(tabflow)<-c("i", "j", "fij")

tabflow$i<-as.character(tabflow$i)
tabflow$j<-as.character(tabflow$j)
tabflow$fij<-as.numeric(tabflow$fij)

# compute gross flows

flow_vol<-flowtype(tabflow, origin ="i",destination="j",fij="Fij", 
                   format="L",x="bivolum", lowup="up")

colnames(flow_vol)<-c("i", "j", "fij")


# Flowmap gross flows : the top 2% plus important
#--------------------------

# criterion selection
Q98<-(quantile(flow_vol$fij, 0.98)) # 2% of the most important flows
max<-(max(flow_vol$fij))


# Flowmapping 

source <- "United Nations, Department of Economic and Social Affairs, Population Division (2019)"
authors <- "Françoise Bahoken & Nicolas Lambert, 2021"

# Graphic parameters
par(mar=c(0,0,1,0))

sizes <- getFigDim(x = countries, width = 1500,mar = c(0,0,0,0), res = 150)
png("maps/flowvol_2percent.png", width = sizes[1], height = sizes[2], res = 150)

# Overlay a spatial background 
par(bg = "NA")

plot(st_geometry(countries), col=NA, border=NA)

plot(st_geometry(bbox), col="#d5ebf2", border=NA, add=T)
plot(st_geometry(graticule), col = "white",lwd=1, add=T)
plot(st_geometry(countries), col="#e3d0c1", border = "white", add=T)
plot(st_geometry(land), col=NA, border = "#317691", lwd = 1, add=T)

x <- -16300000 ; y <- 12250000

flow<-flow_vol

flowmap(tab=flow,
        origin.f = "i",
        destination.f = "j",
        nodes= pt,
        code="adm0_a3_is",
        nodes.X="X",
        nodes.Y="Y",
        filter=T,
        threshold=Q98,  # plot flow value > mean value
        taille=15,     
        a.head=0,      #pas de tête de flèche
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
                var=c(Q98,max),  
                lwd=15,               
                frame = FALSE,
                col="#636363",
                values.rnd = 0
)


layoutLayer(title = "2% du volume de flux de migrants les plus importants",
            author <- authors,
            sources <- source,
            tabtitle = T,
            frame = TRUE,
            col = "#636363") # coltitle ="#636363"

dev.off()



#-------------------------------
# Map 3 : Migrations filtrées sur la distance
#
#-------------------------------

# 1) Calcul d'une matrice de distances
#---------------------------

#calcule les distances euclidiennes parcourues par le flux en utilisant la géométrie
# d'un fond de carte areal ou ponctuel 
# via une jointure

map<-countries


head(flow)

#jointure entre OD et fond de carte et calcul des centroïdes des zones
tab<-flowjointure(geom="area",
                  DF.flow=flow,origin = "i",destination = "j",
                  bkg=map,id="adm0_a3_is",x="X",y="Y")

#calcul des distances
tab.distance<-flowdist(tab,
                       dist.method = "euclidian",
                       result = "dist")

tab.distance<-tab.distance %>% select(i,j,distance)
tab<-tab %>% select(i,j,"fij"=ydata)

head(tab.distance)


# 2) Réduction de la matrice de flux en fonction de critères distances parcourues
# critères de distance minimum ou de distance maximum

Q1<-quantile(tab.distance$distance,0.25)   #25% des distances les plus courtes

tab.flow<-flowreduct(tabflow,
                     tab.distance,
                     metric = "continous",
                     d.criteria = "dmax", # critère de distance maximum parcourue 
                     d = 5112934)            # valeur du critère Q1 : 5100 km

# Sélection des fij > 0


flow.distQ1<-tab.flow %>%
  select(i,j,flowfilter) %>%
  filter(flowfilter !=0)

head(flow.distQ1)


#3) cartographie des flux parcourant moins de 5100 km





#------------------------------------------
#------------------------------------------


#----------------------------
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
            
