# Isere - flowmap tests


#rm(list=ls())

setwd("D:/R/github/transcarto/rflows")


#Packages
library("dplyr")
library("spatstat")

library("sf")
library("cartograflow")
library("mapsf")


# Load data
#--------------------

fdc <- st_read("D:/R/github/transcarto/rflows/data/isere_com.shp", quiet = TRUE ) 

flow<- read.csv("D:/R/github/transcarto/rflows/data/isere/fij/mobpro_isere2017.csv", 
                header=TRUE,sep=",",
                stringsAsFactors=FALSE,
                encoding="UTF-8",dec=".", check.names=FALSE)

head(flow)

#Selecting useful variables
flow<-flow %>% select(CODGEO,DCLT,fij)
colnames(flow)<-c("i","j","fij")

# Variable typing
flow$i<-as.character(flow$i)
flow$j<-as.character(flow$j)
flow$fij<-as.numeric(flow$fij)


#Spaghetti
#--------------
# (1) Simple vizu du flux

flowmap(tab=flow,
        origin.f = "i",
        destination.f = "j",
        bkg= fdc, #nodes= pt
        code="INSEE_COM",
        nodes.X="X",
        nodes.Y="Y",
        filter=F,
        plota=fdc
)


#(2) Vizu with global filtering
#------------------------------------

# flow criterion selection
#---------------------------
summary(flow$fij)


mean<-mean(flow$fij)     #as Tobler's said
Q3<-quantile(flow$fij,0.75)   #25% of the most important migrations
Q95<-quantile(flow$fij, 0.95) # 5% of the most important migrations
Q98<-quantile(flow$fij, 0.98) # 2% of the most important migrations
max<-max(flow$fij)


plot.new()

par(bg = "NA")

plot(st_geometry(fdc), col=NA, border=NA)

mf_map(fdc, col = "#CCCCCC", border = "white", lwd = 0.5, add = TRUE)

#plot(st_geometry(fdc), col="#e3d0c1", border = "white", add=T)

flowmap(tab=flow,
        origin.f = "i",
        destination.f = "j",
        bkg= fdc, #nodes= pt
        code="INSEE_COM",
        nodes.X="X",
        nodes.Y="Y",
        filter=T,
        threshold=Q95,  # plot the top 3% flow value
        taille=20,     
        a.head=1,      
        a.length = 0.09,
        a.angle = 30,
        a.col="#636363",
        add=TRUE
)


#--- variante bilateral flows ---------

#compute bilateral volum (gross) FSij
#---------

flow_vol<-flowtype(flow, format="L", origin="i", destination="j", fij="fij",x="bivolum")

head(flow_vol)

# Bivolum thresholding criterion
mean<-mean(flow_vol$FSij)
Q95<-quantile(flow_vol$FSij,0.95)
colnames(flow_vol)<-c("i","j","fij")

# Plot bivol

plot.new()

par(bg = "NA")

plot(st_geometry(fdc), col=NA, border=NA)

mf_map(fdc, col = "#CCCCCC", border = "white", lwd = 0.5, add = TRUE)


flowmap(tab=flow_vol,
        origin.f = "i",
        destination.f = "j",
        bkg= fdc, #nodes= pt
        code="INSEE_COM",
        nodes.X="X",
        nodes.Y="Y",
        filter=T,
        threshold=mean,  # plot the up-mean bilateral volum of flows
        taille=20,     
        a.head=0,   #no arrow   
        a.col="#636363",
        add=TRUE
)


#(3) Customize the map : add theme etc.
#--------------------------


# Theme - template Isere
#--------

communes<-fdc

col = "#82d477" #c291bc"

credits = paste0("Bahoken Françoise & Nicolas Lambert, 2021\n",
                 "Source: IGN & INSEE, 2021")
theme = mf_theme(x = "default", bg = "#f0f0f0", tab = FALSE, 
                 pos = "center", line = 2, inner = FALSE, 
                 fg = col, mar = c(0,0, 2, 0),cex = 1.9)


template = function(title, file, note = "", basemap = TRUE, scale = TRUE){
  
  mf_export(
    communes,
    export = "png",
    width = 1000,
    filename = file,
    res = 96,
    theme = theme, 
    expandBB = c(-.02,0,-.02,0)
  )
  
  if (basemap == TRUE){
    mf_shadow(x = communes, col = "grey50", cex = 1, add = TRUE)
    mf_map(communes, col ="#CCCCCC", border = "white", lwd = 0.5, add = TRUE)
  }
  
  mf_title(title)
  
  if (scale == TRUE){
    mf_scale(size = 20, pos = "bottomright", lwd = 1.2, cex = 1, col = "#383838", unit = "km")
  }
  
  mf_credits(
    txt = credits,
    pos = "bottomleft",
    col = "#1a2640",
    cex = 0.8,
    font = 3,
    bg = "#ffffff30"
  )
  
  if(note != ""){
    mf_annotation(
      x = c(885000, 6435000),
      txt = note,
      pos = "bottomleft", cex = 1.2, font = 2,
      halo = TRUE, s = 1.5
    )
  }
}


template("Template cartographique", "maps/template_isere_flow.png", note = "Département de\nl'Isère (38)")
mf_map(fdc, col = col, border = "white", lwd = 0.5, add = TRUE)
dev.off()

#-----------
#flowmap Isere
#-----------

#graphic parameters


# Overlay a spatial background 

plot.new()

par(bg = "NA")

plot(st_geometry(fdc), col=NA, border=NA) #bg="#f0f0f0"

mf_map(fdc, col = NA, border = "white", lwd = 0.5, add = TRUE)

flowmap(tab=flow,
        origin.f = "i",
        destination.f = "j",
        bkg= fdc, #nodes= pt
        code="INSEE_COM",
        nodes.X="X",
        nodes.Y="Y",
        filter=T,
        threshold=Q95,  # plot flow value > mean fij
        taille=15,     
        a.head=1,      
        a.length = 0.09,
        a.col="#82d477", #636363 (grey)
        add=TRUE
)


mf_legend_pl( pos="bottomleft",
              val = c(Q95,Q98, max),
              col=col,
              lwd=15, 
              title="Nombre de navetteurs, 2019",
              title_cex=1.0,
              val_cex= 0.8,
              frame=F,
              
)

#authors<- "Bahoken Françoise & Nicolas Lambert, 2021\n"
#sources<-"Source: IGN & INSEE/MOBPRO2019, 2021"

layoutLayer(title = "Flux de migrants supérieurs à xxx",
            author <- authors,
            sources <- source,
            tabtitle = T,
            frame = TRUE,
            col = "#82d477") # coltitle ="#636363"


#flowmap_1 = dotdensitymap(x = communes, var = "pop2018", onedot = onedot, radius = 300)

template("Carte de flux supérieur à XX", "maps/flowmap_isere_1.png")


dev.off()

