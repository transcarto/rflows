
#-------------------- 
# FLOWMAPPER carte Interactions rectangle (type = "rect")
# >> ligne 1356 et suivantes
#--------------------
# doit absolument être réalisée avec une matrice symétrique !

library("dplyr")
library("cartograflow")

#---------- fermeture de la matrice

# liste de codes des sous-régions

liste_subreg <- subregions %>% select(id)
liste_subreg <- as.data.frame(liste_subreg$id)

flow_sq <- flowcarre(
  tab = flows,
  liste = liste_subreg,
  origin = "i",
  dest = "j",
  valflow = "fij",
  format = "L",
  diagonale = TRUE,
  empty.sq = FALSE
)

colnames(flow_sq) <- c("i", "j","fij")                
       

#-------- calcul du volume bilateral avec cartograflow
flow_bivol<- flowtype(flow_sq,origin ="i",destination="j",fij="fij",
                      format = "L", x = "bivolum", lowup = "up")

head(flow_bivol)
colnames(flow_bivol) <- c("i", "j","fij")  

flow_bivol$i <- as.character(flow_bivol$i)
flow_bivol$j <- as.character(flow_bivol$j)
flow_bivol$fij <- as.numeric(flow_bivol$fij)

#---------- idem avec flowmapper_flowtypes

test2 <- flowtype(
  flow_sq,
  origin = "i",
  destination = "j",
  fij = "fij",
  format = "L",
  x = "bivolum",
  lowup = "up"
)

#----------- cartographie des rectangles

#```{r, eval = TRUE, message = FALSE, warning = FALSE, results = "hide"}

template("Interactions", "maps/ttt_interactions_volume.png")
c <- ttt_flowmapper(
  x = subregions,
  xid = "id",
  size = "thickness",
  type = "rect",
  df = flow_bivol,  #matrice symétrique
  dfid = c("i", "j"),
  dfvar = "fij", #Somme des flux entrants et sortants
  col = col,
  border = "#424242",
  border2 = col,
  add = TRUE
)
dev.off()


#```

#ttt_interactions_vol.png : image où les bandes sont //
# on décide donc de prendre la partie supérieure
#<img src = "maps/ttt_interactions_vol.png"></img>

#ttt_interactions_volume.png : image OK
#<img src = "maps/ttt_interactions_volume.png"></img>

