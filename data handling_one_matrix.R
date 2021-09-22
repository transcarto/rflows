#----------------------------
# DATA HANDLING : COMPUTE SEVERAL TYPES OF MATRIX
# Computes bilateral flows (Fij) and marginal (on i) flow indicators
#----------------------------

#rm(list=ls())

# Compute flow types for one matrix

setwd("D:/R/github/transcarto/rflows")

# Packages
#----------------------------

library("dplyr")
library("cartograflow")


# Import data
#----------------------------

#geography
countries <- st_read("./data/world/geom/countries.gpkg")

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



#----------------------------
# (I) Compute flows types indicators (on fij) 
# for OD flowmapping
#----------------------------



# 1) Close and square the matrix
#----------------------------

# creating a single vector list of codes
liste<-countries%>%select(adm0_a3_is)
liste<-as.data.frame(liste$adm0_a3_is)


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



# (2) Compute Gross and net flows as Tobler's 
#----------------------------


# Compute bilateral flow volum : FSij
flow_vol<-flowtype(tabflow, origin ="i",destination="j",fij="Fij", 
                   format="L",x="bivolum")

# Compute bilateral flow balance : FBij
flow_net<-flowtype(tabflow, origin ="i",destination="j",fij="Fij", 
                   format="L", x="bibal")




# (3) Compute asymmetry of bilateral flows : FAij
#----------------------------

flow_asy<-flowtype(tabflow, origin ="i",destination="j",fij="Fij", 
                   format="L", x="biasym")



#suppress NA cells due to zero division (eg for flow_asy)

for (i in 1:nrow(flow_asy))
  for (j in 1:ncol(flow_asy))
  {if (is.na.data.frame(flow_asy[i,j])==TRUE) {flow_asy[i,j]<-0}
  }



#----------------------------
# Compute all 9 types of bilateral flows indicators (on Fij)
# (and supress NA)
#---------------------------
# Fij : observed flow 
# Fji : reverse flow 
# FSij : bilateral volum flow
# FBij : bilateral balance flow
# FAij : asymetry of bilateralflow
# minFij : bilateral - cooperation - ie. symetry as min of (Fij, Fji)
# maxFij : bilateral - competition - ie. symetry as max of (Fij, Fji)
# rangeFij : bilateral range as (maxFij - minFij)# FDij : bilateral disymetry as (bilateral volum / bilateral range)

flow_indic<-flowtype(tabflow, origin ="i",destination="j",fij="fij",
                     format="L", x="alltypes")

# supress NA cells

dim(flow_indic)

for (i in 1:nrow(flow_indic))
  for (j in 1:ncol(flow_indic))
  {if (is.na.data.frame(flow_indic[i,j])==TRUE) {flow_indic[i,j]<-0}
  }

flow_indic$i<-as.character(flow_indic$i)
flow_indic$j<-as.character(flow_indic$j)

head(flow_indic)

# Export 

if(!dir.exists("./data/world/fij_indic"))
{dir.create("./data/world/fij_indic")}

st_write(flow_indic,"./data/world/fij_indic/fij_indic_migr2019.csv")




#----------------------------
# (II) Compute flows margins indicators (on i) 
# for choropleth flowmapping
#----------------------------

flow<- read.csv2("./data/world/fij/migr2019_T.csv",
                 header=TRUE,sep=",",
                 stringsAsFactors=FALSE,
                 encoding="UTF-8",dec=".", check.names=FALSE)


# Oi : marginal sum of the place of origin
#----------------------------
tabOi<-flow %>%
  group_by(i)%>%
  summarise(Oi = sum(fij),count_Oi = n())

# Di : marginal sum of the place of destination
#----------------------------
tabDj<-flow %>%
  group_by(j)%>%
  summarise(Dj = sum(fij),count_Dj = n())

colnames(tabDj)<-c("i", "Dj","count_Dj")

as.data.frame(tabOi)
as.data.frame(tabDj)

# Margin OiDj table
#----------------------------
tabOiDj<-merge(tabOi,tabDj,
               by=c("i"), 
               all.X=TRUE,all.Y=TRUE)

# Add Asymetry
#----------------------------
tabOiDj <- tabOiDj %>%
  mutate (Vol=Oi+Dj, Bal=Oi-Dj, Asy=Bal/Vol)

str(tabOiDj)

tabOiDj$i<-as.character(tabOiDj$i)
tabOiDj$Oi<-as.numeric(tabOiDj$Oi)
tabOiDj$Dj<-as.numeric(tabOiDj$Dj)
tabOiDj$Vol<-as.numeric(tabOiDj$Vol)
tabOiDj$Bal<-as.numeric(tabOiDj$Bal)
tabOiDj$Asy<-as.numeric(tabOiDj$Asy)


# Export
#----------------------------

if(!dir.exists("./data/world/fij_OiDj"))
{dir.create("./data/world/fij_OiDj")}

st_write(tabOiDj,"./data/world/fij_OiDj/tabOiDj_migr2019.csv")