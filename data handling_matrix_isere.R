# -------------------------------------------
# Isere - Mobilites professionnelles MOBPRO 2017 - 2020
#
# Download Data handling and matrix preparation 
# -------------------------------------------

#rm(list=ls())

setwd("D:/R/github/transcarto/rflows/")

library("tidyverse")

#-----------------------
# I- download data
#-----------------------

data_url <- "https://www.insee.fr/fr/statistiques/fichier/4509353/base-csv-flux-mobilite-domicile-lieu-travail-2017.zip"
data_website <- "https://www.insee.fr/fr/statistiques/4509353"
data_source <- "INSEE, Mobilités professionnelles en 2017 : déplacements domicile - lieu de travail, Recensement de la population - Base flux de mobilité"


if(!dir.exists("./data/isere")){dir.create("data")}
if(!dir.exists("./data/isere/input")){dir.create("./data/isere/input")}

file <- "./data/isere/input/base-csv-flux-mobilite-domicile-lieu-travail-2017.zip"

# Dezip
#-----------------------

download.file(url =data_url, destfile = "./data/isere/input/mobpro2017.zip")
unzip("./data/isere/input/mobpro2017.zip", exdir = "data/isere/input")
file.remove("data/isere/input/mobpro2017.zip")

mobpro2017 <-st_read(dsn = "./data/isere/input/base-flux-mobilite-domicile-lieu-travail-2017.csv",
                     stringsAsFactors = F)

#-----------
# Extraction des OD de l'Isère (commune*commune)
#-----------

# Création d'un champs département origine et destination
#-----------

tabflow<-mobpro2017

tabflow=tabflow %>%
  mutate (dept_O=CODGEO, dept_D=DCLT)

tabflow$dept_O <- substr(tabflow$dept_O, 1, 2)
tabflow$dept_D <- substr(tabflow$dept_D, 1, 2)

head(tabflow)


# Filtrage du département 38 en origine et zn destination
#-----------

tabflow=filter(tabflow,dept_O=="38")
tabflow=filter(tabflow,dept_D=="38")
head(tabflow)


# Variable typing
#-----------
tabflow$NBFLUX_C17_ACTOCC15P<-as.numeric(tabflow$NBFLUX_C17_ACTOCC15P)

# Export
#-----------

if(!dir.exists("./data/isere")){dir.create("data")}
if(!dir.exists("./data/isere/fij")){dir.create("./data/isere/fij")}


st_write(tabflow,"./data/isere/fij/mobpro_isere2017.csv")

