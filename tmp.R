library(sf)



library("readxl")
library("comparator")
library("reshape2")
# Import et lise en forme des données


data_url <- "https://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByOriginAndDestination_2019.xlsx"
file <- "data/world/UN_MigrantStockByOriginAndDestination_2019.xlsx"
if (!file.exists(file)){
download.file(url=data_url, destfile=file)
} 

sheet <- "Table 1"
year <- 2019

migr <- data.frame(read_excel(file, skip = 15, sheet = sheet))
migr <- migr[migr[,1]==year,]


# Data Cleaning

migr <- migr[!is.na(migr[,6]),]
migr <- subset(migr, select=-c(...1,...2,...5,...4, ...6,Total,Other.North,Other.South))
colnames(migr)[1] <- "i"
migr <- migr[order(migr[,"i"], decreasing =FALSE),]
for (i in 2:length(colnames(migr))){migr[,i] <- as.numeric(migr[,i])}

# Get ISO codes

ctr <- countries[,1:3] %>% st_drop_geometry()
ctr <- ctr[order(ctr[,"label"], decreasing =FALSE),]
codes <- ctr$adm0_a3_is

# Verification manuelle
ctr$rows <- migr[,"i"]
ctr$cols <- colnames(migr)[-1]
for(i in 1:nrow(ctr)){
  ctr$rows_test[i] = LCS(similarity = TRUE)(ctr$label[i], ctr$rows[i]) / ((nchar(ctr$label[i]) + nchar(ctr$rows[i])) / 2) * 100
  ctr$cols_test[i] = LCS(similarity = TRUE)(ctr$label[i], ctr$cols[i]) / ((nchar(ctr$label[i]) + nchar(ctr$cols[i])) / 2) * 100
}

head(ctr[order(ctr$cols_test),c("label","rows","cols","rows_test", "cols_test")], decreasing = FALSE)

# On replace les noms de lignes et de colonnes par les codes ISO

rownames(migr) <- codes
colnames(migr) <- c("i",codes)
migr <- migr[,-1]

View(migr)

# On transpose la matrice

migr <- t(migr)

# Avec reshape2, on la convertit au format i,j,fij

migr <- melt(migr)
colnames(migr) = c("i","j","fij")
migr <- migr[!is.na(migr$fij),]
migr = migr[migr$fij>0,]
migr = migr[order(migr$fij, decreasing = TRUE),]

