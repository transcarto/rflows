migr = read.dbf("./RP2018_migcom_dbf/FD_MIGCOM_2018.dbf")

library(dplyr)

migr_agg = migr %>% group_by(COMMUNE,ARM,DCRAN,CSM) %>% summarise(IPONDI=sum(IPONDI))

saveRDS(migr_agg %>% ungroup(),"FD_MIGCOM_2018AGG.rds")
migr_agg=readRDS("FD_MIGCOM_2018AGG.rds")%>% mutate_if(is.factor,as.character)

com_arr = migr_agg %>% select(COMMUNE,ARM) %>% filter(ARM!="ZZZZZ") %>% distinct()

migr_com = migr_agg  %>%
  left_join(com_arr,by=c("DCRAN"="ARM")) %>%
  mutate(COMMUNE_ANM1 = if_else(is.na(COMMUNE.y),DCRAN,COMMUNE.y),COMMUNE=COMMUNE.x) %>%
  select(COMMUNE,COMMUNE_ANM1,CSM,IPONDI) %>%
  mutate(DEP_ANM1= substr(COMMUNE_ANM1,1,2),DEP = substr(COMMUNE,1,2))

migr_clean = migr_com %>%
  filter(CSM==7) %>%
  filter(COMMUNE!=COMMUNE_ANM1) %>%
  filter(!(DEP %in% c("2A","2B",96:100)),!(DEP_ANM1 %in% c("2A","2B",96:100))) %>%
  arrange(desc(IPONDI))

migr_in = migr_clean %>% group_by(COMMUNE) %>% summarize(NIN = sum(IPONDI))
migr_out = migr_clean %>% group_by(COMMUNE_ANM1) %>% summarize(NOUT = sum(IPONDI))

delta = migr_in %>% full_join(migr_out,by=c("COMMUNE"="COMMUNE_ANM1")) %>%
  tidyr::replace_na(list(NIN=0,NOUT=0)) %>%
  mutate(delta = NIN-NOUT)

remotes::install_github("antuki/CARTElette/CARTElette@RPackage")
library(CARTElette)
COM <- charger_carte(COG=2020,nivsupra="COM")
library(sf)
library(dplyr)
write_sf(COM,"COM.gpkg")

delta_geo = COM %>%
  filter(!(INSEE_DEP %in% c("2A","2B",971:976))) %>%
  left_join(delta,by=c("INSEE_COM"="COMMUNE")) %>%
  tidyr:: replace_na(list(delta=0)) %>%
  st_transform(2154)


plot(delta_geo %>% group_by(INSEE_DEP) %>% summarize(delta=sum(delta)) %>% ungroup() %>% select(delta))

library(ttt)

pot = poisson.potential(delta_geo,cellsize = 10000,nb_it = 5000)
plot(pot %>% select(attractivity))



migr_clean = migr_com %>%
  filter(COMMUNE!=COMMUNE_ANM1) %>%
  group_by(COMMUNE,COMMUNE_ANM1) %>%
  summarise(IPONDI=sum(IPONDI)) %>%
  arrange(desc(IPONDI))

migr_in = migr_clean %>% group_by(COMMUNE) %>% summarize(NIN = sum(IPONDI))
migr_out = migr_clean %>% group_by(COMMUNE_ANM1) %>% summarize(NOUT = sum(IPONDI))

delta = migr_in %>% full_join(migr_out,by=c("COMMUNE"="COMMUNE_ANM1")) %>%
  tidyr::replace_na(list(NIN=0,NOUT=0)) %>%
  mutate(delta = NIN-NOUT)


m <- rbind(c(95000,5920000), c(95000,7110000), c(1270000,7110000),
           c(1270000,5920000), c(95000,5920000))
p <- st_sf(st_sfc(st_polygon(list(m))), crs = st_crs(delta_geo))

COMLAMB = COM %>% st_transform(2154)
etranger = st_sf(INSEE_COM="99999",INSEE_DEP="99",geometry=st_geometry(st_difference(p,st_union(COMLAMB))),
                 crs=st_crs(delta_geo))


COMWE = COM %>%st_transform(2154) %>% select(INSEE_COM,INSEE_DEP) %>% bind_rows(etranger)
tail(COMWE)



delta_geo = COMWE %>%
  left_join(delta,by=c("INSEE_COM"="COMMUNE")) %>%
  tidyr:: replace_na(list(delta=0))

delta_geo$delta[delta_geo$INSEE_COM=="99999"]

plot(delta_geo %>% group_by(INSEE_DEP) %>% summarize(delta=sum(delta)) %>% ungroup() %>% select(delta))


pot = poisson.potential(delta_geo,cellsize = 10000,method = "solve")
 plot(pot %>% select(attractivity))
