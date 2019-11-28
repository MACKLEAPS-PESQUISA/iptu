library(sf)
library(dplyr)

GEO <- st_read("/home/tsuneki/Downloads/FAU/LOG2018/LOG2018_CEM_RMSP.shp")
GEO <- filter(GEO, MUNICIPIO == "SAO PAULO")

GEO <- GEO[,c(6,26)]
GEO <- GEO[!is.na(GEO$BAIRRO),]

st_write(GEO, "/home/tsuneki/Downloads/FAU/git/iptu/planilhas/geometria/geometria.shp")