library(dplyr)
library(foreign)
library(sf)
library(tidyverse)
library(ineq)

geo <- st_read("/home/tsuneki/Downloads/FAU/git/iptu/planilhas/geometria/original/geometria.shp")
gini <- read.csv("/home/tsuneki/Downloads/FAU/git/iptu/planilhas/resultados/1995/Gini1995.csv", sep = ";")


geo_final <- filter(geo, BAIRRO == toString(gini[1,1]))

geo_final[,3] <- gini[1,2]

colnames(geo_final)[3] <- "GINI"

i <- 2

while(i < length(gini$Bairro)){
  geo_aux <- filter(geo, BAIRRO == toString(gini[i,1]))
  geo_aux[,3] <- gini[i,2] 
  colnames(geo_aux)[3] <- "GINI"
  geo_final <- rbind(geo_final, geo_aux)
  i <- i + 1
}

st_write(geo_final, "/home/tsuneki/Downloads/FAU/git/iptu/planilhas/resultados/1995/geometria1995.shp")
