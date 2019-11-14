startTime <- Sys.time()
library(dplyr)
library(foreign)
library(sf)
library(tidyverse)
library(ineq)

#lendo a tabela LOG2018_CEM_RMSP.dbf
tabela <- read.dbf("/home/tsuneki/Downloads/FAU/LOG2018/LOG2018_CEM_RMSP.dbf", as.is = F)[,c(6,19)]

#criando um dataframe com a tabela LOG2018_CEM_RMSP.dbf
df <- data_frame()
df <- bind_rows(tabela)

#retirando os valores nulos do CEP
tabela <- df[!is.na(df$CEP_E),]

rm(df)

#lendo a tabela do IPTU de 1995
tab <- read.csv2("/home/tsuneki/Downloads/FAU/csv/IPTU_1995.csv", header = T, sep = ";", stringsAsFactors = F, dec = ",", quote = "")[,c(7,18,20,21,22,23,24,25,32)]

#reformatando os CEPs
for(i in 1:length(tabela[,2])){
  tabela[i, 3] <- paste(substring(tabela[i, 2], c(1,6), c(5,8)), collapse = "-")
}

#retirando a coluna com os CEPs nao formatados
tabela <- tabela[c(1,3)]
names(tabela) <- c("Bairro","CEP.DO.IMOVEL")


#criando um dataframe com a juncao das tabelas a partir do CEP
aux <- data_frame()
aux <- left_join(tab, tabela, by = "CEP.DO.IMOVEL")  
aux <- aux %>% distinct()
aux <- aux[!is.na(aux$Bairro),]

rm(tab, tabela)

#tratando a tabela dos bairros desejados
bairros <- read.csv("/home/tsuneki/Downloads/FAU/csv/Distrito_zonas.csv", sep = ",", header = T)
names(bairros) <- c("CaD","Bairro","Z. Fiscal")
bairros <-bairros[c(2,3)]

#tratando a tebaela de geolocalizacoes
geo <- st_read("/home/tsuneki/Downloads/FAU/LOG2018/LOG2018_CEM_RMSP.shp")[c(6,26)]
geo <- geo[!is.na(geo$BAIRRO),]

colnames(geo)[1] <- "Bairro"

bairros <- inner_join(bairros, geo, by = "Bairro")

rm(geo)

teste <- duplicated(bairros[,1])

cout<-1

for(i in 1:length(teste)){
  i <- i+0
  if(teste[i] == FALSE){
    bairros[cout,] <- bairros[i,]
    cout <- cout + 1
  }
}

bairros <- bairros[-c(92:23502),]

rm(cout, teste)

#criando a tabela com os dados, bairros e geolocalkizacoes desejados
tabelaFimBairros <- inner_join(aux, bairros, by = "Bairro")  

rm(aux, bairros)

tabelaFimBairros <- tabelaFimBairros %>% distinct()

#calculo do valor venal
for(i in 1:length(tabelaFimBairros[,1])){
  areaTerreno <- tabelaFimBairros[i,4] + 0
  fracaoIdeal <- tabelaFimBairros[i,3] + 0
  m2Terreno <- tabelaFimBairros[i,7] + 0
  VVT <- areaTerreno*fracaoIdeal*m2Terreno
  
  areaConstrucao <- tabelaFimBairros[i,5] + 0
  m2Construcao <- tabelaFimBairros[i,8] + 0
  FOBS <- tabelaFimBairros[i,9] + 0
  VVC <- areaConstrucao*m2Construcao*FOBS
  
  VVI = VVC+VVT
  
  tabelaFimBairros[i,13] <- paste(VVI)
}

rm(areaTerreno,fracaoIdeal,m2Terreno,VVT,areaConstrucao,m2Construcao,FOBS,VVC,VVI)

# Coluna 13 = VVI
colnames(tabelaFimBairros)[13] <- "VVI" 
View(tabelaFimBairros)




#adicionar ano na tabela
tabelaFimBairros[,14] <- 95

#renomeacao das colunas
names(tabelaFimBairros) = c("NOME.DO.CONTRIBUINTE", "CEP.DO.IMOVEL","FRACAO.IDEAL","AREA.DO.TERRENO","AREA.CONSTRUIDA","AREA.OCUPADA","VALOR.DO.M2.DO.TERRENO","VALOR.DO.M2.DE.CONSTRUCAO","FATOR.DE.OBSOLESCENCIA","BAIRRO","Z.FISCAL","GEOMETRIA","VVI","ANO")


#criacao de um .csv da tabela tratada
write.csv(tabelaFimBairros, "/home/tsuneki/Downloads/FAU/git/iptu/planilhas/resultados/tabelaFinal95.csv")
endTime <- Sys.time()
endTime - startTime

#calculo do Gini
ListaBairros <- unique(tabelaFimBairros$BAIRRO)

giniFinal <- data_frame()

bairroVVI <- tabelaFimBairros[,c(10,13)]

x<- data_frame()

i <- 1

while(i <= length(ListaBairros)){
  x <- data_frame()
  x[1,1] <- ListaBairros[i]
  x[1,2] <- NA
  names(x) <- c("BAIRRO","VVI")
  
  x <- inner_join(x, bairroVVI, by="BAIRRO")
  
  x <- x[,c(1,3)]
  colnames(x)[2] <- "VVI"
  
  aux <- x[,2]
  aux <- as.numeric(unlist(aux))
  gini <- Gini(aux)

  giniFinal[i,1] <- x[1,1]
  giniFinal[i,2] <- gini
  i <- i + 1
}

  names(giniFinal) <- c("Bairro","Gini")
  