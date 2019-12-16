library(dplyr)
library(foreign)
library(sf)
library(tidyverse)
library(ineq)

'
TABELA DO IPTU
'

#lendo a tabela LOG2018_CEM_RMSP.dbf que contém os ceps e os respectivos bairros
cep_bairro <- read.dbf("/home/tsuneki/Downloads/FAU/LOG2018/LOG2018_CEM_RMSP.dbf", as.is = F)[,c(6,19)]

#criando um dataframe com a tabela LOG2018_CEM_RMSP.dbf para poder retirar os valores nulos do CEP
df <- data_frame()
df <- bind_rows(cep_bairro)

#retirando os valores nulos do CEP
cep_bairro <- df[!is.na(df$CEP_E),]

rm(df)

#lendo a tabela do IPTU
IPTU <- read.csv2("/home/tsuneki/Downloads/FAU/csv/IPTU_1995.csv", header = T, sep = ";", stringsAsFactors = F, dec = ",", quote = "")[,c(7,18,20,21,22,23,24,25,29,32)]

#reformatando os CEPs
for(i in 1:length(cep_bairro[,2])){
  cep_bairro[i, 3] <- paste(substring(cep_bairro[i, 2], c(1,6), c(5,8)), collapse = "-")
}
rm(i)

#retirando a coluna com os CEPs nao formatados
cep_bairro <- cep_bairro[c(1,3)]
names(cep_bairro) <- c("BAIRRO","CEP.DO.IMOVEL")


#criando um dataframe com a juncao das tabelas para incluir os bairros
IPTU_CEP <- data_frame()
IPTU_CEP <- left_join(IPTU, cep_bairro, by = "CEP.DO.IMOVEL")  
IPTU_CEP <- IPTU_CEP %>% distinct()
IPTU_CEP <- IPTU_CEP[!is.na(IPTU_CEP$BAIRRO),]

rm(cep_bairro, IPTU)

#tratando a tabela dos bairros desejados
bairros <- read.csv("/home/tsuneki/Downloads/FAU/csv/Distrito_zonas.csv", sep = ",", header = T)
names(bairros) <- c("CaD","BAIRRO","Z. Fiscal")
bairros <-bairros[c(2,3)]

#criando a tabela com os dados do IPTU, CEP e bairros
IPTU_CEP_BAIRROS <- inner_join(IPTU_CEP, bairros, by = "BAIRRO")  

rm(IPTU_CEP, bairros)

#separando os tipo de imoveis desejados
residencia <- IPTU_CEP_BAIRROS$TIPO.DE.USO.DO.IMOVEL[4]
residencia_coletiva <- IPTU_CEP_BAIRROS$TIPO.DE.USO.DO.IMOVEL[5]
residencia_loja <- IPTU_CEP_BAIRROS$TIPO.DE.USO.DO.IMOVEL[32]

IPTU_CEP_BAIRROS <- filter(IPTU_CEP_BAIRROS, TIPO.DE.USO.DO.IMOVEL == c(residencia, residencia_coletiva, residencia_loja, "apartamento"))


IPTU_CEP_BAIRROS <- IPTU_CEP_BAIRROS %>% distinct()

#calculo do valor venal
for(i in 1:length(IPTU_CEP_BAIRROS[,1])){
  areaTerreno <- IPTU_CEP_BAIRROS[i,4] + 0
  fracaoIdeal <- IPTU_CEP_BAIRROS[i,3] + 0
  m2Terreno <- IPTU_CEP_BAIRROS[i,7] + 0
  VVT <- areaTerreno*fracaoIdeal*m2Terreno
  
  areaConstrucao <- IPTU_CEP_BAIRROS[i,5] + 0
  m2Construcao <- IPTU_CEP_BAIRROS[i,8] + 0
  FOBS <- IPTU_CEP_BAIRROS[i,10] + 0
  VVC <- areaConstrucao*m2Construcao*FOBS
  
  VVI = VVC+VVT
  
  IPTU_CEP_BAIRROS[i,13] <- paste(VVI)
}

rm(i,areaTerreno,fracaoIdeal,m2Terreno,VVT,areaConstrucao,m2Construcao,FOBS,VVC,VVI)

#adicionar ano na tabela
IPTU_CEP_BAIRROS[,14] <- 1995

#renomeacao das colunas
names(IPTU_CEP_BAIRROS) = c("NOME.DO.CONTRIBUINTE", "CEP.DO.IMOVEL","FRACAO.IDEAL","AREA.DO.TERRENO","AREA.CONSTRUIDA","AREA.OCUPADA","VALOR.DO.M2.DO.TERRENO","VALOR.DO.M2.DE.CONSTRUCAO","TIPO.DE.USO.DO.IMOVEL","FATOR.DE.OBSOLESCENCIA","BAIRRO","Z.FISCAL","VVI","ANO")

#criacao do .csv da tabela tratada do IPTU
write.table(IPTU_CEP_BAIRROS, "/home/tsuneki/Downloads/FAU/git/iptu/planilhas/resultados/1995/IPTUfinal1995.csv", sep = ";", dec = ".", row.names = F)

'
CALCULO DO GINI
'

#Criação de uma tabela com apenas os bairros e VVI
Bairros <- unique(IPTU_CEP_BAIRROS$BAIRRO)

giniFinal <- data_frame()

Bairro_VVI <- IPTU_CEP_BAIRROS[,c(11,13)]


#Calculo do Gini e criação de um data frame do Gini por Bairro
x<- data_frame()

i <- 1

while(i <= length(Bairros)){
  x <- data_frame()
  x[1,1] <- Bairros[i]
  x[1,2] <- NA
  names(x) <- c("BAIRRO","VVI")
  
  x <- inner_join(x, Bairro_VVI, by="BAIRRO")
  
  x <- x[,c(1,3)]
  colnames(x)[2] <- "VVI"
  
  aux <- x[,2]
  aux <- as.numeric(unlist(aux))
  gini <- Gini(aux)
  
  giniFinal[i,1] <- x[1,1]
  giniFinal[i,2] <- gini
  i <- i + 1
}

rm(aux,Bairro_VVI,x,gini,i,Bairros,IPTU_CEP_BAIRROS)

#adição do ano na tabela do Gini
giniFinal[,3] <- 1995

#Nomeação das colunas
names(giniFinal) <- c("Bairro","Gini","Ano")

#criação de um .csv com os dados do Gini
write.table(giniFinal, file = "/home/tsuneki/Downloads/FAU/git/iptu/planilhas/resultados/1995/Gini1995.csv", sep = ";", dec = ".",row.names = F)

rm(giniFinal)
