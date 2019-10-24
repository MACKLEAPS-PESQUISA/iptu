startTime <- Sys.time()
library(maptools)
library(dplyr)
library(foreign)

#lendo a tabela LOG2018_CEM_RMSP.dbf
tabela <- read.dbf("/home/tsuneki/Downloads/FAU/LOG2018/LOG2018_CEM_RMSP.dbf", as.is = F)[,c(6,19)]

#criando um dataframe com a tabela LOG2018_CEM_RMSP.dbf
df <- data_frame()
df <- bind_rows(tabela)

#retirando os valores nulos do CEP
tabela <- df[!is.na(df$CEP_E),]
View(tabela)

#lendo a tabela do IPTU de 1995
IPTU1995 <- read.csv2("/home/tsuneki/Downloads/FAU/csv/IPTU_1997.csv", header = T, sep = ";", stringsAsFactors = F, dec = ",", quote = "")[,c(7,18,20,21,22,23,24,25,32)]

View(IPTU1995)

#reformatando os CEPs
for(i in 1:length(tabela[,2])){
  tabela[i, 3] <- paste(substring(tabela[i, 2], c(1,6), c(5,8)), collapse = "-")
}

#retiando a coluna com os CEPs nao formatados
tabela <- tabela[c(1,3)]
names(tabela) <- c("Bairro","CEP.DO.IMOVEL")

View(tabela)

#criando um dataframe com a juncao das tabelas a partir do CEP
tabelaFim95 <- data_frame()
tabelaFim95 <- left_join(IPTU1995, tabela, by = "CEP.DO.IMOVEL")  
tabelaFim95 <- tabelaFim95 %>% distinct()
tabelaFim95 <- tabelaFim95[!is.na(tabelaFim95$Bairro),]

View(tabelaFim95)

#tratando a tabela dos bairros desejados
bairros <- read.csv("/home/tsuneki/Downloads/FAU/csv/Distrito_zonas.csv", sep = ",", header = T)
names(bairros) <- c("CaD","Bairro","Z. Fiscal")
bairros <-bairros[c(2,3)]

View(bairros)

#criando a tabela com os dados e bairros desejados
tabelaFimBairros <- inner_join(tabelaFim95, bairros, by = "Bairro")  

View(tabelaFimBairros)

#criacao de um .csv da tabela tratada
write.csv(tabelaFimBairros, "/home/tsuneki/Downloads/FAU/csv/tabelaFinal.csv")

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
  
  tabelaFimBairros[i,12] <- paste(VVI)
}
#renomeacao das colunas
names(tabelaFimBairros) = c("NOME.DO.CONTRIBUINTE", "CEP.DO.IMOVEL","FRACAO.IDEAL","AREA.DO.TERRENO","AREA.CONSTRUIDA","AREA.OCUPADA","VALOR.DO.M2.DO.TERRENO","VALOR.DO.M2.DE.CONSTRUCAO","FATOR.DE.OBSOLESCENCIA","BAIRRO","Z.FISCAL","VVI")

View(tabelaFimBairros)

write.csv(tabelaFimBairros, "/home/tsuneki/Downloads/FAU/csv/tabelaFinal97.csv")
endTime <- Sys.time()
endTime - startTime
