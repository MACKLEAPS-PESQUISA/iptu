library(maptools)
library(dplyr)
library(foreign)


tabela <- read.dbf("/home/tsuneki/Downloads/FAU/LOG2018/LOG2018_CEM_RMSP.dbf", as.is = F)[,c(6,19)]

df <- data_frame()
df <- bind_rows(tabela)

tabela <- df[!is.na(df$CEP_E),]
View(tabela)

IPTU1995 <- read.csv2("/home/tsuneki/Downloads/FAU/csv/IPTU_1995.csv", header = T, sep = ";", stringsAsFactors = F, dec = ",", quote = "")[,c(7,18,21,22,24,25)]

View(IPTU1995)

for(i in 1:length(tabela[,2])){
  tabela[i, 3] <- paste(substring(tabela[i, 2], c(1,6), c(5,8)), collapse = "-")
}

tabela <- tabela[c(1,3)]
names(tabela) <- c("Bairro","CEP.DO.IMOVEL")

View(tabela)

tabelaFim95 <- data_frame()
tabelaFim95 <- left_join(IPTU1995, tabela, by = "CEP.DO.IMOVEL")  
tabelaFim95 <- tabelaFim95 %>% distinct()
tabelaFim95 <- tabelaFim95[!is.na(tabelaFim95$Bairro),]

View(tabelaFim95)

bairros <- read.csv("/home/tsuneki/Downloads/FAU/csv/Distrito_zonas.csv", sep = ",", header = T)
names(bairros) <- c("CaD","Bairro","Z. Fiscal")
bairros <-bairros[2]

View(bairros)

tabelaFimBairros <- inner_join(tabelaFim95, bairros, by = "Bairro")  

View(tabelaFimBairros)

write.csv(tabelaFimBairros, "/home/tsuneki/Downloads/FAU/csv/tabelaFinal.csv")
