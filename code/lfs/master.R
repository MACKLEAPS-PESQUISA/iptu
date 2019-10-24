library(maptools)
library(dplyr)
library(foreign)


tabela <- read.dbf("/home/tsuneki/Downloads/FAU/LOG2018/LOG2018_CEM_RMSP.dbf", as.is = F)[,c(6,19)]

df <- data_frame()
df <- bind_rows(tabela)


tabela <- df[!is.na(df$CEP_E),]
View(tabela)

IPTU1995 <- read.csv2("/home/tsuneki/Downloads/FAU/csv/IPTU_1995.csv", header = T, sep = ";", stringsAsFactors = F, dec = ",", quote = "")[,c(18,21,22,24,25)]

View(IPTU1995)

for(i in 1:length(tabela[,2])){
  tabela[i, 3] <- paste(substring(tabela[i, 2], c(1,6), c(5,8)), collapse = "-")
}

tabela <- tabela[c(1,3)]


names(tabela) <- c("Bairro","CEP.DO.IMOVEL")

View(tabela)



# TESTES TESTES TESTES
editando <- tabela
View(editando)

# tabela sem repetções - pesquisar comando UNIQUE() e comando DUPLICATED()
tabela_unica <- data.frame(unique(editando$c('Bairro','CEP.DO.MOVEL')))
View(tabela_unica)
#table(tabela$coluna) para ver quantas vezes se repete

# tentando com duplicated -> https://pt.stackoverflow.com/questions/91898/eliminando-linhas-duplas-de-um-data-frame
df <- editando
tabela_unica_final <- data.frame(duplicated(editando))
View(tabela_unica_final)
tab <- df[!tabela_unica_final]
View(tab)

# testenando coisas 

  # (opção merge para fundir as duas tabelas)



tabelaFim95 = merge(IPTU1995,tabela, by = "CEP.DO.IMOVEL")  
View(tabelaFim)






# Problemas: 

  # Eliminar as repetiçoes de cep tanto em tabela quanto em IPTU
  # Alguns ceps existem na tabela e não existem no iptu, e vice-versa

IPTU2018 <- read.csv2("/home/tsuneki/Downloads/FAU/csv/IPTU_2018.csv", header = T, sep = ";", stringsAsFactors = F, dec = ",", quote = "")[,c(7,18,21,22,24,25)]
tabelaFim18 = merge(IPTU2018,tabela, by = "CEP.DO.IMOVEL")  

View(tabelaFim18)
