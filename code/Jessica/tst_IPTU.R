# teste com os dados IPTU
#importação

library(stringr)
library(dplyr)
library(ineq)

###########################


dt1 <- read.csv2("C:/Users/jessica/Desktop/FAU/Arquivos/IPTU/IPTU2019.csv",header = T, 
                 stringsAsFactors = FALSE, sep=";", dec= ",")

dt2 <- read.csv("C:/Users/jessica/Desktop/FAU/Arquivos/IPTU/IPTU2019.csv",header = T, 
                sep = ";", stringsAsFactors = FALSE, dec= ",")

 dt1$


sapply(bases[[1]], class)

sapply(df, class)

View(dt2018)

class(dt1995$VALOR.DO.M2.DE.CONSTRUCAO)

#tirar a virgula para . para poder colocar a coluna em numerico
dt1995$`VALOR DO M2 DE CONSTRUCAO` <- str_replace_all(dt1995$`VALOR DO M2 DE CONSTRUCAO`,",",".")
#mudança para numerico
dt1995$`VALOR DO M2 DE CONSTRUCAO` <- as.numeric(dt1995$`VALOR DO M2 DE CONSTRUCAO`)


dado <- dt1995 %>% select(ANO.DO.EXERCICIO,BAIRRO.DO.IMOVEL,VALOR.DO.M2.DE.CONSTRUCAO,VALOR.DO.M2.DO.TERRENO,)

#renomear as colunas para ficar mais facil
dado <- dado %>% rename(ano= ANO.DO.EXERCICIO,
                        bairroImovel= BAIRRO.DO.IMOVEL,
                        valorConstrucao= VALOR.DO.M2.DE.CONSTRUCAO,
                        valorTerreno = VALOR.DO.M2.DO.TERRENO)

#calcular gini por bairro
dadoG <- dado %>% select(ano,bairroImovel,valorConstrucao) %>%
  group_by(bairroImovel) %>%
  summarise(ineq(valorConstrucao,type = "Gini"))

View(dado)

# não sei se fiz certo mas estamos tentando fazer de uma forma ou de outra
#preciso de mais rientações com relação a variaveis usar
#mas fazer o agrpamento ficou muito melor que o esperado
#esses dados precisam passar por toda limpesa possivel antes de se trabalhar com eles mas é posive.

# como os arquivos csv são grandes vale a pena trabalahr com big data nisso

dado <- dado %>% select(ano,bairroImovel,valorConstrucao,valorTerreno) %>% 
         mutate(valorConstrucao*valorTerreno)

dado$`valorConstrucao * valorTerreno`

dado <- dado %>% rename(ValorVenal = `valorConstrucao * valorTerreno` )

class(dado$ValorVenal)

giniVenal <- ineq(dado$ValorVenal,type = "Gini")

View(dadoG2)

plot(Lc(dado$ValorVenal),col="darkred",lwd=2)

Linha <- sort(dado$ValorVenal, decreasing = FALSE)



plot(Linha)

dado$ValorVenal

valorAcima <- dado %>% select(ValorVenal) %>%
              filter(ValorVenal > 1000000)

