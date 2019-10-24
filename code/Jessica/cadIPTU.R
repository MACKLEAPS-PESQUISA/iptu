#
#
# importação de todos os arquivos CSV para fazer uma unica base de dados
#Assim pode-se calcular a quantidade de residencias cadastradas, indice de gini
#e demais estatisticas por ano em toda  base de dados
#

library(dplyr)
library(ineq)
library(stringr)
library(ggplot2)


################################

# caminho par ao seu diretório de trabalho no caso os arquivos a serem importados
setwd("C:/Users/jessica/Desktop/FAU/Arquivos/IPTU") 

# caminho para o diretório dos arquivos  e extensao deles
arquivos <- list.files(path = "C:/Users/jessica/Desktop/FAU/Arquivos/IPTU", 
                       pattern = "*.csv") 



# função lapply para importar todos os arquivos usando a função read.cvs2 
# ou read.cvs
bases <- lapply(arquivos, function(x){
  b <- read.csv2(x, header = TRUE, sep = ";",stringsAsFactors = FALSE, dec = ",", quote = "")
  names(b) <- c("numContribuinte", "anoExercicio","numdaNL","dtCadastramento",
                "tipoContribuinte1","CPF_CNPJ_Contr1","nomeContr1",
                "tipoContribuinte2","CPF_CNPJ_Contr2","nomeContr2",
                "numCondominio","codLogImovel","logradouroImovel","numImovel",
                "complementoImovel","BairroImovel","refImovel","cepImovel",
                "qtdEsquinaFrente","FracaoIdeal","areaTerreno","areaConstruida",
                "areaOcupada","valM2Terreno","valM2Construcao","AnoConstrucaoCorrigido",
                "qtdPavimentos","testadaCalculo","tipoUsoImovel","tipoPadraoConstrucao",
                "tipoTerreno","fatorObselencia","anoVidaContribuinte",
                "mesInicioVidaContribuinte","faseContribuinte")
  b
})



# a saida da função foi uma lista em que cada elemento é um "arquivo"
#agora precisa colcoar tudo num data frame só.

memory.limit(size=180000)
df <- data.frame()

for (i in 1:as.numeric(length(bases))) {
   #para ter cert3eza que o campo ano não estara com sujeira
    
    df <- rbind(df,bases[[i]])
    
}

dado <- df %>% select(anoExercicio,nomeContr1,BairroImovel,logradouroImovel,cepImovel,
                         valM2Terreno,valM2Construcao,
                      areaConstruida,areaOcupada,areaTerreno) %>%
#precisa indicae que a função mutate é da biblioteca dplyr e não da plyr
                  dplyr::mutate(PGVI = ifelse(areaOcupada != 0,
                                          areaOcupada*valM2Construcao,
                                         ifelse(areaOcupada == 0 & areaConstruida != 0,
                                           areaConstruida*valM2Construcao,
                                         ifelse(areaOcupada == 0 & areaConstruida == 0, 
                                         areaTerreno*valM2Terreno,0))),
                                valAbsRep = (areaTerreno * valM2Terreno) + (areaConstruida*valM2Construcao))
    
#PGVI = 
# area contrução * valor de construção ||
#area terreno * valor de terreno ||
#areaOcupada * valor de construção
df$testadaCalculo[df$testadaCalculo!=0]

dado2 <- df %>% select(anoExercicio,nomeContr1,BairroImovel,logradouroImovel,cepImovel,
                      valM2Terreno,valM2Construcao,numImovel,
                      areaConstruida,areaOcupada,areaTerreno,testadaCalculo,fatorObselencia) %>%
                 filter(cepImovel %in% c('04510-000') & numImovel== 71) %>%
  #precisa indicae que a função mutate é da biblioteca dplyr e não da plyr
  dplyr::mutate(PGVI = ifelse(areaOcupada != 0,
                              areaOcupada*valM2Construcao,
                              ifelse(areaOcupada == 0 & areaConstruida != 0,
                                     areaConstruida*valM2Construcao,
                                     ifelse(areaOcupada == 0 & areaConstruida == 0, 
                                            areaTerreno*valM2Terreno,0))),
                valAbsRep = (areaTerreno * valM2Terreno) + (areaConstruida*valM2Construcao))

dado2 <- dado2  %>% select(anoExercicio,nomeContr1,BairroImovel,logradouroImovel,cepImovel,
                       valM2Terreno,valM2Construcao,numImovel,
                       areaConstruida,areaOcupada,areaTerreno,testadaCalculo,PGVI,valAbsRep) %>%
                       dplyr::mutate(PGVItestada = PGVI*testadaCalculo)
              
#calculando indice de gini por ano
dadoG <- dado %>% select(anoExercicio,PGVI,valAbsRep) %>%
                   group_by(anoExercicio) %>%
                   dplyr::summarise(GiniPGVI=ineq(PGVI,type = "Gini"),
                                    mediaPGVI = mean(PGVI),
                                    medianaPGVI = median(PGVI),
                                    somaPGVI = sum(PGVI),
                                    GiniAbsRep =ineq(valAbsRep,type = "Gini"), 
                                    mediaAbsRep = mean(valAbsRep),
                                    medianaAbsRep = median(valAbsRep),
                                    somaAbsRep = sum(valAbsRep),
                                    qtd = n())

setwd("C:/Users/jessica/Desktop/FAU/") 
write.csv2(dadoG, "Gini17-19.csv", row.names = FALSE)

             
#grafico


gGiniPGVI <- ggplot(Gini,aes(x= anoExercicio, y = GiniPGVI)) +
         geom_line()+geom_point()

gGiniAbsRep <- ggplot(Gini,aes(x= anoExercicio, y = GiniAbsRep)) +
  geom_line()+geom_point()


n = sum(Gini$qtd)
soma = sum(qtdpercent)

posicao = x
qtdpercent <- (Gini$qtd*100)/n
  
gQtde <- ggplot(Gini, aes(x= anoExercicio, y = qtdpercent,fill = anoExercicio)) +
         geom_bar(stat = "identity")


Gini$mediaPGVI

#valor inicial gini$mediaPGVI[1]
#valor final   gini$mediaPGVI[2]
#aumento = valor final-valor inicial
#perc = (aumento /valor inicial)*100

#outra forma é dividir o valor fina pelo inicial e multiplicar por 100
# oerc = (valor final /valor inicial) * 100
#depois subtraia o calor encontrado por 100
#perc = perc-100

valoracaoMediaPGVI <- c()
tst <- c()
for(i in 2:as.numeric(length(Gini$mediaPGVI))){
  valoracaoMediaPGVI [i-1] <-(Gini$mediaPGVI[i] -Gini$mediaPGVI[i-1])/Gini$mediaPGVI[i-1]*100
  tst[i-1] <- ((Gini$mediaPGVI[i]/Gini$mediaPGVI[i-1])*100) - 100 
  }


valoracaoMedianaPGVI <- c()
for(i in 2:as.numeric(length(Gini$medianaPGVI))){
  valoracaoMedianaPGVI [i-1] <-(Gini$medianaPGVI[i] -Gini$medianaPGVI[i-1])/Gini$medianaPGVI[i-1]*100
}

valoracaoMediaAbsRep<- c()
for(i in 2:as.numeric(length(Gini$mediaAbsRep))){
  valoracaoMediaAbsRep [i-1] <-(Gini$mediaAbsRep[i] -Gini$mediaAbsRep[i-1])/Gini$mediaAbsRep[i-1]*100
}

valoracaoMedianaAbsRep<- c()
for(i in 2:as.numeric(length(Gini$medianaAbsRep))){
  valoracaoMedianaAbsRep [i-1] <-(Gini$medianaAbsRep[i] -Gini$medianaAbsRep[i-1])/Gini$medianaAbsRep[i-1]*100
}

valoracaoSomaPgvi<- c()
for(i in 2:as.numeric(length(Gini$somaPGVI))){
  valoracaoSomaPgvi [i-1] <-(Gini$somaPGVI[i] -Gini$somaPGVI[i-1])/Gini$somaPGVI[i-1]*100
}

valoracaoSomaAbsRep<- c()
for(i in 2:as.numeric(length(Gini$somaAbsRep))){
  valoracaoSomaAbsRep [i-1] <-(Gini$somaAbsRep[i] -Gini$somaAbsRep[i-1])/Gini$somaAbsRep[i-1]*100
}

valoracaoMediaPGVI <- valoracaoMediaPGVI[-25]

valMediaPGVI <- ggplot(Gini[-1,],aes(x= anoExercicio, y=valoracaoMediaPGVI))+
                geom_line()+geom_point()

valMedianaPGVI <- ggplot(Gini[-1,],aes(x= anoExercicio, y=valoracaoMedianaPGVI))+
  geom_line()+geom_point()


valMediaAbsRep <- ggplot(Gini[-1,],aes(x= anoExercicio, y=valoracaoMediaAbsRep))+
  geom_line()+geom_point()

valMedianaPAbsRep <- ggplot(Gini[-1,],aes(x= anoExercicio, y=valoracaoMedianaAbsRep))+
  geom_line()+geom_point()

valsomaPgvi <- ggplot(Gini[-1,],aes(x= anoExercicio, y=valoracaoSomaPgvi))+
  geom_line()+geom_point()

valsomaAbsrep <- ggplot(Gini[-1,],aes(x= anoExercicio, y=valoracaoSomaAbsRep))+
  geom_line()+geom_point()



