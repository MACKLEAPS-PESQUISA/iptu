arquivos2 <- list.files(path = "C:/Users/jessica/Desktop/FAU/Arquivos/IPTU/sep", 
                        pattern = "*.csv") 
setwd("C:/Users/jessica/Desktop/FAU/Arquivos/IPTU/sep") 

bases2 <- lapply(arquivos2, function(x){
  b <- read.csv2(x, header = TRUE, sep = ",",stringsAsFactors = FALSE)
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

df <- bases2[[1]]
class(df$areaTerreno)

df$valM2Construcao[df$valM2Construcao == 0]

df$valM2Terreno[df$valM2Terreno==0]

class(df$valM2Terreno)



# Desconfio que seja o metro quadrado do terreno e o valor de metro quadrado construido
