#Desvolvido por: Jessica Ribas
#Versão 1.0
#testes para entender como calcular indice de gini no R
#seguindo orientação do site r-bloguer

###############################################

#install.packages("ineq")

library(ineq)

#Exemplo do site
AirPassengers

class(AirPassengers)

gini <- ineq(AirPassengers,type = "Gini")

plot(LC(AirPassengers))


plot(Lc(AirPassengers),col="darkred",lwd=2)
