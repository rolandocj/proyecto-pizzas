###### REGRESION MULTIVARIADA

#Carga de la base de datos
url<-"https://raw.githubusercontent.com/rolandocj/proyecto-pizzas/develop/codigos/preprocesamientoDeDatos.R"
source(url)

#####  MANOVA
#plantea si existe diferencia significativa
#entre las variables entre grupos
manova<-manova(as.matrix(pizzas[,-c(1,9)])~Marca,data=pizzas)
#Primero checamos si individualmente las medias son iguales
#que es lo que plantea H0
summary.aov(manova)
#Se rechaza que en cualquier variable las medias son iguales
#ahora, todas las variables
summary(manova)
# Se rechaza que todas las marcas tienen medias iguales.

JG<-c(which(Marca=="J"),which(Marca=="G"))
MARCA_JG<-c(rep("J",length(which(Marca=="J"))),
          rep("G",length(which(Marca=="G"))))
PIZ_JG<-cbind(pizzas[JG,-c(1,9)],MARCA_JG)
manova_JG<-manova(as.matrix(pizzas[JG,-c(1,9)])~MARCA_JG,data=PIZ_JG)
summary.aov(manova)
summary(manova)



#### REGRESION MULTIVARIADA
names(pizzas)
nutrientes<-pizzas[,c("Proteina","Grasa","Sodio","Carbohidratos")]
nonutrientes<-pizzas[,c("Humedad","Ceniza","Calorias")]

nonutrientes<-as.matrix(nonutrientes)
nutrientes<-as.matrix(nutrientes)

reg_mult<-lm(nonutrientes~nutrientes)
summary(reg_mult)
