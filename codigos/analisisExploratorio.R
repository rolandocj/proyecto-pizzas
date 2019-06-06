######           Proyecto 1       #######
##      ESTADISTICA MULTIVARIADA      ##


#Carga de paqueteria
library(ggplot2)
library(corrplot)
library(RCurl)

#Carga de la base de datos
url<-"https://raw.githubusercontent.com/rolandocj/proyecto-pizzas/develop/codigos/preprocesamientoDeDatos.R"
source(url)

#Analisis de Marca
summary(Marca)
Marca
ggplot(pizzas, aes(x=Marca))+
  geom_bar(fill="steelblue")


#Analisis de la variable Id
sort(Id)
length(Id)
max(Id)-min(Id)
#plot(1:length(Id),sort(Id))
ggplot(data = pizzas, 
       aes(x=1:length(Id),y=sort(Id)))+
  geom_point(aes(color=Marca))
#Tres saltos relevantes, checar si estan relacionados
#con alguna otra variable. Puede ser por marca o algún tipo
summary(Id)



#Análisis humedad
head(Humedad)
summary(Humedad)
ggplot(data = as.data.frame(pizzas),
       aes(x=Marca, y=Humedad)) +
  geom_boxplot()


#Analisis de Proteina
summary(Proteina)
ggplot(data = as.data.frame(pizzas),
       aes(x=Marca, y=Proteina)) +
  geom_boxplot()


#Analisis de Grasa
summary(Grasa)
ggplot(data = as.data.frame(pizzas),
       aes(x=Marca, y=Grasa)) +
  geom_boxplot()


#Analisis de Ceniza
summary(Ceniza)
ggplot(data = as.data.frame(pizzas),
       aes(x=Marca, y=Ceniza)) +
  geom_boxplot()+
  geom_hline(yintercept=3)


#Análisis de Sodio
summary(Sodio)
ggplot(data = as.data.frame(pizzas),
       aes(x=Marca, y=Sodio)) +
  geom_boxplot()+
  geom_hline(yintercept=1.2)


#Analisis de Carbohidratos
summary(Carbohidratos)
ggplot(data = as.data.frame(pizzas),
       aes(x=Marca, y=Carbohidratos)) +
  geom_boxplot()

#Analisis de Calorias
summary(Calorias)
ggplot(data = as.data.frame(pizzas),
       aes(x=Marca, y=Calorias)) +
  geom_boxplot()


#######
#CARBOHIDRATOS MAS GRASAS MAS PROTEINAS
ggplot(data=pizzas,
       aes(x=1:length(Id),y=rowSums(pizzas[,c("Proteina","Carbohidratos","Grasa")])))+
  geom_point(aes(color=Marca))+
  ylab("Carbo+prote+grasa")+xlab("obs")
######

#ggplot(data=pizzas,
#       aes(x=1:length(Id),y=rowSums(pizzas[,c("Proteina","Carbohidratos","Grasa")])))+
#  geom_point(aes(color=Marca))+
#  ylab("Carbo+prote+grasa")+xlab("obs")
######

#CALIDAD DE DATOS
mean((Grasa/9)-(Calorias))
mean((Carbohidratos/6)-(Calorias))
mean((Proteina/4)-(Calorias))

#Correlación entre las variables
correlacion<-cor(as.matrix(pizzas[,-c(1,9)]))
corrplot.mixed(correlacion, upper="square",lower="number")

