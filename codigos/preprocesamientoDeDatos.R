#### PREPROCESAMIENTO DE DATOS ####

#Se varga libreria para carga de datos desde github
library(RCurl)

#Carga de la base de datos
url<-"https://raw.githubusercontent.com/rolandocj/proyecto-pizzas/develop/datos/pizzas.csv"
x<-getURL(url)
pizzas<-read.csv(text=x)


#Nombres de las columnas
#names(pizzas)
names(pizzas)<-(c("Id","Humedad","Proteina","Grasa","Ceniza","Sodio","Carbohidratos","Calorias","Marca"))
#Fijamos los nombres como variables
attach(pizzas)
