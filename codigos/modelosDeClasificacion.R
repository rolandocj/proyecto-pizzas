### MODELOS DE CLASIFICACION

library(RCurl)
#para LDA
library(MASS)
#para multilogit
library(nnet)

#Carga de la base de datos
url<-"https://raw.githubusercontent.com/rolandocj/proyecto-pizzas/develop/codigos/preprocesamientoDeDatos.R"
source(url)

#variables numericas usadas para clasificar
vars.num <- c("Humedad","Proteina","Grasa","Ceniza","Sodio","Carbohidratos","Calorias")

#variables usadas para lda
set.seed(123)
muestra.train <- sample(1:nrow(pizzas), nrow(pizzas)/2)
muestra.test <- setdiff(1:nrow(pizzas), muestra.train)

#datasets para train y test, considerar usar otra forma de validación
pizzas.train <- data.frame(pizzas[muestra.train,vars.num], Marca = Marca[muestra.train])
pizzas.test <- data.frame(pizzas[muestra.test,vars.num], Marca =  Marca[muestra.test])

#.+.+.+.+.+.+.+   LDA   .+.+.+.+.+.+.+#
pizzas.lda <- lda(Marca ~ ., data = pizzas.train)
summary(pizzas.lda)

pizzas.train.pred.lda <- predict(pizzas.lda, pizzas.train)
pizzas.test.pred.lda <- predict(pizzas.lda, pizzas.test)

#Error en conjunto de entrenamiento
MC.train.lda <- table(pizzas.train$Marca,pizzas.train.pred.lda$class)
MC.train.lda
error.train.lda  <- 1-sum(diag(MC.train.lda))/sum(MC.train.lda)
error.train.lda

#Error en conjunto de prueba
MC.test.lda <- table(pizzas.test$Marca,pizzas.test.pred.lda$class)
MC.test.lda
error.test.lda  <- 1-sum(diag(MC.test.lda))/sum(MC.test.lda)
error.test.lda

#.+.+.+.+.+.+.+   MULTILOGIT   .+.+.+.+.+.+.+#
pizzas.log <- multinom(Marca ~ ., data = pizzas.train, MaxNWts = 1500)
pizzas.log
summary(pizzas.log)
#confint(pizzas.log)

pizzas.train.pred.log <- predict(pizzas.log, pizzas.train)
pizzas.test.pred.log <- predict(pizzas.log, pizzas.test)

#Error de entrenamiento
MC.train.log <- table(pizzas.train$Marca,pizzas.train.pred.log)
MC.train.log
error.train.log  <- 1-sum(diag(MC.train.log))/sum(MC.train.log)
error.train.log

#Error de prueba
MC.test.log <- table(pizzas.test$Marca,pizzas.test.pred.log)
MC.test.log
error.test.log  <- 1-sum(diag(MC.test.log))/sum(MC.test.log)
error.test.log
