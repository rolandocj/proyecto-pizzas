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
muestra.train <- sample(1:nrow(pizzas), nrow(pizzas)*(1/2))
muestra.test <- setdiff(1:nrow(pizzas), muestra.train)

table(pizzas$Marca[muestra.train])
table(pizzas$Marca[muestra.test])
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
xtable(MC.test.lda)
error.test.lda  <- 1-sum(diag(MC.test.lda))/sum(MC.test.lda)
error.test.lda

#.+.+.+.+.+.+.+   MULTILOGIT   .+.+.+.+.+.+.+#
<<<<<<< HEAD
pizzas.log <- multinom(Marca ~ ., data = pizzas, MaxNWts = 1500)
=======
pizzas.log <- multinom(Marca ~ ., data = pizzas.train, MaxNWts = 1500)
pizzas.log
>>>>>>> a6dae224afcdea58ae6408a746c4c153273dce9f
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






#CROSS VALIDATION
library(caret)
set.seed(27)
#CONTROL DE CV, 5 FOLDS
control<-trainControl(method = "cv",number = 5)
#REDES
rn<-train(pizzas[,-9],as.factor(as.matrix(Marca)),
          method="nnet",trControl=control)
max(rn$results$Accuracy)
#Multinom reg
mr<-train(pizzas[,-9],as.factor(as.matrix(Marca)),
          method="multinom",trControl=control)
max(mr$results$Accuracy)
#LDA
lda<-train(pizzas[,-9],as.factor(as.matrix(Marca)),
           method="lda",trControl=control)
max(lda$results$Accuracy)
#RANDOM FOREST
rf<-train(pizzas[,-9],as.factor(as.matrix(Marca)),
          method="rf",trControl=control)
max(rf$results$Accuracy)
#Arboles
tree<-train(pizzas[,-9],as.factor(as.matrix(Marca)),
          method="ctree",trControl=control)
max(tree$results$Accuracy)

