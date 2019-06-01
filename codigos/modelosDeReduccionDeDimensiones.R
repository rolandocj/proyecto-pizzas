### MODELOS DE REDUCCION DE DIMENSIONES

library(RCurl)
library(ggfortify)

#Carga de la base de datos
url<-"https://raw.githubusercontent.com/rolandocj/proyecto-pizzas/develop/codigos/preprocesamientoDeDatos.R"
source(url)

#variables numericas usadas para la reduccion de dimension
vars.num <- c("Humedad","Proteina","Grasa","Ceniza","Sodio","Carbohidratos","Calorias")

pizzas.pca <- prcomp(pizzas[,vars.num], 
                     center = TRUE, scale = TRUE)

#screplot
screeplot(pizzas.pca)

#loadings
pizzas.pca$rotation[,1:2]

#grafica sencilla 
ggplot() +
  geom_text(data = data.frame(pizzas.pca$x[,1:2]),
            aes(x = PC1, y = PC2, colour = Marca),
            label = Marca, alpha = 1,
            size = 2, check_overlap = TRUE)

#biplot del PCA
autoplot(pizzas.pca, data = pizzas, alpha = 0,
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,
         loadings.label.colour = 'black') +
  geom_text(aes(colour = Marca), label=Marca, 
            size = 2, vjust=-1,
            check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="PCA con datos normalizados")

### ANALISIS FACTORIAL ###
n.factores <- 2
#promax
pizzas.fa <- factanal(x = pizzas[,vars.num],factors = n.factores,scores="Bartlett",
                      rotation = "promax")
#varimax
pizzas.fa <- factanal(x = pizzas[,vars.fa],factors = n.factores,scores="Bartlett")
pizzas.fa

#hacer una tabla con comunalidades y varianzas especificas
(cargas <-pizzas.fa$loadings) # Cargas factoriales
(var.esp <-pizzas.fa$uniquenesses) # Singularidades
(comunalidades <- diag(cargas %*% t(cargas)))


#aproximacion a la matriz de correlacion
pred_vc <- cargas%*%t(cargas) + diag(var.esp)
round(cor(pizzas[,vars.num]) - pred_vc,digits=3)

#grafica de factanal
autoplot(pizzas.fa, pizzas = state.x77, alpha = 0,
         loadings = TRUE, loadings.label = TRUE) +
  geom_text(aes(colour = Marca), label=Marca, 
            size = 2, vjust=-1,
            check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Análisis factorial")
