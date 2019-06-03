### MODELOS DE REDUCCION DE DIMENSIONES

library(RCurl)
library(ggfortify)
library(ggrepel)

#Carga de la base de datos
url<-"https://raw.githubusercontent.com/rolandocj/proyecto-pizzas/develop/codigos/preprocesamientoDeDatos.R"
source(url)

#variables numericas usadas para la reduccion de dimension
vars.num <- c("Humedad","Proteina","Grasa","Ceniza","Sodio","Carbohidratos","Calorias")

pizzas.pca <- prcomp(pizzas[,vars.num], 
                     center = TRUE, scale = TRUE)

pizzas.pca <- princomp(pizzas[,vars.num], cor = TRUE)
plot(pizzas.pca$x[,1:2])

#screplot
screeplot(pizzas.pca)

#loadings
pizzas.pca$rotation[,1:2]

#varianza acumulada


#grafica sencilla 
ggplot() +
  geom_text(data = data.frame(pizzas.pca$x[,1:2]),
            aes(x = PC1, y = PC2, colour = Marca),
            label = Marca, alpha = 1,
            size = 2, check_overlap = TRUE)

#### Calculo de centroides. Ejecutar sin importar la eleccion por realizar ####
medias.pca <- aggregate(pizzas.pca$x[,1:2], by= list(pizzas$Marca), FUN=mean)

#biplot del PCA
autoplot(pizzas.pca, data = pizzas, alpha = 1, scale = 0,
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 4,
         loadings.label.colour = 'black',
         colour = 'Marca',
         frame = TRUE,
         frame.colour = 'Marca') +
  geom_text_repel(data = medias.pca, 
            aes(x = PC1, y = PC2, label= medias$Group.1),
            alpha = .9,size = 10, show.legend	= FALSE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="PCA con datos normalizados")

library(cluster)
autoplot(clara(pizzas[,vars.num], 4))




#getting the convex hull of each unique point set
library(dplyr)

pizza.PC 

pizzas.PC.cluster <- data.frame(pizzas.pca$x[,1:2], cluster = cluster)

find_hull <- function(df) df[chull(df$PC1, df$PC2),]
hulls <- ddply(pizzas.PC.cluster, "cluster", find_hull)



geom_polygon(data = hulls, alpha = 0.5) +
  labs(x = "Efficiency", y = "Mandate")

plot <- ggplot(data = nomissing, aes(x = eff, y = man, colour=issue, fill = issue)) +
  geom_point() + 
  geom_polygon(data = hulls, alpha = 0.5) +
  labs(x = "Efficiency", y = "Mandate")
plot

autoplot(pizzas.pca, data = data.frame(pizzas, cluster), alpha = 1, scale = 0,
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 4,
         loadings.label.colour = 'black',
         colour = 'cluster',
         frame = TRUE,
         frame.colour = 'cluster') +
  geom_text(aes(), label=Marca, 
            size = 2, vjust=-1,
            check_overlap = TRUE)+
  geom_text_repel(data = medias.pca, 
                  aes(x = PC1, y = PC2, label= medias$Group.1),
                  alpha = .9,size = 10, show.legend	= FALSE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="PCA con datos normalizados")


library(ggplot2)



?fanny
autoplot(fanny(pizzas[,vars.num], 4), frame = TRUE, scale = 0) +
  geom_text(aes(colour = Marca), label=Marca, 
            size = 2, vjust=-1,
            check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="PCA con datos normalizados")



autoplot(kmeans(pizzas[,vars.num], 4), data = pizzas[,vars.num], frame = TRUE, scale = 0) +
  geom_text(aes(colour = Marca), label=Marca, 
            size = 2, vjust=-1,
            check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="PCA con datos normalizados")

pizzas.km <- kmeans(pizzas[,vars.num], 4)
pizzas.km <- kmeans(pizzas.pca$x[,1:2], 4)
cluster <- as.factor(pizzas.km$cluster)

ggplot() +
  geom_text(data = data.frame(pizzas.pca$x[,1:2]),
            aes(x = PC1, y = PC2, colour = cluster),
            label = Marca, alpha = 1,
            size = 2, check_overlap = TRUE)

### ANALISIS FACTORIAL ###
n.factores <- 2
#promax
pizzas.fa <- factanal(x = pizzas[,vars.num],factors = n.factores,scores="Bartlett",
                      rotation = "promax")
#varimax
pizzas.fa <- factanal(x = pizzas[,vars.fa],factors = n.factores,scores="Bartlett")
pizzas.fa

#hacer una tabla con comunalidades y varianzas especificas
(cargas <- pizzas.fa$loadings) # Cargas factoriales
(var.esp <- pizzas.fa$uniquenesses) # Singularidades
(comunalidades <- diag(cargas %*% t(cargas)))


cbind(cargas, var.esp, comunalidades)
pizzas.fa

#aproximapizzas.facion a la matriz de correlacion
pred_vc <- cargas%*%t(cargas) + diag(var.esp)
pizzas.fa$correlation
round(cor(pizzas[,vars.num]) - pred_vc,digits=3)

#grafica de factanal
autoplot(pizzas.fa, data = pizzas, alpha = 0,
         loadings = TRUE, loadings.label = TRUE) +
  geom_text(aes(colour = Marca), label=Marca, 
            size = 2, vjust=-1,
            check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Análisis factorial")


medias.fa <- aggregate(pizzas.fa$scores, by= list(pizzas$Marca), FUN=mean)
medias.fa

autoplot(pizzas.fa, data = pizzas, alpha = 1, scale = 0,
         loadings = TRUE, loadings.label = TRUE,
         colour = 'Marca', frame = TRUE, frame.colour = 'Marca') +
  geom_text_repel(data = medias.fa, 
                  aes(x = Factor1, y = Factor2, label= medias$Group.1),
                  alpha = .9,size = 10, show.legend	= FALSE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Análisis factorial")



  labs(title="PCA con datos normalizados")