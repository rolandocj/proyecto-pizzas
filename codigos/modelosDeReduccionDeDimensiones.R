### MODELOS DE REDUCCION DE DIMENSIONES

library(RCurl)
library(ggfortify)
library(ggrepel)
library(cluster)

#Carga de la base de datos
url<-"https://raw.githubusercontent.com/rolandocj/proyecto-pizzas/develop/codigos/preprocesamientoDeDatos.R"
source(url)

#variables numericas usadas para la reduccion de dimension
vars.num <- c("Humedad","Proteina","Grasa","Ceniza","Sodio","Carbohidratos","Calorias")


#.+.+.+.+.+.+.+   PCA   .+.+.+.+.+.+.+#

pizzas.pca <- prcomp(pizzas[,vars.num], 
                     center = TRUE, scale = TRUE)

#pizzas.pca <- princomp(pizzas[,vars.num], cor = TRUE)
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

#### Calculo de centroides ####
medias.pca <- aggregate(pizzas.pca$x[,1:2], by= list(pizzas$Marca), FUN=mean)

#biplot del PCA
autoplot(pizzas.pca, data = pizzas, alpha = 1, scale = 0,
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 4,
         loadings.label.colour = 'black',
         colour = 'Marca',
         frame = TRUE, frame.colour = 'Marca') +
  geom_text_repel(data = medias.pca, 
            aes(x = PC1, y = PC2, label= medias.pca$Group.1),
            alpha = .9,size = 10, show.legend	= FALSE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="PCA con datos normalizados")


#.+.+.+.+.+.+.+   ANALISIS FACTORIAL   .+.+.+.+.+.+.+#
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

#### Calculo de centroides ####
medias.fa <- aggregate(pizzas.fa$scores, by= list(pizzas$Marca), FUN=mean)

#grafica de factanal
autoplot(pizzas.fa, data = pizzas, alpha = 1, scale = 0,
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 4,
         loadings.label.colour = 'black',
         colour = 'Marca',
         frame = TRUE, frame.colour = 'Marca') +
  geom_text_repel(data = medias.fa, 
                  aes(x = Factor1, y = Factor2, label= medias.fa$Group.1),
                  alpha = .9,size = 10, show.legend	= FALSE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Análisis factorial")


#.+.+.+.+.+.+.+   CLUSTERING   .+.+.+.+.+.+.+#


#### Seleccion de numero de clusters  ####
## usando fpc
library(fpc)
?pamk
pamk.best <- pamk(pizzas[,vars.num])
pamk.best
plot(pam(pizzas[,vars.num], pamk.best$nc))

## usando NbClust
#install.packages("NbClust")
library("NbClust")
?NbClust
nbc <- NbClust(data = pizzas[,vars.num], diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 7, method = c("kmeans"))
cluster <- as.factor(nbc$Best.partition)


#### Clustering  ####
#la variable "cluster" tiene el etiquetado de los datos
#y es el que se usa para la representacion grafica en 2d


n.clusters <- 5

#kmeans
pizzas.km <- kmeans(pizzas[,vars.num], n.clusters)
cluster <- as.factor(pizzas.km$cluster)

#fuzzy means
pizzas.km <- fanny(pizzas[,vars.num], n.clusters)
cluster <- as.factor(pizzas.km$clustering)

#### Visualizacion  ####

#visualzacion del clustering en 2 componentes principales
autoplot(pizzas.pca, data = data.frame(pizzas, cluster), alpha = 1, scale = 0,
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 4,
         loadings.label.colour = 'black',
         colour = 'cluster',
         frame = TRUE, frame.colour = 'cluster') +
  geom_text_repel(data = medias.pca, 
                  aes(x = PC1, y = PC2, label= medias.pca$Group.1),
                  alpha = .9,size = 10, show.legend	= FALSE) +
  geom_text(aes(), label=Marca, 
            size = 2, vjust=-1,
            check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Clustering", subtitle = "Componentes principales")

#visualzacion del clustering en 2 factores
autoplot(pizzas.fa, data = data.frame(pizzas, cluster), alpha = 1, scale = 0,
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 4,
         loadings.label.colour = 'black',
         colour = 'cluster',
         frame = TRUE, frame.colour = 'cluster') +
  geom_text_repel(data = medias.fa, 
                  aes(x = Factor1, y = Factor2, label= medias.fa$Group.1),
                  alpha = .9,size = 10, show.legend	= FALSE) +
  geom_text(aes(), label=Marca, 
            size = 2, vjust=-1,
            check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Clustering", subtitle = "Dos factores")
