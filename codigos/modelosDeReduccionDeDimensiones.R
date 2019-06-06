### MODELOS DE REDUCCION DE DIMENSIONES Y CLUSTERING

library(RCurl)
library(ggfortify)
library(ggrepel)
#para determinar numero optimo de clusters
library(factoextra)
#para clustering jerarquico
library(cluster)
library(dendextend)
library(xtable)

#Carga de la base de datos
url<-"https://raw.githubusercontent.com/rolandocj/proyecto-pizzas/develop/codigos/preprocesamientoDeDatos.R"
source(url)

#variables numericas usadas para la reduccion de dimension
vars.num <- c("Humedad","Proteina","Grasa","Ceniza","Sodio","Carbohidratos","Calorias")


#.+.+.+.+.+.+.+   FUNCIONES   .+.+.+.+.+.+.+#
#funcion para graficas sobre pca
pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,1))
  plot(x.pvar,xlab="Componente principal", ylab="Proporción de varianza explicada", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Componente principal", ylab="Proporcion de varianza explicada acumulada", ylim=c(0,1), type='b')
  par(mfrow=c(1,1))
}

#.+.+.+.+.+.+.+   PCA   .+.+.+.+.+.+.+#

pizzas.pca <- prcomp(pizzas[,vars.num], 
                     center = TRUE, scale = TRUE)

#loadings
pizzas.pca$rotation[,1:2]
#varianza acumulada

#graficas de varianza de pca
pcaCharts(pizzas.pca)

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

#agregar prueba de esfericidad 

#promax
pizzas.fa <- factanal(x = pizzas[,vars.num],factors = n.factores,scores="Bartlett",
                      rotation = "promax")
#varimax
pizzas.fa <- factanal(x = pizzas[,vars.num],factors = n.factores,scores="Bartlett")
pizzas.fa

#hacer una tabla con comunalidades y varianzas especificas
(cargas <- pizzas.fa$loadings) # Cargas factoriales
(var.esp <- pizzas.fa$uniquenesses) # Singularidades
(comunalidades <- diag(cargas %*% t(cargas)))

#resumen
cbind(cargas, var.esp, comunalidades)

#hacer prueba para el numero de factores
#el determinante es muy pequenio, como afecta esto a la prueba de hipotesis
det(cor(pizzas[,vars.num]))

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
###########
fviz_nbclust
fviz_nbclust(pizzas[,vars.num], kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")


fviz_nbclust(pizzas[,vars.num], hcut, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")
?fviz_nbclust

# Silhouette method
fviz_nbclust(pizzas[,vars.num], hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")
##########
?NbClust
nbc <- NbClust(data = pizzas[,vars.num], diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 7, method = c("kmeans"))

nbc2 <- NbClust(data = pizzas[,vars.num], diss = NULL, distance = "euclidean", 
        min.nc = 3, max.nc = 6, method = "complete", index = "alllong")

nbc$Best.nc
nbc$Best.nc[1,]
hist(nbc$Best.nc[1,])
hist(nbc$Best.nc)
cluster <- as.factor(nbc$Best.partition)

?NbClust
#### Clustering  ####
#################################
### Clustering jerarquico ###

k <- 5
X <- pizzas[,vars.num]
d <- dist(X, method = "euclidean")

#complete
hclus1 <- agnes(d,diss=TRUE,method="complete")
dend1 <- as.dendrogram(hclus1,hang=-1)
cluster.j1 <- as.factor(cutree(dend1, k))
plot(X, col = cluster.j1, main = "Clustering jerárquico (complete)")
cluster <- cluster.j1
colores <- rainbow(12)

colors.dend <- colores[as.numeric(pizzas$Marca)]
cols <- colors.dend[order.dendrogram(dend1)]
labels_colors(dend1) <- as.numeric(cols)

d1 <- color_branches(dend1,k=k,groupLabels=FALSE)
plot(d1,main=paste("Single (euclidean)\n",paste("k=",k)))

#average
hclus2<- agnes(d,diss=TRUE,method="average")
dend2 <- as.dendrogram(hclus2,hang=-1)
cluster.j2 <- as.factor(cutree(dend2, k))
plot(X, col = cluster.j2, main = "Clustering jerárquico (average)")

#single
hclus3 <- agnes(d,diss=TRUE,method="single")
dend3 <- as.dendrogram(hclus3,hang=-1)
cluster.j3 <- as.factor(cutree(dend3, k))
plot(X, col = cluster.j3, main = "Clustering jerárquico (single)")

################################
#la variable "cluster" tiene el etiquetado de los datos
#y es el que se usa para la representacion grafica en 2d


n.clusters <- 5

#kmeans
pizzas.km <- kmeans(pizzas[,vars.num], n.clusters)
cluster <- as.factor(pizzas.km$cluster)

?kmeans
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
