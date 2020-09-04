base <- Base_depto
dim(base)
summary(base)

library("VIM")
#check dos dados missing da base
is.na(base)
sum(is.na(base))
aggr(base)

library(ggplot2)
library(tidyverse) 
library(cluster)    
library(factoextra) 

#não utilizaremos a coluna "data" para o cluster
df <- base[c(2:18)]

#colocamos os dados na mesma escala
df <- scale(df)

#testamos dividir os dados em 2, 3, 4 e 5 grupos
k2 <- kmeans(df, centers = 2, nstart = 25)
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

#plotagem dos dados
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

#para visualizar todos os graficos de uma vez
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#escolhemos o método para indicar o número ideal de clusters
library(NbClust)
fviz_nbclust(df, kmeans, method = 'silhouette')

#plotamos o número de clusters indicado no metodo "silhouette"
set.seed(123)
final <- kmeans(df, 3, nstart = 25)
print(final)
fviz_cluster(final, data = df)

#criamos uma lista para receber os dados do cluster final
lista <- k3$cluster

#incluímos a coluna "cluster" no data frame
library(dplyr)
base %>%
  mutate(cluster = lista)

lista
