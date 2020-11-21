#################
#  Agrupamiento de provincias a partir de estadisticas criminales en Argentina
#
#  Grupo B: Benitez, Garcia, Rodriguez, Rechimon
#  fecha de creacion: 12/11/2020
#  actualizacion: 16/11/2020 - comentarios
#
##################

### Bibliotecas
library(readr)
library(tidyverse) # select, pipes, gather
library(ggplot2)
library(corrplot) # corrplot
library(psych)
library(plotly)
library(dplyr) 
library(cluster) 
library(factoextra) 
library(pheatmap) # dendograma doble con mapa de calor
library(clValid) # comparar metodos de agrupamiento
###

### Leer data
# Tasa: casos cada 100k de habitantes por provincia
tasasCriminales <- read_csv("tasasCriminales.csv")
head(tasasCriminales)
###

### Procesamiento de datos
# X1 como rownames
tasasCriminales <- tasasCriminales %>% 
  remove_rownames %>% 
  column_to_rownames(var="X1")

# Visualizamos 
X11()
boxplot(tasasCriminales, 
        names = colnames(tasasCriminales), las=2,
        xlab = "Provincias", ylab = "Tasas") 

# Correlaciones
corrplot.mixed(cor(tasasCriminales), tl.pos = "lt")

# Estandarizamos las variables
scaled.tasas <- as.data.frame(scale(tasasCriminales))

# Analisis de Componentes Principales
KMO(tasasCriminales) # Mayor es mejor, va de 0 a 1, msa = medida de adecuacion del muestreo
###

### Clustering de Provincias
# Calculo de distancias
# metodos:
# "euclidean", "maximum", "manhattan", "canberra", "binary", 
# "pearson", "spearman", "kendall", "minkowski"
dist.eucl <- dist(scaled.tasas, method = "euclidean")
fviz_dist(dist.eucl)

dist.maximum <- dist(scaled.tasas, method = "maximum")
fviz_dist(dist.maximum)

dist.manh <- dist(scaled.tasas, method = "manhattan")
fviz_dist(dist.manh)

dist.mink <- get_dist(scaled.tasas, method = "minkowski")
fviz_dist(dist.mink)

# Numero optimo de clusters
fviz_nbclust(scaled.tasas, kmeans, method = "wss") # 3 < k < 6
fviz_nbclust(scaled.tasas, kmeans, method = "silhouette") # 3 o 5

# Numero de clusters y metodo de agrupamiento optimo
comparacion <- clValid(
  obj        = scaled.tasas,
  nClust     = 3:6, # en promedio grupos de a 4 provincias
  clMethods  = c("hierarchical", "kmeans", "pam"),
  validation = c("stability", "internal"))
summary(comparacion)

# Guardamos los grupos optimos
k <- 5

set.seed(124) # experimentos replicables

# Clustering jerarquico
# metodos:
# "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), 
# "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
hc.res <- hclust(dist.eucl, method = "ward.D2")
fviz_dend(hc.res, cex = 0.6, k, palette = "jco", horiz = F)

# k-means clustering
km.res <- kmeans(scaled.tasas, k, nstart = 25)
fviz_cluster(km.res, data = scaled.tasas, palette = "jco",
             ggtheme = theme_minimal())

# pam clustering
pam.res <- pam(x = scaled.tasas, k = k, metric = "euclid")
fviz_cluster(pam.res, data = scaled.tasas, palette = "jco",
             ggtheme = theme_minimal())


# heatmap
pheatmap(mat = scaled.tasas, scale = "none",
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "ward.D2",
         cutree_rows = k, fontsize = 8)
###

### Guardar Datos
# Asignar variable de clase y de provincia
tasasCriminales$clase <- as.factor(cutree(hc.res, k = 5))
write.csv(tasasCriminales, "tasasCriminalesClasificadas.csv")

# Pasar a formato vertical
data_long <- gather(tasasCriminales, crimen, tasa, 1:9, factor_key=TRUE)

# Plotear grupos
acp <- prcomp(tasasCriminales[,1:9])
princomp <- as.data.frame(acp$x)
princomp$clase <- tasasCriminales[,10]

p <- plot_ly(x=princomp$PC1,y=princomp$PC2,text=rownames(princomp),
             mode="markers",color = princomp$clase,marker=list(size=11))
p <- layout(p,title="PCA Clusters from Hierachical Clustering",
            xaxis=list(title="PC1"),
            yaxis=list(title="PC2"))
p


ggplot(data_long, aes(x = crimen, y = tasa, group=clase, colour = clase)) + 
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")

###

### Clustering de Crimenes
# Transpuesta para agrupar por crimenes
transpuesta <- as.data.frame(t(scaled.tasas))

# Calculo de distancias
dist.eucl2 <- dist(transpuesta, method = "euclidean")

# Guardamos los grupos optimos
k2 <- 4

set.seed(200) # experimentos replicables

# Clustering jerarquico
hc.res2 <- hclust(dist.eucl2, method = "ward.D2")
fviz_dend(hc.res2, cex = 0.6, k2, palette = "jco", horiz = T)

# k-means clustering
km.res2 <- kmeans(transpuesta, k2, nstart = 25)
fviz_cluster(km.res2, data = transpuesta, palette = "jco",
             ggtheme = theme_minimal())

# pam clustering
pam.res2 <- pam(x = transpuesta, k = k2, metric = "euclid")
fviz_cluster(pam.res2, data = transpuesta, palette = "jco",
             ggtheme = theme_minimal())
###

