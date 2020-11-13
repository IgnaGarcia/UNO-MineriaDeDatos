##############################################################
# Analisis de Componentes Principales del dataset de Ozono   #
#                                                            #
# Creado: 2020-09-06   v. 2020-09-10                         #
# Ultima Mod: plots                                          #
#                                                            #
# Grupo B: GAD, Benitez, Garcia, Rechimon, Rodriguez         #
#                                                            #
##############################################################

##### IMPORTAMOS LAS BIBLIOTECAS A USAR
library(readxl)
library(corrplot)
library(PerformanceAnalytics)
library(psych)
library(rela)
library(dplyr)
library(ggplot2)
library(plotly)

options(scipen = 6)



###### LEEMOS EL DATASET
data <- read_excel("ozono.xls", na = "?")

# Corroboramos que se haya leido bien
head(data, 5)

# Visualizamos el numero de variables y de casos
ncol(data)
nrow(data)



##### PREPARACION DE LOS DATOS
# Visualizamos el numero de casos con null
sapply(data, function(x) sum(is.na(x)))

#--- Preferimos mantener variables y eliminar casos NA ---#
data <- na.omit(data)

sapply(data, function(x) sum(is.na(x)))
nrow(data)

# Hacemos un resumen de los datos
summary(data)


# Matriz de correlaciones
cor_data <- cor(data[, -1]) 

#--- Azul cuando es cor+ y Rojo para cor-
corrplot(cor_data) 

#chart.Correlation(data[, -1]) # para graficar correlciones y histogramas



###
# Test de barlett
cortest.bartlett(cor_data, n= 1847)

# Test de KMO
KMO(data[, -1])

# Grafico para ver con cuantas CP nos quedamos
scree(data[, -1])


# Componentes Principales
cp <- prcomp(data[,-1], scale = TRUE)

#Resumen de los Componentes Principales
summary(cp)
names(cp)
cp$center  # Media de cada variable
cp$scale   #  Desviacion estandard de cada variable
cp$sdev    # Varianza de cada componente
#cp$rotation 
#cp$x


# para graficar:
biplot(x = cp, scale = 0, cex = 0.6, col = c("grey", "brown3"))

biplot(x = cp, scale = 0, cex = 0.6, xlabs=rep(".", nrow(data)),col = c("grey", "brown3"))

pc1 <- cp[[2]][,1]
pc2 <- cp[[2]][,2]
pc3 <- cp[[2]][,3]
pc4 <- cp[[2]][,4]
pc5 <- cp[[2]][,5]
pc6 <- cp[[2]][,6]
pc7 <- cp[[2]][,7]

pc <- data.frame(cbind(pc1, pc2, pc3, pc4, pc5, pc6, pc7))

rm(pc1, pc2, pc3, pc4, pc5, pc6, pc7)

fig <- plot_ly(x = ~pc[1,], y = ~pc$pc1, name = "PC1", type = "bar", text = rownames(pc), textangle=-90,  textposition='auto')%>% 
  layout(
    title = "PC1",
    xaxis = list(title = "Variables", showticklabels=FALSE),
    yaxis = list(title = "Valores")
  )
fig

fig1 <- plot_ly(x = ~pc[2,], y = ~pc$pc2, name = "PC2", type = "bar", text = rownames(pc), textangle=-90,  textposition='auto')%>% 
  layout(
    title = "PC2",
    xaxis = list(title = "Variables", showticklabels=FALSE),
    yaxis = list(title = "Valores")
  )
fig1

fig2 <- plot_ly(x = ~pc[2,], y = ~pc$pc3, name = "PC3", type = "bar", text = rownames(pc), textangle=-90,  textposition='auto')%>% 
  layout(
    title = "PC3",
    xaxis = list(title = "Variables", showticklabels=FALSE),
    yaxis = list(title = "Valores")
  )
fig2
