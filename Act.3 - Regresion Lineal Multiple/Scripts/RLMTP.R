##
# Modelo de Regresion lineal Multiple, Actividad 3
# Creado por Grupo B: Benitez, Garcia, Rodriguez, Rechimon
#
# Creado: 24/09/2020 v. 25/09/2020
# Ultima mod: resumen de distribucion de residuos
##

#### Importamos bibliotecas
library(readr)
library(dplyr)
library(corrplot)
library(data.table)
library(leaps)
library(ggplot2)
library(GGally)
library(lmtest)
library(nortest)
library(fmsb)
library(olsrr)


#### LEEMOS EL DATASET
temperaturas <- read_csv("informacion-meteorologica-2012.csv", 
                         col_types = cols(HORA = col_number()))

# Corroboramos que se haya leido bien
head(temperaturas, 5)


##### PREPARAMOS LOS DATOS
data <- temperaturas[, c(2,4:11)] #Variables innecesarias
colnames(data) <- c("Hora", "Velocidad_Viento_MS", "Direc_Viento","Temperatura_C", "Humedad_Relativ", "Presion_hPA", "Pluviometro_MM", "Rad_Solar_WM2", "UV_Rayos" )

summary(data) #Resumen estadistico de los datos

data <- subset(data, data$Temperatura_C!=1802.6 & data$Humedad_Relativ!=255.0) #Outliers


#### Visualizamos las Correlaciones
cor_data <- cor(data[,]) 
corrplot(cor_data) 


#### Creamos el MODELO 1 
## Creamos la formula
variables <- c("Hora","Velocidad_Viento_MS", "Direc_Viento", "Humedad_Relativ", "Presion_hPA", "Pluviometro_MM", "Rad_Solar_WM2", "UV_Rayos" )
y <- "Temperatura_C"  #  variable a predecir

f <- as.formula(paste(y,paste(variables, collapse = " + "),sep = " ~ "))
print(f) #  aqui vemos a todas las variables


## Creamos el Modelo FULL Variables
lm.full <- lm(f, data = data)
summary(lm.full)
print(lm.full)

#par(mfrow=c(2,2),4)
plot(lm.full)
residuos1 <- rstandard(lm.full)
mean(residuos1)
hist(residuos1)


#### Creamos el Modelo NULL Variables
lm.null <- lm(Temperatura_C ~ 1, data = data ) # Intercept-only 
summary(lm.null)

anova(lm.full, lm.null ) #Contrastamos modelos, si p-value es chico podemos partir del null hacia full


#### Aplicamos Metodos de Seleccion de Variables
lm.step.bw   <- step(lm.full, direction = "backward") 

step.fw      <- step(lm.null, 
                     scope = ~ Hora + Velocidad_Viento_MS + Direc_Viento + Humedad_Relativ + Presion_hPA + Pluviometro_MM + Rad_Solar_WM2 + UV_Rayos, 
                     direction = "forward")

lm.step.both <- step(lm.null, 
                     scope = ~ Hora + Velocidad_Viento_MS + Direc_Viento + Humedad_Relativ + Presion_hPA + Pluviometro_MM + Rad_Solar_WM2 + UV_Rayos, 
                     direction = "both")


#### Evaluamos el mejor modelo
regsubsets.out <- regsubsets(f
                             , data = data
                             , nbest = 2    #  cuantos  mejores modelos quiero por cada cantidad de variables
                             , nvmax = 7  # max tama?o del modelo
                             , force.in = NULL, force.out = NULL
                             , method = "exhaustive")
summary(regsubsets.out)

plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")


#### Creamos Modelo 2 a partir de los analisis
m2     = lm(data = data, Temperatura_C ~ Direc_Viento + Humedad_Relativ + Presion_hPA + Rad_Solar_WM2)


#### Analisis de la Bondad del Modelo
#Resumen
summary(m2) 
plot(m2)

#Residuos
residuos2 <- rstandard(m2)
mean(residuos2)
hist(residuos2)

#Mas Resumenes sobre residuos
p_ <- GGally::print_if_interactive
pm <- ggnostic(m2)

p_(pm)

pm <- ggpairs(
  m2, c(".fitted", ".resid"),
  columnLabels = c("fitted", "residuals"),
  lower = list(continuous = ggally_nostic_resid)
)
p_(pm)

## Agrupamiento de las direcciones del viento por patron raro
# library(sqldf)
# sqldf("SELECT Direc_Viento FROM data GROUP BY Direc_Viento")

#Test de Kolmogorov-Smirnov
lillie.test(residuos2) # si  p-value < 0.05 entonces NO hay distribucion normal
#   REVISAR

# Analisis de Colinealidad: Durbin-Watson Test
dwtest(m2) # si p-value < 0.05 entonces hay autocorrelacion
#   REVISAR

VIF(m2) # si p-value > 10 hay colinealidad
ols_vif_tol(m2) # si VIF> 4 hay que investigar si hay colinealidad 

cor_model <- cor(data[, c(3,5,6,8)]) 
corrplot(cor_model) 

# Breusch-Pagan Test:
bptest(m2)  # si  p-value < 0.05 entonces la varianza de los residuos es homocedastica
#   REVISAR

#### Comparacion de Y con Ybonete
ybonete <- predict(m2, data)
plot(x=data$Temperatura_C, y= ybonete)
