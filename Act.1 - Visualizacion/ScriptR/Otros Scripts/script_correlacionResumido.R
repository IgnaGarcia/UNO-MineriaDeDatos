# Corrleacion entre Casos Diarios Relativos y Movilidad urbana en Argentina vs Chile vs Brasil

# creado: 2020-08-23   v.  2020-8-28 
# ultima modificacion: se hicieron los resumenes y se terminaron las vistas

# Autor: GAD, Ignacio Garcia, Pablo Rechimon, Miguel Rodriguez, Nicolas Benitez

###############################################



##################### IMPORTAR BIBLIOTECAS
check_packages <- function(packages) {
  if (all(packages %in% rownames(installed.packages()))) {
    TRUE
  } else{
    cat(
      "Instalar los siguientes packages antes de ejecutar el presente script\n",
      packages[!(packages %in% rownames(installed.packages()))],
      "\n"
    )
  }
}
packages_needed <- c("ggplot2", "plotly", "sqldf", "lubridate", 
                     "tidyr", "data.table", "readr", "dplyr", "RColorBrewer" )
check_packages(packages_needed)

library(gsubfn)
library(proto)
library(RSQLite)
library(sqldf)
library(data.table)
library(lubridate)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(dplyr)
library(RColorBrewer)

options(scipen = 6) #para evitar notacion cientifica



##################### LEER DATOS DEL COVID

# URL con datos del COVID-19
URL          <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
url_archivo  <- paste(URL,"time_series_covid19_confirmed_global.csv", sep = "")
COVID_19_h   <- read.csv(url_archivo, sep = ",", header = T)

#Analizamos lo leido y corroboramos que se leyo bien
View(COVID_19_h)



##################### PREPARAR SUBSET DE DATOS PARA ARGENTINA, BRASIL y CHILE

COVID_19_h <- select(COVID_19_h, -c(Lat, Long, Province.State)) #Eliminar cols
setnames(COVID_19_h, "Country.Region", "pais") #Renombrar cols
COVID_19 <- COVID_19_h %>% gather(fecha, acumCasos, 2:ncol(COVID_19_h)) #Parse a formato vertical

COVID_19$fecha <- as.Date(COVID_19$fecha, format = "X%m.%d.%y") #Formatear fecha

#Preparar subset de Arg, Br y Chl
ARG_target = "Argentina"
C19_ARG <- subset(COVID_19, pais == ARG_target)

BR_target = "Brazil"
C19_BR <- subset(COVID_19, pais == BR_target)

CHL_target = "Chile"
C19_CHL <- subset(COVID_19, pais == CHL_target)

#Join entre subsets
C19_Sudamerica <- sqldf("
   SELECT arg.fecha fecha, arg.acumCasos acumCasosARG, 
      br.acumCasos acumCasosBR, chl.acumCasos acumCasosCHL
   FROM C19_ARG arg, C19_BR br, C19_CHL chl
   WHERE arg.fecha==br.fecha AND arg.fecha==chl.fecha")

#Visualizar resultado a traves de las primeras y ultimas 5 cols
head(C19_Sudamerica, 5)
tail(C19_Sudamerica, 5)



##################### LEER DATOS DE HABITANTES DEL MUNDO
data_habitantes <- read.csv("C:/work/Explotacion/Actividad1/habitantesMundo.csv", 
                            sep=";", stringsAsFactors=TRUE)



#################### PREPARAR DATOS DE HABITANTES
#Subset de ARG, BR y CHL
SUD_habitantes <- subset(data_habitantes, pais == ARG_target | 
                           pais == BR_target | 
                           pais == CHL_target)

SUD_habitantes <- select(SUD_habitantes, -c(Variant, Index, Notes, codigo, Type)) #Eliminar cols

#Visualizar habitantes del subset
SUD_habitantes



##################### CALCULAR CASOS DIARIOS EN RELACION A LOS HABITANTES
#Agregar columnas para los casos
C19_Sudamerica$casosARG <- 0
C19_Sudamerica$casosBR <- 0
C19_Sudamerica$casosCHL <- 0

#Contador y tope
i <- 1
max <- nrow(C19_Sudamerica)

#Recorrer y calcular casos diarios de cada 10.000 habitantes
while (i<=max) {
  if(C19_Sudamerica$acumCasosARG[i] > 0){
    C19_Sudamerica$casosARG[i] <- (C19_Sudamerica$acumCasosARG[i] - C19_Sudamerica$acumCasosARG[i-1]) * 10000 / SUD_habitantes$cantidad[1]
  }
  if(C19_Sudamerica$acumCasosBR[i] > 0){
    C19_Sudamerica$casosBR[i] <- (C19_Sudamerica$acumCasosBR[i] - C19_Sudamerica$acumCasosBR[i-1]) * 10000 / SUD_habitantes$cantidad[2]
  }
  if(C19_Sudamerica$acumCasosCHL[i] > 0){
    C19_Sudamerica$casosCHL[i] <- (C19_Sudamerica$acumCasosCHL[i] - C19_Sudamerica$acumCasosCHL[i-1]) * 10000 / SUD_habitantes$cantidad[3]
  }
  i <- i+1
}

#Visualizar resultado a traves de las primeras y ultimas 5 cols
head(C19_Sudamerica, 5)
tail(C19_Sudamerica, 5)



##################### LEER DATOS DE MOVILIDAD
data_movilidad_h <- read_csv("applemobilitytrends-2020-08-26.csv", locale = locale(grouping_mark = ""))



##################### PREPARAR DATOS
setnames(data_movilidad_h, "region", "pais") #Renombrar cols
setnames(data_movilidad_h, "transportation_type", "transporte")
data_movilidad_h <- select(data_movilidad_h, -c(geo_type, alternative_name, country, 'sub-region'))
data_movilidad_h$pais <- as.factor(data_movilidad_h$pais) #Parse a factor del pais
data_movilidad_h$transporte <- as.factor(data_movilidad_h$transporte) #Parse a factor del tipo de transporte

#Pasar a formato vertical
movilidad <- data_movilidad_h %>% gather(fecha, tasa, 3:ncol(data_movilidad_h))

movilidad$fecha <- as.Date(as.character(movilidad$fecha)) #Formatear fecha

#Subsets de ARG, BR y CHL
MOV_ARG <- subset(movilidad, transporte == "walking" & pais == ARG_target)
MOV_BR <- subset(movilidad, transporte == "walking" & pais == BR_target)
MOV_CHL <- subset(movilidad, transporte == "walking" & pais == CHL_target)

#Join entre subsets
MOV_Sudamerica <- sqldf("
  SELECT arg.fecha fecha, arg.tasa tasaARG, br.tasa tasaBR, chl.tasa tasaCHL
  FROM MOV_ARG arg, MOV_BR br, MOV_CHL chl
  WHERE arg.fecha == br.fecha AND arg.fecha == chl.fecha")

#Visualizar resultado a traves de las primeras y ultimas 5 cols
head(MOV_Sudamerica, 5)
tail(MOV_Sudamerica, 5)



##################### JOIN ENTRE MOVILIDAD Y CASOS
SUD_MovC19 <- sqldf("
  SELECT mov.fecha fecha, c19.casosARG, mov.tasaARG, c19.casosBR, mov.tasaBR, c19.casosCHL, mov.tasaCHL
  FROM C19_Sudamerica c19, MOV_Sudamerica mov
  WHERE mov.fecha == c19.fecha")

#Visualizar resultado a traves de las primeras y ultimas 5 cols
head(SUD_MovC19, 5)
tail(SUD_MovC19, 5)

#Calculamos y analizamos medidas estadisticas
sd(SUD_MovC19$casosARG, na.rm = T)
sd(SUD_MovC19$tasaARG, na.rm = T)
sd(SUD_MovC19$casosBR, na.rm = T)
sd(SUD_MovC19$tasaBR, na.rm = T)
sd(SUD_MovC19$casosCHL, na.rm = T)
sd(SUD_MovC19$tasaCHL, na.rm = T)

summary(SUD_MovC19) #Minimos, Maximos, Quartiles y Promedio


##################### GENERAR FIGURAS
#Vector de colores
colores <- c("blue", "lightblue", "yellow", "orange", "red", "darkred")


#Time Series de Movilidad en Arg, Br y Chl con plotly
fig2 <- plot_ly(SUD_MovC19,  x = ~fecha, y = ~tasaARG, 
                name = 'Argentina', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~tasaBR, name = 'Brasil', mode = 'lines') %>%
  add_trace(y = ~tasaCHL, name = 'Chile', mode = 'lines') %>%
  layout(title = "Time Series de Movilidad",
         xaxis = list(title = "Fecha", type = "date",
                      tickmode = "linear", tick0 = min(SUD_MovC19$fecha),
                      tickformat = "%d/%m", dtick = 86400*10000, tickangle = 75),
         yaxis = list (title = "Tasa de Movilidad", tickangle = -45))

fig2


#Time Series de Movilidad en Arg, con color en relacion a cantidad de casos con plotly
fig3 <- qplot(data = SUD_MovC19, x = fecha, y = tasaARG, colour = casosARG, geom = "line") +
  geom_line(size = 1) +
  scale_colour_gradient2(low = "lightblue", mid="orange", high = "red", 
                         midpoint = max(SUD_MovC19$casosARG)/2) +
  ggtitle("Time Series de Movilidad con aumento de Casos") +
  scale_x_date(date_breaks = "7 day", date_labels =  "%d/%m") +
  theme(plot.title = element_text(lineheight = 1,face ='bold'))   +
  ylab("Tasa de Movilidad") +
  xlab("") +
  labs(caption = "Fuente de los datos: apple.com/covid19/mobility") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) 
fig3


#Filtramos puntos antes de que llegue el primer caso
filtered_SUD_MovC19 <- sqldf("SELECT * FROM SUD_MovC19 
                     WHERE casosARG > 0 AND casosBR > 0 AND casosCHL > 0")


#Correlacion entre Movilidad y Casos en Arg
fig5 <- plot_ly(data = filtered_SUD_MovC19,  x = ~casosARG, y = ~tasaARG, name = "Argentina",
                type = 'scatter', mode = 'markers') %>%  
  layout(title = "Dispercion entre Tasa de Movilidad y Casos",
         xaxis = list(title = "Casos c/10.000 hab"),
         yaxis = list (title = "Tasa de Movilidad", tickangle = -45))
fig5


#Correlacion entre Movilidad y Casos en Arg, Br y Chl
fig6 <- plot_ly(data = filtered_SUD_MovC19,  x = ~casosARG, y = ~tasaARG, name = "Argentina",
                type = 'scatter', mode = 'markers') %>%
  add_trace(x = ~casosBR, y = ~tasaBR, name = 'Brasil', mode = 'markers') %>%
  add_trace(x = ~casosCHL, y = ~tasaCHL, name = 'Chile', mode = 'markers') %>%
  layout(title = "Dispercion entre Tasa de Movilidad y Casos",
         xaxis = list(title = "Casos c/10.000 hab"),
         yaxis = list (title = "Tasa de Movilidad", tickangle = -45))
fig6


#Correlacion entre Movilidad y Casos en Arg, Br y Chl filtrando tasa mayor a 100 y casos mayor a 4
fig7 <- plot_ly(data = filtered_SUD_MovC19,  x = ~casosARG, y = ~tasaARG, name = "Argentina",
                type = 'scatter', mode = 'markers') %>%
  add_trace(x = ~casosBR, y = ~tasaBR, name = 'Brasil', mode = 'markers') %>%
  add_trace(x = ~casosCHL, y = ~tasaCHL, name = 'Chile', mode = 'markers') %>%
  layout(title = "Dispercion entre Tasa de Movilidad y Casos",
         xaxis = list(title = "Casos c/10.000 hab", range= c(0, 4)),
         yaxis = list (title = "Tasa de Movilidad", tickangle = -45, range= c(0, 100)))
fig7


#Time Series de casos de Covid-19 en Arg, Br y Chl
fig8 <- plot_ly(data = filtered_SUD_MovC19,  x = ~fecha, y = ~casosARG, name = "Argentina",
                type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~casosBR, name = 'Brasil', mode = 'lines') %>%
  add_trace(y = ~casosCHL, name = 'Chile', mode = 'lines') %>%
  layout(title = "Time Series de Confirmados c/10.000 habitantes",
         xaxis = list(title = "Fecha", type = "date",
                      tickmode = "linear", tick0 = min(filtered_SUD_MovC19$fecha),
                      tickformat = "%d/%m", dtick = 86400*10000, tickangle = 75),
         yaxis = list (title = "Nro de Casos", tickangle = -45))
fig8
