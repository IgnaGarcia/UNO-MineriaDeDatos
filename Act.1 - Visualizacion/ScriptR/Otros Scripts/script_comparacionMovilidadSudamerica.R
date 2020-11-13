# graficamos la movilidad  en la ciudad de Buenos Aires
# creado: 2020-04-25   v.  2020-04-27 
# ultima modificacion: 2020-08-24 por Grupo B
# Autor: GAD
# archivo de input se puede bajar de: https://covid19.apple.com/mobility
# clase 2
###############################################
options(allow_html=TRUE)
# Bibliotecas a importar
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
packages_needed <- c("readr", "ggplot2", "plotly", "tidyverse", "data.table")
check_packages(packages_needed)
library(readr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(sqldf)
library(data.table)  #  para  setnames

############    Leer
#mobility_url <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/2007HotfixDev47/v2/en-us/applemobilitytrends-"
#aux        <- paste(mobility_url,"2020-05-03.csv", sep = "")
#mobility   <- read.csv(aux, sep = ",", header = T)
library(readr)
mobility <- read_csv("applemobilitytrends-2020-08-20.csv", 
                                           locale = locale(grouping_mark = ""))

##########   preparar los datos
mobility$region              <- as.factor(mobility$region)
mobility$transportation_type <- as.factor(mobility$transportation_type)
colnames(mobility)
levels(mobility$region)
levels(mobility$transportation_type)

library(tidyr)
datos_v   <- mobility     %>% gather(fecha, tasa    , 7:ncol(mobility))

# Change type of date from Character to Date
datos_v$fecha     <- as.Date(as.character(datos_v$fecha))
colnames(datos_v)

#  Generamos los dataframes de nuestros paises elegidos
trans = "walking"
pais = "Argentina"
pais2 = "Brazil"
pais3 = "Chile"
pais4 = "Uruguay"
datosArgentina <- subset(datos_v, region == pais & transportation_type == trans)
datosBrasil <- subset(datos_v, region == pais2 & transportation_type == trans)
datosChile <- subset(datos_v, region == pais3 & transportation_type == trans)
datosUruguay <- subset(datos_v, region == pais4 & transportation_type == trans)

######  fin de preparacion de los datos



###########   Realizamos el Join de nuestros dataFrames
datosTotal <- sqldf ("select da.fecha, da.tasa as TasaArgentina, db.tasa as TasaBrasil, dc.tasa as TasaChile from datosArgentina da, datosBrasil db, datosChile dc where da.fecha = db.fecha and da.fecha = dc.fecha")

###########   Figura de movilidad
g3 <- ggplot(datosTotal, aes(x = fecha, y = TasaArgentina)) +
  geom_line(aes(y = TasaBrasil), color="darkgreen") +
  geom_line(aes(y = TasaChile), color="darkred") +
  #geom_line(aes(y = TasaUruguay), color="darkred") +
  geom_line(size = 0.6) +
  ggtitle(paste("COVID_19 - Movilidad en ",pais,sep = "")) +
  scale_x_date(date_breaks = "7 day", date_labels =  "%d %b") +
  #scale_y_continuous(limits = c(0, 100), breaks = seq(1, 10, 1)) +
  theme(plot.title = element_text(lineheight = 1,face ='bold'))   +
  ylab("tasa de movilidad") +
  xlab("") +
  labs(caption = "Fuente de los datos: apple.com/covid19/mobility") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) 
g3


