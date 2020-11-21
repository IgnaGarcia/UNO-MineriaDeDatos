library(readr)
library(tidyverse)
library(reshape2)

#funcion para normalizar nombres de columnas
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+', '_', tolower(names))
  names = make.names(names, unique = TRUE, allow_ = TRUE)
  names = gsub('.', '_', names, fixed = TRUE)
}


### Leer dataset
data <- read_delim("snic-provincias.csv", ";", escape_double = FALSE, 
                   locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
colnames(data) = dbSafeNames(colnames(data))

habitantesArg <- read_csv("habitantesArg.csv", col_names = FALSE)
###


### Procesar datos
str(data)
str(habitantesArg)

# Filtrar por año y por delito de interes
data <- filter(data, data$anio == 2019, 
               data$codigo_delito_snic_id %in% c(1, 2, 5, 10, 11, 13, 14, 15, 19))

# Quedarnos con columnas de valor(prov, delito, casos)
data <- data[,c(3,5,6)]

# Pasar los vos valores de nombre de delito y canditad como columna-valor
data <- dcast(data, provincia_nombre ~ codigo_delito_snic_nombre, 
              value.var = "cantidad_hechos" )

# Unir datos de habitantes con datos criminales
data <- left_join(data, habitantesArg, 
                  by= c("provincia_nombre" = "X1"))

# Convertir la provincia en nombre de columna
data <- data %>% 
  remove_rownames %>% 
  column_to_rownames(var="provincia_nombre")

# Renombrar columnas
colnames(data) <- c("amenazas","homicidios", "tentativas_homicidios", "hurtos", "lesiones", 
                    "abusos/acosos", "contra_libertad", "robos", "violaciones", "totHabitantes")

# Obtener la tasa dividiendo por los habitantes x/c 100.000 habitantes
for (i in 1:24){
  for (j in 1:9){
    data[i,j] = data[i,j] / data[i,10] * 100000
  }
}

# Eliminamos el total de habitantes ya que no nos es mas util
data <- data[,1:9]
###


### Guardamos el archivo para leerlo en el script de clustering
write.csv(data, "tasasCriminales.csv")
###

rm(data, habitantesArg, i, j, dbSafeNames)
