# Completar
# 2020-08-31  version actual:   2020-08-31
# Pablo Hernan Rechimon, Ignacio Garcia
# aprendemos a leer desde una URL, setnames, select, 
# group_by(), summarise_all(), la funcion sapply

library(reshape2)
library(data.table)

############################ leer los datos

# Dataset Covid Mundial.

#URL          <- "https://covid.ourworldindata.org/data/"
#URL_csv   <- paste(URL,"owid-covid-data.csv", sep = "")
#COVID_19 <- read.csv(URL_csv, sep = ",", header = T)
#View(COVID_19)

# Dataset Covid Argentina.
URL          <- "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/"
URL_csv   <- paste(URL,"Covid19Casos.csv", sep = "")
COVID_19_Arg <- read.csv(URL_csv, sep = ",", header = T)
View(COVID_19_Arg)


################################### preparo los datos
COVID_19_Arg <- select(COVID_19_Arg, -c(,"sepi_apertura", "origen_financiamiento", "fecha_diagnostico", "ultima_actualizacion" ,"fecha_apertura",  "fecha_internacion" ,"fecha_cui_intensivo",  "fecha_cui_intensivo" ,  "carga_provincia_id",  "clasificacion" ,   "residencia_provincia_id"  , "residencia_departamento_id" )   )
colnames(COVID_19_Arg)
setnames(COVID_19_Arg, "residencia_provincia_nombre", "provincia")
setnames(COVID_19_Arg, "residencia_pais_nombre", "pais")
setnames(COVID_19_Arg, "residencia_departamento_nombre", "localidad")
setnames(COVID_19_Arg, "carga_provincia_nombre", "carga_provincia")
setnames(COVID_19_Arg, "edad_años_meses", "medida")
setnames(COVID_19_Arg, "id_evento_caso", "id")
setnames(COVID_19_Arg, "sexo", "genero")


# eliminamos  columnas no usadas
# library(dplyr) Built-in
library(sqldf)
library(RSQLite)
library(dplyr) 
library(readr)
datos <- COVID_19_Arg  %>%  
  mutate(fallecido  = case_when(.$fallecido == "NO" ~ 0,
                                   .$fallecido == "SI" ~ 1,))
datos <- datos  %>%  
  mutate(cuidado_intensivo  = case_when(.$cuidado_intensivo == "NO" ~ 0,
                                .$cuidado_intensivo == "SI" ~ 1))

datos <- datos  %>%  
  mutate(asistencia_respiratoria_mecanica  = case_when(.$asistencia_respiratoria_mecanica == "NO" ~ 0,
                                          .$asistencia_respiratoria_mecanica == "SI" ~ 1))

datos <- datos  %>%  
  mutate(medida  = case_when(.$medida == "Años" ~ .$edad,
                             .$medida == "Meses" ~ .$edad/12))

View(datos)

str(datos)
datos$genero <- as.factor(datos$genero)

