inicio <- Sys.time()

library(raster)
library(ggplot2)
library(corrplot)
library(devtools)
install_github("JoshOBrien/gdalUtilities", force = T)

brick_5_2019 <- brick('./imagenes/noruega/2020-09-10-00_00_2020-09-10-23_59_Sentinel-2_L2A_Custom_script.tiff')
rast <- brick_5_2019
names(rast)

names(rast)<- c("red", "green", "blue", "NIR")
nir      <-  rast$NIR # Infrarojo cercano
red      <-  rast$red     
green    <-  rast$green
blue     <-  rast$blue

rast$ndvi     <-  (nir-red)/(nir+red) 
rast$ndwi     <-  (green - nir) / (green + nir)

sum(sapply(data, function(x) sum(is.na(x))))
data <- na.omit(rast)
summary(data)
rm(nir, red, green, blue )
names(rast)

# Matriz de correlaciones
cor_data <- cor(data[,]) 

#--- Azul cuando es cor+ y Rojo para cor-
corrplot(cor_data) 


## visualizamos el ndvi
rasterVis::gplot(rast$ndvi) +
  geom_tile(aes(fill = (value)))  +
  viridis::scale_fill_viridis(direction = -1, na.value='#FFFFFF00') +
  theme_bw() +
  labs(title = 'Indice NDVI')

# ## visualizamos el ndwi
rasterVis::gplot(rast$ndwi) +
  geom_tile(aes(fill = (value)))  +
  viridis::scale_fill_viridis(direction = -1, na.value='#FFFFFF00') +
  theme_bw() +
  labs(title = 'Indice NDWI')

## visualizamos la banda Red
rasterVis::gplot(rast$Red) +
  geom_tile(aes(fill = (value)))  +
  viridis::scale_fill_viridis(direction = -1, na.value='#FFFFFF00') +
  theme_bw() +
  labs(title = 'Banda Red')

# ## visualizamos la banda  Green
rasterVis::gplot(rast$Green) +
  geom_tile(aes(fill = (value)))  +
  viridis::scale_fill_viridis(direction = -1, na.value='#FFFFFF00') +
  theme_bw() +
  labs(title = 'Banda Green')

## visualizamos la banda Blue
rasterVis::gplot(rast$Blue) +
  geom_tile(aes(fill = (value)))  +
  viridis::scale_fill_viridis(direction = -1, na.value='#FFFFFF00') +
  theme_bw() +
  labs(title = 'Banda Blue')

# ## visualizamos la banda NIR
rasterVis::gplot(rast$ndwi) +
  geom_tile(aes(fill = (value)))  +
  viridis::scale_fill_viridis(direction = -1, na.value='#FFFFFF00') +
  theme_bw() +
  labs(title = 'Banda NIR')
fin   <- Sys.time()
print(fin - inicio) # 4 min sin el for 

########################
########################
GDALinfo('./Data/gec-acqId0000007388-a-sm4-0000000000-vv-h')
# rows  6524      
# columns 16384              
# dim <- rows  * columns