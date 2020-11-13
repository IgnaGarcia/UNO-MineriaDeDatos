###############
# Time Series
# Actividad 5
#
# Creator : Group B - Benitez, Garcia, Rodriguez, Rechimon
# 
# Create date: 2020/10/29
# Update date: 2020/11/6 
#        comment: comentarios
###############

##----------------START LIBRARIES
library(readxl)
library(ggfortify)
library(scales)
library(forecast)
library(tseries)

library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(earth)
library(zoo) # Para trabajar cuartos como date

library(rsample)
##----------------END LIBRARIES


##----------------START READ DATA
datos <- read_excel("SINF_POCUP_0810.xlsx", skip = 1)
##----------------END READ DATA


##----------------START PROCESS DATA
datos <- na.omit(datos)
head(datos, 5)

# Transformamos los datos en una serie temporal 
cadenas.ts  <- ts(datos[, 2:4], start = c(2008,1), frequency = 4)

cadena_1.ts <- ts(datos[17:50, 2],   start = c(2012,1), frequency = 4)
cadena_2.ts <- ts(datos[17:50, 3],   start = c(2012,1), frequency = 4)
cadena_3.ts <- ts(datos[, 4],   start = c(2008,1), frequency = 4)


# Plots
# Serie Temporal de las 3 cadenas juntas y por separado
autoplot(cadenas.ts, xlab = "Periodo", ylab = "Tasa")

# Tomamos desde 2012 hasta la fecha
autoplot(cadena_1.ts, colour = "blue", linetype = "dashed")
autoplot(cadena_2.ts, colour = "blue", linetype = "dashed")

autoplot(cadena_3.ts, colour = "blue", linetype = "dashed")


# Descomponemos las series
desc.cadena_1 <- decompose(cadena_1.ts)
plot(desc.cadena_1, xlab ='Periodo')

desc.cadena_2  = decompose(cadena_2.ts)
plot(desc.cadena_2, xlab ='Periodo')

desc.cadena_3  = decompose(cadena_3.ts)
plot(desc.cadena_3, xlab ='Periodo')


# obtener la tendencia y la componente estacional
desc.cadena = decompose(cadenas.ts)

sTrend   = desc.cadena$trend
sSeasonal= desc.cadena$seasonal
ts.plot(cbind(sTrend,sTrend + sSeasonal),xlab='Tendencia + componente estacional', lty=2:1)

# Numero estimado de diferencias requeridas para realizar una ST estacionaria
# Prueba de raiz unitaria para identificar si la serie es estacionaria
ndiffs(cadena_1.ts)  ## [1] 1 desde 2008 a la fecha | 0 desde 2012 a la fecha
nsdiffs(cadena_1.ts) ## [1] 1 desde 2008 a la fecha | 1 desde 2012 a la fecha

ndiffs(cadena_2.ts)  ## [1] 1 desde 2008 a la fecha | 0 desde 2012 a la fecha
nsdiffs(cadena_2.ts) ## [1] 1 desde 2008 a la fecha | 1 desde 2012 a la fecha

ndiffs(cadena_3.ts)  ## [1] 1 
nsdiffs(cadena_3.ts) ## [1] 0

# Remueve la tendencia y compara el ruido con la estacionalidad
diff.cadena_1.ts <- autoplot(diff(cadena_1.ts))
diff.cadena_1.ts
boxplot(diff(cadena_1.ts) ~ cycle(diff(cadena_1.ts)), xlab = "Periodo", ylab = "Diferencias")

diff.cadena_2.ts<-autoplot(diff(cadena_2.ts))
diff.cadena_2.ts
boxplot(diff(cadena_2.ts) ~ cycle(diff(cadena_2.ts)), xlab = "Periodo", ylab = "Diferencias")

diff.cadena_3.ts<-autoplot(diff(cadena_3.ts))
diff.cadena_3.ts
boxplot(diff(cadena_3.ts) ~ cycle(diff(cadena_3.ts)), xlab = "Periodo", ylab = "Diferencias")

# Autocorrelation Function:
# Visualizamos que tan correlacionado se encuentra el último valor con el pasado (LAG)
# Si la serie es estacional, la autocorrelación sera muy alta para retrasos multiplos
# de la frecuencia
autoplot(acf(cadena_1.ts, plot = FALSE))
autoplot(acf(diff(cadena_1.ts), plot = FALSE))

autoplot(acf(cadena_2.ts, plot = FALSE))
autoplot(acf(diff(cadena_2.ts), plot = FALSE))

autoplot(acf(cadena_3.ts, plot = FALSE))
autoplot(acf(diff(cadena_3.ts), plot = FALSE))

# Partial ACF 
# .Permite visualizar cuando determinados retrasos (LAG) son buenos para el modelo,
# útiles para data estacional
# .Remueve la dependencia de lags en otros lags utilizando la correlación de los 
# residuos

autoplot(acf(cadena_1.ts, plot = FALSE))
autoplot(pacf(diff(cadena_1.ts), plot = FALSE))
autoplot(acf(cadena_2.ts, plot = FALSE))
autoplot(pacf(diff(cadena_2.ts), plot = FALSE))
autoplot(acf(cadena_3.ts, plot = FALSE))
autoplot(pacf(diff(cadena_3.ts), plot = FALSE))

# Se agrega Lag determinado por el ACF para ver nuevamente la serie sin tendencia
diff.cadena_1.ts.4<-diff(cadena_1.ts, lag = 4)
autoplot(diff.cadena_1.ts.4, ts.colour = "red")
##
sd(diff(cadena_1.ts, lag = 4))
summary(diff(cadena_1.ts, lag = 4))
##

diff.cadena_2.ts.4<-diff(cadena_2.ts, lag = 4)
autoplot(diff.cadena_2.ts.4, ts.colour = "red")

diff.cadena_3.ts.4<-diff(cadena_3.ts, lag = 4)
autoplot(diff.cadena_3.ts.4, ts.colour = "red")


##----------------START TESTS TS
# Test ADF
# Ho: Existe raiz unitaria, no es estacionaria
# Ha: No existe raiz unitaria, es estacionaria
adf.test(diff.cadena_1.ts.4)
## 0.08002   No se puede rechazar la hipotesis nula

adf.test(diff.cadena_2.ts.4)
## 0.08938   No se puede rechazar la hipotesis nula

adf.test(diff.cadena_3.ts.4)
## 0.1646    No se puede rechazar la hipotesis nula.

# Test KPSS
# Ho: Es estacionaria alrededor de una tendencia determinista 
# Ha: Existe raiz unitaria, no es estacionaria
kpss1<-kpss.test(diff.cadena_1.ts.4)
kpss1$p.value
# 0.01    Se rechaza la hipotesis nula

kpss2<-kpss.test(diff.cadena_2.ts.4)
kpss2$p.value
# 0.01    Se rechaza la hipotesis nula

kpss3<-kpss.test(diff.cadena_3.ts.4)
kpss3$p.value
## 0.1    Se acepta la hipotesis nula
##----------------END TESTS TS
##----------------END PROCESS DATA


##----------------START ARIMA MODEL
##-- CREATE MODEL
# (X,X,X) Parte no Estacional (X,X,X) Parte estacional del modelo [X] Frecuencia
modelArima <- auto.arima(cadena_1.ts, stepwise = FALSE, approximation = FALSE)

modelArima
# ARIMA(1,0,0)(2,1,0)[4] con tendencia, y que arroja un AIC=-160.39

##-- TEST MODEL
# Ho: Los datos se distribuyen de forma independiente, las correlaciones 
# en la población de la que se toma la muestra son 0.
# Ha: Los datos no se distribuyen de forma independiente.
# Test de Box-Pierce
Box.test(modelArima$residuals) # 0.966   Ho

# Test de Ljung-Box
Box.test(modelArima$residuals, type="Ljung-Box") # 0.9645  Ho

# Ho: Hay distribucion normal en los residuos,  la asimetría y el exceso de curtosis 
# son nulos (asimetría = 0 y curtosis = 3)
# Test de Jarque-Bera
jarque.bera.test(modelArima$residuals) # 0.8863 Ho

# Ho: La distribución de residuos no es normal.
# Test de Shapiro-Wilk
shapiro.test(modelArima$residuals) # 0.5031 Ho

##-- MAKE PREDICTION
forecast <- forecast(modelArima, level = c(95), h = 4)
autoplot(forecast)
ggtsdiag(modelArima)
autoplot(acf(forecast$residuals, plot = FALSE))
autoplot(pacf(forecast$residuals, plot = FALSE))

forecast$method
forecast$fitted
forecast$residuals

y_cadena_1 <- as.data.frame(forecast$mean)
y_cadena_1$x
##----------------END ARIMA MODEL


##----------------START MODELS
# Modifico los nombres para trabajar con el script facilmente
colnames(datos) <- c("date", "value", "value2", "value3")

data <- datos[1:2]
data <- data[17:50,]
# Para utilizar graficos dinamicos de plotly en ggplot
interactive <- TRUE

# Trabajar los periodos como date
# data$date <- as.yearqtr(data$date, format = "Q%q/%y")
data$date <- as.Date(as.yearqtr(data$date, format = "Q%q/%y"), frac = 1)
class(data$date)

# Visualizamos la serie temporal de forma dinamica
data %>%
  plot_time_series(date, value, .interactive = interactive)

data %>%
  tk_tbl(rename_index = 2008) %>%
  plot_acf_diagnostics(
    .date_var = date,
    .value    = value,
    .lags     = 20,
    .show_white_noise_bars = TRUE,
    .interactive = FALSE
  )

##-- STEP 1: SPLIT DATA
splits <- initial_time_split(data, prop = 0.8)

##-- STEP 2: BUILD MODELS
# 1- ARIMA REG
model.arimaReg <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(value ~ date, data = training(splits))
model.arimaReg
# ARIMA(0,1,0)(1,1,0)[4]  AIC=-123.45   AICc=-122.82   BIC=-121.27

# 2- ARIMA BOOST
model.arimaBoost <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(value ~ date , data = training(splits))
model.arimaBoost
# ARIMA(0,1,0)(1,1,0)[4] AIC=-123.45   AICc=-122.82   BIC=-121.27

# 3- ETS
model.ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(value ~ date, data = training(splits))
model.ets
# AIC -133.7518  AICc -127.8571   BIC -124.6810

# 4- PROPHET
model.prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(value ~ date, data = training(splits))

# 5- LINEAL REGRESSION
model.lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ year(date), data = training(splits))

# 6- MARS
model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth") 
recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num) %>%
  step_rm(date)

model.mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))

##-- STEP 3: CREATE MODEL TABLE
models_tbl <- modeltime_table(
  model.arimaReg,
  model.arimaBoost,
  model.ets,
  model.prophet,
  model.lm,
  model.mars
)
models_tbl

##-- STEP 4: CALIBRATION
# Calibramos para determinar los intervalos de confianza y metricas de 
# precision. Estos son predicciones y residuales que se calculan a partir de los
# datos de testing
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

##-- STEP 5: TEST MODELS
# Visualizamos: Forecast vs Test Dataset
# Evaluamos la prueba. Nivel de precisión
calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = data
  ) %>%
  plot_modeltime_forecast(
    .interactive = interactive
  )

# Probar modeltime_accuracy para recolectar metricas comunes
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )

# MAE Media de error absouto
# MAPE Porcentaje de Media de error absoluto
# MASE Media de error absoluto a escala
# SMAPE Media de error absoluto simetrica
# RMSE Raiz media de error cuadratico
# RSQ error cuadratico


# STEP 6: REFIT WITH FULL DATA
# Paso 6: Reajustar al dataset completo y preveer el alcance
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data)
refit_tbl %>%
  modeltime_forecast(actual_data = data) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25,
    .interactive = interactive
  )
##----------------END MODELS