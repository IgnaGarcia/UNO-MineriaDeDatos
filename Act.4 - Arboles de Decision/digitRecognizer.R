###############
# Digit Recognizer
# Competition of Kaggle
#
# Creator : Group B - Benitez, Garcia, Rodriguez, Rechimon
# In representation of : Universidad Nacional del Oeste, Argentina
#
# Create date: 2020/10/07
# Update date: 2020/10/08 
#        comment: comentarios para 66 variables
###############


##----------------START LIBRARIES
library(readr) #To read .csv
library(tidyverse) #To use select(), subset(), mutate(), and others
library(ranger) #To Random Forest
library(pROC) #ROC
library(randomForest)
##----------------END LIBRARIES


start <- Sys.time()
##----------------START READ DATA
data <- read_csv("digit-recognizer/train.csv")
test <- read_csv("digit-recognizer/test.csv")
##----------------END READ DATA


##----------------START PROCESS DATA
#str(data)
sapply(data, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

data$label <- as.factor(data$label)

smp_size <- floor(0.80 * nrow(data))
set.seed(123)

train_ind <- sample(seq_len(nrow(data)), size = smp_size)
data.train <-  data[train_ind, ]
data.test <- data[-train_ind, ]
##----------------END PROCESS DATA
  

##----------------START MODELS
###------------ MEJOR VARIABLE EN POCOS MINUTOS
# nvars <- 33 #Moidificar este valor
# modelo.ranger <- ranger( label ~ . , data= data.train[,]
#                          , num.trees = 800
#                          , mtry = nvars
#                          , importance = "impurity"
#                          , write.forest = T
#                          , probability = F
#                          , alpha = 0.01
# ) 
# prediccion <- predict(modelo.ranger, data.test) 
# mc <- with(data.test,table(prediccion$predictions, data.test$label))
# 
# library(caret)
# cm <- confusionMatrix(prediccion$predictions, data.test$label)
# modelo.ranger$prediction.error
# aciertos <- sum(diag(mc)) / sum(mc) * 100
# aciertos

# Out:
# 24 vars= 96.65476
# 25 vars= 96.71429
# 28 vars= 96.77381
# 30 vars= 96.78
# 33 vars= 96.83333----
# 35 vars= 96.77381

####--------------------
###---------- Important Vars
# tuneRF(x = data.train, y = data.train$label,
#        mtryStart  = 8, stepFactor = 2,    
#        ntreeTry   = 600, improve    = 0.01)
# Out:
# mtry = 33   OOB error = 1.32%
# mtry = 66   OOB error = 0.52%
# mtry = 132  OOB error = 0.08%
# mtry = 264  OOB error = 0%
qvar <- 33
###

###---------- Model 1, con probabilidades
model1 <- ranger( label ~ . , data= data.train[,]
                  , num.trees = 1000
                  , mtry = qvar
                  , importance = "impurity"
                  , write.forest = T
                  , probability = T
                  , alpha = 0.005
) # Out:
model1$prediction.error
# qvars 33 -> OOB: 0.09154588
# qvars 66 -> OOB: 0.08434042
# qvars 132 -> OOB: 0.07927676
###

###---------- Model 2, sin probabilidades
model2 <- ranger( label ~ . , data= data.train[,]
                  , num.trees = 3000
                  , mtry = qvar
                  , importance = "impurity"
                  , write.forest = T
                  , probability = F
                  , alpha = 0.005
)
model2$predictions

# roc <- multiclass.roc(data.train$label, model2$predictions, levels=10, percetnt= T, auc= T, ci= T, plot= T)
# plot.roc(roc, legacy.axes= T, print.thres= "best", print.auc= T)

model2$confusion.matrix
aciertos <- sum(diag(model2$confusion.matrix)) / sum(model2$confusion.matrix) * 100
aciertos 
# Salida: 
# qvars 33: 96.63393
# qvars 66: 96.61607 
# qvars 132: 96.57143
###

###--------- Robustez del Modelo
vAciertos_RF=c(0)

for(j in 1:10){
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  data.train <- data[train_ind, ]
  data.test <- data[-train_ind, ]
  
  modelo.ranger <- ranger( label ~ . , data= data.train[,]
                           , num.trees = 1000
                           , mtry = qvar
                           , importance = "impurity"
                           , write.forest = T
                           , probability = F
                           , alpha = 0.01
  )
  
  prediccion <- predict(modelo.ranger, data.test) 
  mc <- with(data.test,table(prediccion$predictions, data.test$label))
  
  #Calculo el % de  aciertos totales
  aciertos <- sum(diag(mc)) / sum(mc) * 100
  vAciertos_RF[j] = aciertos
}
cat('promedio: ',mean(vAciertos_RF),
    '%  ; min: ', min(vAciertos_RF),
    ' ; max: ', max(vAciertos_RF), '\n')

# Out:
# qvars: 33 -> promedio:  96.6119 %  ; min:  96.42857  ; max:  96.82143
# qvars: 66 -> promedio:  96.57976 %  ; min:  96.2381  ; max:  96.78571 
# qvars: 132 -> promedio:  96.46548 %  ; min:  96.27381  ; max:  96.64286
###
##----------------END MODELS


##----------------START PREDICTION
preds <- predict(model2, data = test)
test$Label <- as.numeric(as.character(preds$predictions))
test <- mutate(test, ImageId = row_number())
# KAGGLE:
# qvars 33 -> 0.96542
# qvars 66 -> 0.96564 
# qvars 132 -> 0.96425
write.csv (select(test, ImageId, Label), 'submission.csv' ,fileEncoding= "UTF-8",row.names= F)
##----------------END PREDICTION

totalTime <- Sys.time() - start
print(totalTime) 
# qvars 33 -> Salida: 1.132367 horas
# qvars 66 -> Salida: 2.0389 horas
# qvars 132 -> Salida: 2.952094 horas