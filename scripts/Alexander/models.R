##############################################################################-
# DATE:
#   2024/mar/31
# AUTHOR:
#  Alexander Almeida
# DESCRIPTION:
#   código que corre los modelos. 
##############################################################################-

# Prepare workspace
rm (list=ls())
source("scripts/00_packages.R")
gc()

#cargar las bases de datos 
train<-readRDS(file="stores/train_clean.rds") 
test<-readRDS(file="stores/test_clean.rds")

#Tranformar la variable "pobre" a factor. 
train$Pobre <- factor(train$Pobre,
                      levels=c("0","1"),
                      labels=c("No", "Yes"))

###################Correr los modelos:########################################## 
set.seed(1492)

ctrl <-  trainControl(method = "cv",
                      number = 5,
                      classProbs = TRUE,
                      savePredictions = T)

#1- Logit 
set.seed(1492)
logit <- train(
  Pobre ~ edad + edad_2 + Genero + estudiante + busca_trabajo + 
    amo_casa + primaria + secundaria + media + superior+num_cuartos+
    contributivo+num_adulto+rural+vivienda_arriendo,
  trControl = ctrl,
  metric = "Accuracy",
  family = "binomial",
  method = "glmnet"
)

#Reaizar la predicción fuera de la muestra
test$pobre <- predict(logit, test)

#Transformar la variable a 1 y 0. 
test<- test%>% 
  mutate(pobre =ifelse(pobre=="Yes",1,0))
head(test %>% select(id,pobre))

#Crear el arhivo de subida en kaggle. 
submit<-test  %>% select(id,pobre)
write.csv(submit,"stores/Submissions/Alexander/classification_logit.csv",row.names=FALSE)

#Modelo 2: QDA
set.seed(1492)
qda <- train(
             Pobre ~ edad + edad_2 + Genero + estudiante + busca_trabajo +
               amo_casa + primaria + secundaria + media + superior+
               num_cuartos+contributivo+num_adulto+rural+vivienda_arriendo,
             trControl = ctrl,
             metric = "Accuracy",
             data = train,
             method = "qda"
)
#Realizar la predicción fuera de la muestra
test$pobre <- predict(qda, test)

#Transformar la variable a 1 y 0. 
test<- test%>% 
  mutate(pobre =ifelse(pobre=="Yes",1,0))
head(test %>% select(id,pobre))

submit<-test  %>% select(id,pobre)
write.csv(submit,"stores/Submissions/Alexander/classification_qda.csv",row.names=FALSE)

#Modelo 3: LDA
set.seed(1492)
qda <- train(
  Pobre ~ edad + edad_2 + Genero + estudiante + busca_trabajo +
    amo_casa + primaria + secundaria + media + superior+
    num_cuartos+contributivo+num_adulto+rural+vivienda_arriendo,
   data = train,
   trControl = ctrl,
   metric = "Accuracy",
   method = "lda"
)
#Realizar la predicción fuera de la muestra
test$pobre <- predict(qda, test)

#Transformar la variable a 1 y 0. 
test<- test%>% 
  mutate(pobre =ifelse(pobre=="Yes",1,0))
head(test %>% select(id,pobre))

submit<-test  %>% select(id,pobre)
write.csv(submit,"stores/Submissions/Alexander/classification_qda.csv",row.names=FALSE)
 
#Modelo 4: Logit Ridge
lambda_grid <- 10^seq(-4, 0.01, length = 10) #en la practica se suele usar una grilla de 200 o 300
ridge<- train(
  Pobre ~ edad + edad_2 + Genero + estudiante + busca_trabajo + amo_casa + primaria
          + secundaria + media + superior+num_cuartos+contributivo+num_adulto+
          rural+vivienda_arriendo,
            data = train, 
            method = "glmnet",
            family = "binomial", 
            metric = "Accuracy",
            trControl = ctrl,
            tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid), 
            preProcess = c("center", "scale")
)
#Construir la predicción de la base de datos
test$pobre <- predict(ridge, test)

#Cambiar las categorías a 0 y 1. 
test<- test%>% 
  mutate(pobre =ifelse(pobre=="Yes",1,0))
head(test %>% select(id,pobre))

#Subida de kaggle
submit<-test  %>% select(id,pobre)
write.csv(submit,"stores/Submissions/Alexander/classification_logit_lasso.csv",row.names=FALSE)

#Modelo 5: Logit - carret 
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = F,
                    verbose=FALSE,
                    savePredictions = T)

set.seed(1492)
logitcart <- train(  Pobre ~ edad + edad_2 + Genero + estudiante + busca_trabajo + amo_casa + primaria + secundaria + media + superior+num_cuartos+contributivo+num_adulto+rural+vivienda_arriendo, 
              data = train, 
              method = "glm",
              trControl = ctrl,
              family = "binomial", 
              preProcess =  c("center", "scale"))
#Predecir fuera de la muestra
test$pobre <- predict(logitcart, test)

#Cmabiar los factores por números. 
test<- test%>% 
  mutate(pobre =ifelse(pobre=="Yes",1,0))
head(test %>% select(id,pobre))

#Crear la subida a kaggle.
submit<-test  %>% select(id,pobre)
write.csv(submit,"stores/Submissions/Alexander/classification_logit_carret.csv",row.names=FALSE)

#Modelo 6: Random forest
#creamos el bosque enfocado en la accuracy
set.seed(1492)
forest <- train(Pobre ~ edad + edad_2 + Genero + estudiante + 
                  busca_trabajo + amo_casa + primaria + 
                  secundaria + media + superior+num_cuartos+
                  contributivo+num_adulto+rural+vivienda_arriendo, 
                  data = train, 
                  method = "rf",
                  trControl = ctrl,
                  metric="Accuracy",
)   

test$pobre <- predict(forest, test)

#Predecir la variable pobre
test<- test%>% 
  mutate(pobre =ifelse(pobre=="Yes",1,0))
head(test %>% select(id,pobre))

#Crear la subida a kaggle
submit<-test  %>% select(id,pobre)
write.csv(submit,"stores/Submissions/Alexander/classification_Random_Forest.csv",row.names=FALSE)

#Modelo 7: Bagging 
set.seed(1492)
bagging <- bagging(Pobre ~ edad + edad_2 + Genero + estudiante + 
                     busca_trabajo + amo_casa + primaria + secundaria + 
                     media + superior+num_cuartos+contributivo+num_adulto+
                     rural+vivienda_arriendo,
                     data  = train, nbagg = 500)

#predecir fuera de muestra.
test$pobre <- predict(bagging, test)
test<- test%>% 
  mutate(pobre =ifelse(pobre=="Yes",1,0))
head(test %>% select(id,pobre))
test$pobre <- predict(gbm_tree, test)

test<- test%>% 
  mutate(pobre =ifelse(pobre=="Yes",1,0))
head(test %>% select(id,pobre))

#Crear la subida a kaggle
submit<-test  %>% select(id,pobre)
write.csv(submit,"stores/Submissions/Alexander/classification_baaging.csv",row.names=FALSE)


#Modelo 8: Boosting 
grid_gbm<-expand.grid(n.trees= c( 50, 100,150),
                      interaction.depth=c(1,2),
                      shrinkage=c(0.01),
                      n.minobsinnode=c(5, 10))

set.seed(1492) 
gbm_tree <- train(Pobre ~ edad + edad_2 + Genero + estudiante + busca_trabajo 
                  + amo_casa + primaria + secundaria + 
                    media + superior+num_cuartos+contributivo+
                    num_adulto+rural+vivienda_arriendo,
                  data = train, 
                  method = "gbm", 
                  tuneGrid=grid_gbm,
                  metric = "Accuracy",
                  verbose = FALSE
)            


test$pobre <- predict(gbm_tree, test)

test<- test%>% 
  mutate(pobre =ifelse(pobre=="Yes",1,0))
head(test %>% select(id,pobre))

#Crear la subida a kaggle
submit<-test  %>% select(id,pobre)
write.csv(submit,"stores/Submissions/Alexander/classification_boosting.csv",row.names=FALSE)
gbm_tree

 