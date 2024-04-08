##############################################################################-
# DATE:
#   2024/mar/31
# AUTHOR:
#  Jaime Buitrago
# DESCRIPTION:
#   Dsitribution graphs for continuos variables
##############################################################################-

# Prepare workspace

rm (list=ls())
source("scripts/00_packages.R")
library(pROC)
library(MLeval)
gc()

############################################################################-
# 1. Database preparation ----
############################################################################-

train <- fread("stores/raw/train_personas.csv")
db<- fread("stores/raw/train_hogares.csv")

train <- merge(train,db,
                      by = 'id', 
                      no.dups = TRUE,
                      all = TRUE,
                      suffixes = "")
train$db <- "train"
test<- fread("stores/raw/test_personas.csv")
db <- fread("stores/raw/test_hogares.csv")

test<- merge(test,db,
         by = 'id', 
         no.dups = TRUE,
         all = TRUE,
         suffixes = "")

test$db <- "test"

db <- rbind(test,train, fill=TRUE)

x <- c("P6545","P6510", 
            "P6580","P6585s1","P6585s2","P6585s3","P6585s4",
            "P6590","P6600","P6620","P6630s1","P6630s2","P6630s3","P6630s4","P6630s6",
            "P7422","P7472",
            "P7495","P7500s2","P7500s3","P7505")

dummy <- function(var){
  case_when({{var}}==1~1,{{var}}==2 | {{var}}==9~0)
}

model <- db%>%select("id","Pobre","Nper","Dominio","Orden","db","P5140", x)%>%
         mutate(across(x,dummy))

count <- function(var){
    sum({{var}}, na.rm = TRUE)
  }

db <- model%>%summarize(across(x,count), .by=id)

model <- model%>%select("id","Pobre","Nper","Dominio","Orden","db","P5140")%>%
         filter(Orden==1)%>%
         left_join(db)

for (i in 7:28) {
  model[[i]]<-model[[i]]/model$Nper 
}

model <- model %>% mutate(P5140=replace_na(P5140, 0))

db <- model
db$Dominio <- as.factor(db$Dominio)

train <- db%>%filter(db=="train")
test <- db%>%filter(db=="test")

gc()

############################################################################-
# 2. Defining models ----
############################################################################-

model <-formula(paste0("Pobre~","Dominio + ", "P5140 + ",
                       paste0(x, collapse = " + ")))

############################################################################-
# 3. Estimating Linear regression model ----
############################################################################-

set.seed(1492)

ctrl <-  trainControl(method = "cv",
                      number = 5,
                      classProbs = TRUE,
                      savePredictions = T)

train$Pobre <- factor(train$Pobre,
                      levels=c("0","1"),
                      labels=c("No", "Yes"))

logit <- train(model,
               data=train,
               trControl = ctrl,
               metric = "Accuracy",
               method = "glm",
               family = "binomial"
               )

knn <- train(model,
             data=train,
             trControl = ctrl,
             metric = "Accuracy",
             method = "knn",
             preProcess = c("center","scale"),
             use.all = TRUE,
             tuneGrid = expand.grid(k=seq(1,5,by=1))
             )

lda <- train(model,
             data=train,
             trControl = ctrl,
             metric = "Accuracy",
             method = "lda"
             )

qda <- train(model,
             data=train,
             trControl = ctrl,
             metric = "Accuracy",
             method = "qda"
             )

nb <- train(model,
            data=train,
            trControl = ctrl,
            metric = "Accuracy",
            method = "naive_bayes"
            )

x <- c("logit","lda","qda","nb")

models <-  data.frame(Model = character(),
                      Accuracy = numeric(),
                      Recall = numeric(),
                      Precision = numeric(),
                      F1 = numeric())

for (i in x) {
  model <- get(i)
  pred <- model$pred$pred
  obs <- model$pred$obs
  cm <-confusionMatrix(data = pred, 
                  reference = obs, 
                  positive = "Yes",
                  mode = "prec_recall")
  accuracy <- cm$overall['Accuracy']
  recall <- cm$byClass['Recall']
  precision <- cm$byClass['Precision']
  f1 <- cm$byClass['F1']
  models <- rbind(models, data.frame(Model = toupper(i),
                                             Accuracy = accuracy,
                                             Recall = recall,
                                             Precision = precision,
                                             F1 = f1))
}

cm <- data.frame(Threshold = numeric())

for (i in x) {
  model <- get(i)
  obs <- model$pred$obs
  pred <- model$pred$Yes
  roc<-roc(response=obs,predictor=pred)
  roc <- coords(roc, x = "best", best.method = "closest.topleft")
  threshold <- roc$threshold
  cm <- rbind(cm, data.frame(Threshold= threshold))
}

models <- cbind(models,cm)

for (i in seq_along(x)) {
  model <- x[i]
  model_name <- i  
  obs <- model$pred$Yes
  pred <- model$pred$optimized
  pred <- factor(ifelse(obs >= models[6, model_name], 1, 0),
                 labels = c("Yes", "No"))
}

for (i in x) {
  model <- get(i)
  obs <- model$pred$Yes
  pred <- model$pred$optimized
  pred <- factor(ifelse(obs>= models[6,as.numeric(gsub("\\D","",i))], 
                        "Yes", "No"))
  }


logit$pred$optimized <-factor(ifelse(logit$pred$Yes>=models[1,6],1,0),
            labels=c("Yes","No"))

train <- train%>%mutate(pobre_hat_adj=ifelse(hat<=threshold$threshold,1,0))

train$pobre_hat_adj<- factor(train$pobre_hat_adj)

confusionMatrix(data = train$Pobre, 
                reference = train$logit, 
                positive="Yes",
                mode = "prec_recall")

for (var in list("logit","lda","qda","nb")){
  train <- train%>% 
    mutate({{var}}:=predict(!!sym(paste0(var)), 
                            newdata=train, type = "raw"))%>%
    left_join(train)
}

db$hat <- predict(lr,db)
db <- db%>%mutate(Pobre_hat=ifelse(hat>=threshold$threshold,1,0))


db$Pobre_hat <- factor(db$pobre_hat,
                          levels=c("0","1"),
                          labels=c("No", "Yes"))

#########
arrow::write_parquet(db, sink = "stores/db.parquet")
db <- arrow::read_parquet("stores/db.parquet")
train <- db%>%filter(db=="train")
test <- db%>%filter(db=="test")
train$Pobre <- factor(train$Pobre,
                       levels=c("0","1"),
                       labels=c("No", "Yes"))



############################################################################-
# 4. Exporting data base to kaggle ----
############################################################################-

predictSample <- test   %>% 
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)

template<-read.csv("data/sample_submission.csv")

write.csv(predictSample,"classification_elasticnet.csv", row.names = FALSE)
