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

db <- rbind(test,train, fill=TRUE) # A single database to transform all variables

# Variables used by DANE to calculate household income

db <- db%>%mutate(P6240=ifelse(is.na(P6240),0,
                                  ifelse(P6240==1,1,
                                         ifelse(P6240==2,2,
                                                ifelse(P6240>=3,3,0))))) # Defining a PEA proxy

x <- c( "P6545","P6510", #Employed income variables
        "P6580","P6585s1","P6585s2","P6585s3","P6585s4", 
        "P6590","P6600","P6620","P6630s1","P6630s2","P6630s3","P6630s4","P6630s6", 
        "P7422","P7472", #Unemployed and Outside the labour force income variables
        "P7495","P7500s2","P7500s3","P7505") #All 

dummy <- function(var){
  case_when({{var}}==1~1,{{var}}==2 | {{var}}==9~0)
}

model <- db%>%select("id","Pobre","Nper","Dominio","Orden","db",
                     "P5140","P6240", x)%>%
         mutate(across(x,dummy)) # Transforming variables into dummy variables (1 = Yes, 0 = No | NSNR)

count <- function(var){
    sum({{var}}, na.rm = TRUE)
  }

db <- model%>%summarize(across(x,count), .by=id) #Counting people who contribute to household income

model <- model%>%select("id","Pobre","Nper","Dominio","Orden","db",
                        "P6240","P5140")%>%
         filter(Orden==1)%>%
         left_join(db)

for (i in 9:29) {
  model[[i]]<-model[[i]]/model$Nper 
} #Calculating as percentage

model <- model %>% mutate(P5140=replace_na(P5140, 0)) # Rent expense varaible

db <- model
db$Dominio <- as.factor(db$Dominio) # Geographical criteria for poverty lines

train <- db%>%filter(db=="train")
test <- db%>%filter(db=="test")

gc()

############################################################################-
# 2. Defining models ----
############################################################################-

model <-formula(paste0("Pobre~","Dominio + P5140 + P6240 + ",
                       paste0(x, collapse = " + ")))

############################################################################-
# 3. Estimating models ----
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

#Too many ties error don't run knn model
#knn <- train(model,
 #            data=train,
  #           trControl = ctrl,
   #          metric = "Accuracy",
    #         method = "knn",
     #        preProcess = c("center","scale"),
      #       use.all = TRUE,
       #      tuneGrid = expand.grid(k=seq(1,5,by=1))
        #     )

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

############################################################################-
# 4. Alternative cutoffs ----
############################################################################-

x <- c("logit","lda","qda","nb")

# Extracting performance metrics

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

# Calculating optimized cutoffs

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

# Prediction based on optimized cutoffs

#Don't run this loop doesn't work 
for (i in seq_along(x)) {
  threshold <- models[i, 6]
  for (j in x) {
    model <- get(j)
    model$pred$Optimized <- factor(ifelse(model$pred$Yes>=threshold,1,0),
                                   labels = c("Yes"))
    assign(j, model, envir = .GlobalEnv)
  }
}


logit$pred$optimized <-factor(ifelse(logit$pred$Yes>=models[1,6],1,0),
                              levels=c("0","1"),
                              labels=c("Yes","No"))
lda$pred$optimized <-factor(ifelse(lda$pred$Yes>=models[2,6],1,0),
                            levels=c("0","1"),
                            labels=c("Yes","No"))
qda$pred$optimized <-factor(ifelse(qda$pred$Yes>=models[3,6],1,0),
                            levels=c("0","1"),
                            labels=c("Yes","No"))
nb$pred$optimized <-factor(ifelse(nb$pred$Yes>=models[4,6],1,0),
                           levels=c("0","1"),
                           labels=c("Yes","No"))

# Optimized performance metrics 

threshold <-  data.frame(Model = character(),
                      Accuracy = numeric(),
                      Recall = numeric(),
                      Precision = numeric(),
                      F1 = numeric())

for (i in x) {
  model <- get(i)
  pred <- model$pred$optimized
  obs <- model$pred$obs
  cm <-confusionMatrix(data = pred, 
                       reference = obs, 
                       positive = "Yes",
                       mode = "prec_recall")
  accuracy <- cm$overall['Accuracy']
  recall <- cm$byClass['Recall']
  precision <- cm$byClass['Precision']
  f1 <- cm$byClass['F1']
  threshold <- rbind(threshold, data.frame(Model = toupper(i),
                                     Accuracy = accuracy,
                                     Recall = recall,
                                     Precision = precision,
                                     F1 = f1))
}

############################################################################-
# 4. Exporting data base to kaggle ----
############################################################################-

test<- test%>% 
       mutate(logit = predict(logit, newdata = test, type = "raw"),
              lda = predict(lda, newdata = test, type = "raw"),
              qda = predict(qda, newdata = test, type = "raw"),
              nb = predict(nb, newdata = test, type = "raw")
              )
test<- test%>% 
  mutate(logit =ifelse(logit=="Yes",1,0),
         lda =ifelse(lda=="Yes",1,0),
         qda =ifelse(qda=="Yes",1,0),
         nb =ifelse(nb=="Yes",1,0)
  )

for (i in x) {
    pred <- test %>% select(id, !!sym(i))
    colnames(pred) <- c("id", "pobre")
    write.csv(pred, paste0("stores/Submissions/Jaime/", i, ".csv"),
                           row.names = FALSE)
}

#End
