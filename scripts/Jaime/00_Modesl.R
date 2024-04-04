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

rm(train, test)

x <- c("P6545","P6510",
            "P6580","P6585s1","P6585s2","P6585s3","P6585s4",
            "P6590","P6600","P6620","P6630s1","P6630s2","P6630s3","P6630s4","P6630s6",
            "P7422","P7472",
            "P7495","P7500s2","P7500s3","P7505")

dummy <- function(var){
  case_when({{var}}==1~1,{{var}}==2 | {{var}}==9~0)
}

model <- db%>%select("id","Pobre","Npersug","Dominio","Orden","db", x)%>%
         mutate(across(x,dummy))

count <- function(var){
    sum({{var}}, na.rm = TRUE)
  }

db <- model%>%summarize(across(x,count), .by=id)

model <- model%>%select("id","Pobre","Npersug","Dominio","Orden","db")%>%
         filter(Orden==1)%>%
         left_join(db)

for (i in 7:length(x)) {
  model[[i]]<-model[[i]]/model$Npersug 
}

db <- model
db$Dominio <- as.factor(db$Dominio)
db$Pobre <- factor(db$Pobre,
                   levels=c("0","1"),
                   labels=c("No", "Yes"))

############################################################################-
# 2. Defining models ----
############################################################################-

model <-formula(paste0("Pobre~","Dominio + ",paste0(x, collapse = " + ")))

############################################################################-
# 3. Estimating models ----
############################################################################-

ctrl <-  trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

lr <- train(model,
                data=db%>%filter(db=="train"),
                method = "glm",
                family = "binomial",
                trControl = ctrl)
            )

,
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.2),
                  lambda =10^seq(10, -2, length = 10)
                )
                
)

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
