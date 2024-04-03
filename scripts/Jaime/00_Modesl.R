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
people<- fread("stores/raw/train_hogares.csv")

train <- merge(train,people,
                      by = 'id', 
                      no.dups = TRUE,
                      all = TRUE,
                      suffixes = "")

test<- fread("stores/raw/test_personas.csv")
people <- fread("stores/raw/test_hogares.csv")

test<- merge(test,people,
         by = 'id', 
         no.dups = TRUE,
         all = TRUE,
         suffixes = "")

rm(people)


model1 <- c(P6545,P6510,P6580,P6585s1,P6585s2,P6585s3,P6585s4,
           P6590,P6600,P6620,P6630s1,P6630s2,P6630s3,P6630s4,P6630s6,
           P7422,P7472,
           P7495,P7500s2,P7500s3,P7505)

x <- test%>%select(P6545)

dummy <- function(db,var){
  db%>%mutate(new=sum({{var}}))
  #db <- db%>%mutate(gen={{var}})
}

dummy(x,P6545)

table(test$P6545)
table(test$dummy)


model1 <- Pobre~Npersug+Dominio+
          P6545+P6510+P6580+P6585s1+P6585s2+P6585s3+P6585s4+
          P6590+P6600+P6620+P6630s1+P6630s2+P6630s3+P6630s4+P6630s6+
          P7422+P7472+
          P7495+P7500s2+P7500s3+P7505

glm(Pobre~Npersug+Dominio+
      P6545+P6510+P6580+P6585s1+P6585s2+P6585s3+P6585s4+
      P6590+P6600+P6620+P6630s1+P6630s2+P6630s3+P6630s4+P6630s6+
      P7422+P7472,
    data = train, family = "binomial")

str(test$P7422,test$P7472)
