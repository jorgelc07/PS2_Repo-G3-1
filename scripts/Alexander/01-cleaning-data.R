##############################################################################-
# DATE:
#   2024/mar/31
# AUTHOR:
#  Alexander Almeida
# DESCRIPTION:
#   Código con limpieza de datos antes de inciar con los modelos. 
##############################################################################-

# Prepare workspace

rm (list=ls())
source("scripts/00_packages.R")
gc()

#Leer los datos - 
test_hogares      <- read.csv("stores/raw/test_hogares.csv")
test_personas     <- read.csv("stores/raw/test_personas.csv")
train_hogares     <- read.csv("stores/raw/train_hogares.csv")
train_personas    <- read.csv("stores/raw/train_personas.csv")

prop.table(table(train_hogares$Pobre))

#Selección de Variables de interés
train_p <- train_personas%>%
  select(id, Clase, Orden,Dominio,P6020,P6040,P6426,P6050, P6090,P6800,
         P6100, P6210,P6240, P7472, P7422,
         Pet, Oc, Des, Ina, Ingtot)
####################################
#Lista de variables seleccionadas 
#id: 
#Clase
#Orden
#P6020:sexo
#P6040:Edad
#P6050: Parentesco con el jefe o jefa de hogar. 
#P6090: Está afilizado, es cotizante o beneficiario de alguna EPS.
#P6100: Régimen de salud al que pertenece. 
#P6210: Nivel educativo más alto alcanzado. 
#P7472: Recibió o ganó el mes pasado ingresos por concepto de trabajo (desocuplados) 
#P7422: ¿Recibió o ganó el mes pasado ingresos por concepto detrabajo?. (Desocupados)
#Pet: Población en edad de trabajar 1:sí 0: no 
#Oc: Ocupado 1: sí 
#Des: Desocupado 1: sí 
#Ina: Inactivo 1: sí 
#Ingtot: Ingreso total 
###################################################
train_h <- train_hogares %>% 
  select(Pobre, id, Clase, P5000, P5010, P5090,P5140, Nper, Npersug, Lp,
         Ingtotugarr, Ingpcug)


#Pobre: Pobre=1, No pobre=0
#id:
#clase:
#P5000:Incluyendo sala-comedor ¿de cuántos cuartos en total dispone este hogar?
#P5010: ¿En cuántos de esos cuartos duermen las personas de este hogar? 
#P5090: La vivienda es propia, la están pagando, en arriendo. subarriendo, En usufructo, ocupante
#Nper: Personas en el hogar 
#Npersug:Número de personas en la unidad de gasto
#Lp:Línea de pobreza
#Ingtotugarr:Ingreso total de la unidad de gasto con imputación de arriendo. 
#Ingpcug:Ingreso percápita de la unidad de gasto con imputación de arriendo. 


#Tengo 35 variables. Las necesito todas? 
  # - 3.1 Train

train_p$Genero <- ifelse(train_p$P6020 == 2, 1, 0) %>% as.numeric()
train_p$Menores_edad <- if_else(train_p$P6040<=14, 1, 0 , missing = NULL)
train_p$adulto_mayor <- if_else(train_p$P6040>=65, 1, 0 , missing = NULL)
train_p <- train_p %>%
  mutate(Desempleado = replace_na(Des, 0),
         Inactivo = replace_na(Ina, 0),
         Ocupado = replace_na(Oc, 0),
         Pet = replace_na(Pet,0))


#Cambiar las variables que son factores: 
train_p <- train_p %>% mutate(Clase=factor(Clase,levels=c(0,1),labels=c("Urbano","Rural")),
                              #Genero=factor(Genero,levels=c(0,1),labels=c("Hombre","Mujer")),
                              Parentesco_con_jefe=factor(P6050,levels=c(1,2,3,4,5,6,7,8,9),labels=c("jefe", "pareja", "Hijo/hija", "nieto", "otro", "emplead_servicio", "pensionista", "trabajador", "otro_no_pariente")),
                              nivel_edu=factor(P6210,levels=c(1,2,3,4,5,6,9),labels=c("Ninguno", "Preescolar", "Basica_primaria", "Basica_secundaria", "Media", "Superior", "No_Saber")),
                              Desempleado=factor(Desempleado,levels=c(0,1),labels=c("No","Si")),
                              Inactivo=factor(Inactivo,levels=c(0,1),labels=c("No","Si")),
                              Ocupado=factor(Ocupado,levels=c(0,1),labels=c("No","Si")),
                              Pet=factor(Pet,levels=c(0,1),labels=c("No","Si")))

  
  # - Edad (Sólo mayores de 18 años)
  train_p <- rename(train_p, c("edad" = "P6040"))
  train_p$edad_2 <- train_p$edad^2
  
  # - Estudiante
  train_p$estudiante <- ifelse(train_p$P6240 == 3, 1, 0)
  train_p$estudiante[train_p$P6240 != 3] <- 0
  train_p$estudiante[train_p$P6240 == "."] <- 0
  train_p$estudiante[is.na(train_p$estudiante)] <- 0
  
  # - Busca trabajo
  train_p$busca_trabajo <- ifelse(train_p$P6240 == 2, 1, 0)
  train_p$busca_trabajo[train_p$P6240 != 2] <- 0
  train_p$busca_trabajo[train_p$P6240 == "."] <- 0
  train_p$busca_trabajo[is.na(train_p$busca_trabajo)] <- 0
  
  # - Amo(a) de casa
  train_p$amo_casa <- ifelse(train_p$P6240 == 4, 1, 0)
  train_p$amo_casa[train_p$P6240 != 4] <- 0
  train_p$amo_casa[train_p$P6240 == "."] <- 0
  train_p$amo_casa[is.na(train_p$amo_casa)] <- 0
  
  # - Hijos en el hogar
  train_p$hijos_hogar <- ifelse(train_p$P6050 == 3, 1, 0)
  train_p$hijos_hogar[train_p$P6050 != 3] <- 0
  train_p$hijos_hogar[train_p$P6050 == "."] <- 0
  train_p$hijos_hogar[is.na(train_p$hijos_hogar)] <- 0

  # - Experiencia trabajo actual
  
  train_p <- rename(train_p, c("exp_trab_actual" = "P6426"))
  
  # - Horas de trabajo a la semana
   train_p <- rename(train_p, c("horas_trab_usual" = "P6800"))
  
  # - Ciudad
  train_p <- rename(train_p, c("ciudad" = "Dominio"))
  
  # - Número de Cuartos
 # train_p <- rename(train_p, c("numero_cuartos" = "P5010"))
  
  # - Número de personas
 # train_p <- rename(train_p, c("numero_personas" = "Nper"))
  
  # - Imputación de experiencia
  train_p$exp_trab_actual <- ifelse(train_p$edad < 18 & 
                                   is.na(train_p$exp_trab_actual), 0, 
                                 train_p$exp_trab_actual)
  
  # Imputación Horas 
  train_p$horas_trab_usual <- ifelse(train_p$edad < 18 & 
                                    is.na(train_p$horas_trab_usual), 0, 
                                  train_p$horas_trab_usual)

 train_p<- train_p %>% select("id", "Orden", "Clase",
                                  "ciudad", "edad", "edad_2", "Genero", 
                                  "estudiante", "busca_trabajo", "amo_casa",
                                  "hijos_hogar", "Ingtot",, "exp_trab_actual",
                                  "horas_trab_usual", 
                                  "Menores_edad","adulto_mayor","Desempleado","Inactivo",
                                  "Ocupado","Parentesco_con_jefe","nivel_edu","edad_2","estudiante",
                                  "busca_trabajo","amo_casa","hijos_hogar")


#Hogares
 train_h <- train_h %>% rename(num_cuartos = P5000, num_cuartos_dormir = P5010) #Se renombran variables

 #Creacion de variables en base pegada de hogares
train_h <- train_h %>% mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No_Pobre","Pobre")),
                              Clase=factor(Clase,levels=c(0,1),labels=c("Urbano","Rural")),
                              Vivienda=factor(P5090,levels=c(1,2,3,4,5,6),labels=c("Propia_paga","Propia_No_Paga", "Arriendo","Usufructo", "Ocupante_No_Dueño", "Otra")))

train_h <- train_h %>% select("id","Clase","num_cuartos","num_cuartos_dormir","Nper"
                              ,"Npersug","Lp","Ingtotugarr","Ingpcug","Vivienda")

train <- merge(train_h,train_p,
  by = 'id', 
  no.dups = TRUE,
 all = TRUE,
suffixes = "")
saveRDS(train, "stores/train_clean.rds")