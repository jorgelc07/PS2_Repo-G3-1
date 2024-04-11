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

#Selección de Variables de interés
test_p <- test_personas%>%
  select(id, Clase, Orden,Dominio,P6020,P6040,P6426,P6050, 
         P6090, P6800,P6100, P6210,P6240, P7472, P7422,
         Pet, Oc, Des, Ina)
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
test_h <- test_hogares %>% 
  select(id, Clase, P5000, P5010, P5090,P5140, Nper, Npersug, Lp)


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
# - 3.1 test
test_p$Genero <- ifelse(test_p$P6020 == 2, 1, 0) %>% as.numeric()
test_p$Menores_edad <- if_else(test_p$P6040<=14, 1, 0 , missing = NULL)
test_p$adulto_mayor <- if_else(test_p$P6040>=65, 1, 0 , missing = NULL)
test_p <- test_p %>%
  mutate(Desempleado = replace_na(Des, 0),
         Inactivo = replace_na(Ina, 0),
         Ocupado = replace_na(Oc, 0),
         Pet = replace_na(Pet,0))


#Está afiliado, es cotizante o beneficiario de alguna EPS.
test_p$eps <- ifelse(test_p$P6090 == 1, 1, 0) %>% as.numeric()

#Tipo de afiliación 
test_p$contributivo <- ifelse(test_p$P6100 == 1, 1, 0)
test_p$contributivo[test_p$P6100 != 1] <- 0
test_p$contributivo[test_p$P6100 == "."] <- 0
test_p$contributivo[is.na(test_p$P6100)] <- 0

#Rural
test_p$rural<-ifelse(test_p$Clase == 1, 1, 0)
test_p$rural <- ifelse(test_p$Clase == 1, 1, 0) %>% as.numeric()

# - Estudiante
test_p$estudiante <- ifelse(test_p$P6240 == 3, 1, 0)
test_p$estudiante[test_p$P6240 != 3] <- 0
test_p$estudiante[test_p$P6240 == "."] <- 0
test_p$estudiante[is.na(test_p$estudiante)] <- 0

# - Busca trabajo
test_p$busca_trabajo <- ifelse(test_p$P6240 == 2, 1, 0)
test_p$busca_trabajo[test_p$P6240 != 2] <- 0
test_p$busca_trabajo[test_p$P6240 == "."] <- 0
test_p$busca_trabajo[is.na(test_p$busca_trabajo)] <- 0

# - Amo(a) de casa
test_p$amo_casa <- ifelse(test_p$P6240 == 4, 1, 0)
test_p$amo_casa[test_p$P6240 != 4] <- 0
test_p$amo_casa[test_p$P6240 == "."] <- 0
test_p$amo_casa[is.na(test_p$amo_casa)] <- 0

# - Hijos en el hogar
test_p$hijos_hogar <- ifelse(test_p$P6050 == 3, 1, 0)
test_p$hijos_hogar[test_p$P6050 != 3] <- 0
test_p$hijos_hogar[test_p$P6050 == "."] <- 0
test_p$hijos_hogar[is.na(test_p$hijos_hogar)] <- 0

# - Primaria
test_p$primaria <- ifelse(test_p$P6210 == 1, 1, 0)
test_p$primaria[test_p$P6210 == "."] <- 0
test_p$primaria[is.na(test_p$primaria)] <- 0

# - Secundaria
test_p$secundaria <- ifelse(test_p$P6210 == 4, 1, 0)
test_p$secundaria[test_p$P6210 == "."] <- 0
test_p$secundaria[is.na(test_p$secundaria)] <- 0

# - Media
test_p$media <- ifelse(test_p$P6210 == 5, 1, 0)
test_p$media[test_p$P6210 == "."] <- 0
test_p$media[is.na(test_p$media)] <- 0

# - Superior
test_p$superior <- ifelse(test_p$P6210 == 6, 1, 0)
test_p$superior[test_p$P6210 == "."] <- 0
test_p$superior[is.na(test_p$superior)] <- 0

# - Edad (Sólo mayores de 18 años)
test_p <- rename(test_p, c("edad" = "P6040"))
test_p$edad_2 <- test_p$edad^2

# - Estudiante
test_p$estudiante <- ifelse(test_p$P6240 == 3, 1, 0)
test_p$estudiante[test_p$P6240 != 3] <- 0
test_p$estudiante[test_p$P6240 == "."] <- 0
test_p$estudiante[is.na(test_p$estudiante)] <- 0

#-Busca trabajo
test_p$busca_trabajo <- ifelse(test_p$P6240 == 2, 1, 0)
test_p$busca_trabajo[test_p$P6240 != 2] <- 0
test_p$busca_trabajo[test_p$P6240 == "."] <- 0
test_p$busca_trabajo[is.na(test_p$busca_trabajo)] <- 0

#-Amo(a) de casa
test_p$amo_casa <- ifelse(test_p$P6240 == 4, 1, 0)
test_p$amo_casa[test_p$P6240 != 4] <- 0
test_p$amo_casa[test_p$P6240 == "."] <- 0
test_p$amo_casa[is.na(test_p$amo_casa)] <- 0

#-Hijos en el hogar
test_p$hijos_hogar <- ifelse(test_p$P6050 == 3, 1, 0)
test_p$hijos_hogar[test_p$P6050 != 3] <- 0
test_p$hijos_hogar[test_p$P6050 == "."] <- 0
test_p$hijos_hogar[is.na(test_p$hijos_hogar)] <- 0

#-Experiencia trabajo actual
test_p <- rename(test_p, c("exp_trab_actual" = "P6426"))

#-Horas de trabajo a la semana
test_p <- rename(test_p, c("horas_trab_usual" = "P6800"))

#-Ciudad
test_p <- rename(test_p, c("ciudad" = "Dominio"))

#-Imputación de experiencia
test_p$exp_trab_actual <-ifelse(test_p$edad < 18 & 
                                   is.na(test_p$exp_trab_actual), 0, 
                                 test_p$exp_trab_actual)

#-Imputación Horas 
test_p$horas_trab_usual <-ifelse(test_p$edad < 18 & 
                                    is.na(test_p$horas_trab_usual), 0, 
                                  test_p$horas_trab_usual)

test_p<- test_p %>% select("id", "Orden", "Clase",
                             "ciudad", "edad", "edad_2", "Genero", 
                             "estudiante", "busca_trabajo", "amo_casa",
                             "hijos_hogar","exp_trab_actual",
                             "horas_trab_usual", 
                             "Menores_edad","adulto_mayor","Desempleado","Inactivo",
                             "Ocupado","hijos_hogar","edad_2","estudiante",
                             "busca_trabajo","amo_casa","hijos_hogar","eps","contributivo","rural","superior","media","secundaria","primaria")

#Hogares
test_h <- test_h %>% rename(num_cuartos = P5000, num_cuartos_dormir = P5010) 

#-Vivienda propia
test_h$vivienda_propia <- ifelse(test_h$P5090 == 1 | test_h$P5090==2 , 1, 0)
test_h$vivienda_propia[test_h$P5090 != 1 | test_h$P5090 != 2 ] <- 0
test_h$vivienda_propia[test_h$P5090 == "."] <- 0
test_h$vivienda_propia[is.na(test_h$P5090)] <- 0

#vivienda arriendo 
test_h$vivienda_arriendo<-ifelse(test_h$P5090 == 3, 1, 0)
test_h$vivienda_arriendo[test_h$P5090 != 3] <- 0
test_h$vivienda_arriendo[test_h$P5090 == "."] <- 0
test_h$vivienda_arriendo[is.na(test_h$P5090)] <- 0

test_h <-test_h%>% select("id","num_cuartos","num_cuartos_dormir","Nper"
                            ,"Npersug","Lp",
                            "vivienda_arriendo","vivienda_propia")

#Unión de la base de datos 
test <- merge(test_h,test_p,
               by = 'id', 
               no.dups = TRUE,
               all = TRUE,
               suffixes = "")

# Pasar la base únicamente a hogares
test <- test %>% group_by(id) %>%
  summarize(edad = mean(edad,na.rm = TRUE),
            edad_2 = mean(edad_2,na.rm = TRUE),
            Genero = mean(Genero,na.rm = TRUE),
            estudiante = mean(estudiante,na.rm = TRUE),
            busca_trabajo = mean(busca_trabajo,na.rm = TRUE),
            amo_casa =mean(amo_casa,na.rm = TRUE),
            hijos_hogar = mean(amo_casa,na.rm = TRUE),
            primaria = mean(primaria,na.rm = TRUE),
            secundaria = mean(secundaria,na.rm = TRUE),
            media = mean(media,na.rm = TRUE),
            superior = mean(superior,na.rm = TRUE),
            exp_trab_actual = mean(exp_trab_actual,na.rm = TRUE),
            horas_trab_usual = mean(horas_trab_usual,na.rm = TRUE),
            Nper = mean(Nper,na.rm = TRUE),
            num_menores = sum(Menores_edad,na.rm = TRUE),
            num_adulto=sum(adulto_mayor,na.rm = TRUE),
            eps=mean(eps,na.rm = TRUE),
            rural=max(rural,na.rm = TRUE),
            num_cuartos=mean(num_cuartos,na.rm = TRUE),
            num_cuartos_dormir=mean(num_cuartos_dormir,na.rm = TRUE),
            Npersug=mean(Npersug,na.rm = TRUE),
            vivienda_arriendo=max(vivienda_arriendo,na.rm = TRUE),
            vivienda_propia=max(vivienda_propia,na.rm = TRUE),
            contributivo=mean(contributivo,na.rm = TRUE),
            Desempleado=mean(Desempleado,na.rm = TRUE),
            Inactivo=mean(Inactivo,na.rm = TRUE),
            Ocupado=mean(Ocupado,na.rm = TRUE),
            ciudad = first(ciudad))

#Agregar la información a nivel de hogares 
saveRDS(test, "stores/test_clean.rds")