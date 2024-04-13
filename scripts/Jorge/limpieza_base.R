##############################################################################-
# DATE:
#   2024/march/25
# AUTHOR:
#   Jorge Luis Congacha
# DESCRIPTION:
#   clean the data 
##############################################################################-
# Prepare workspace
source("scripts/00_packages.R")
gc()
############################################################################-
# 1. Prepare dataset ----
############################################################################-
test_hogares      <- read.csv("stores/raw/test_hogares.csv")
test_personas     <- read.csv("stores/raw/test_personas.csv")
train_hogares<-read.csv("stores/raw/train_hogares.csv")
train_personas<-read.csv("stores/raw/train_personas.csv")
############# Loading  data ################################################
##### TRAIN PERSONAS #####################
##### Collapse data from indivual level to household level #################
n_distinct(train_personas$id)
n_distinct(train_hogares$id)

#Calculate aggregated huesedhold income from train_personas data, and # people by housedhold.
train_hogares_sum <- aggregate(cbind(Ingtot) ~ id, data = train_personas, FUN = sum)
train_hogares_max <- aggregate(cbind(Orden) ~ id, data = train_personas, FUN = max)
# let´s define a representative member of the household
jefe_hogar_train <- train_personas %>%   #define the head of the household to  keep some relevant variables to predict poverty
  filter(Orden == 1) %>% 
  select(id, P6020, P6040, P6210) # P6040: edad, P6020: sexo 1  hombre, 2 mujer, P6210: nivel educativo 

# Join train_hogares_sum and train_hogares_max
train_hogares_p <- train_hogares_sum %>% 
                     left_join(train_hogares_max, by = c("id"))

train_hogares_p <- train_hogares_p %>% 
                    left_join(jefe_hogar_train, by = c("id"))

#generate Ingtotal per capita
train_hogares_p <- train_hogares_p %>% 
                    mutate(Ingtot_P = (Ingtot / Orden))

##### TEST PERSONAS #####################
#Calculate aggregated # people by huesedhold.
test_hogares_max <- aggregate(cbind(Orden) ~ id, data = test_personas, FUN = max)

# let´s define a representative member of the household
jefe_hogar_test <- test_personas %>%   #define the head of the household to  keep some relevant variables to predict poverty
  filter(Orden == 1) %>% 
  select(id, P6020, P6040, P6210) # P6040: edad, P6020: sexo 1  hombre, 2 mujer, P6210: nivel educativo

# Join
test_hogares_p <- jefe_hogar_test %>% 
                    left_join(test_hogares_max, by = c("id"))

##### MERGE DATA #####################
# Train dataset
train <- train_hogares %>% 
  left_join(train_hogares_p, by = c("id"))

# Test dataset  
test <- test_hogares %>% 
  left_join(test_hogares_p, by = c("id"))

datatable(head(train, 10))

##### DATA #####################
########## Merge the data ###########
# Choose the relevant variables
train_p <- train[, c("id", "Clase", "P6020", "P6040", "Li", "Lp", "Pobre", "Indigente", "Ingtot", "P5090","Nper", "Depto","P5130", "P5140", "P5000", "P5010", "P6210")] 
### TEST DATE
test_p <- test[, c("id", "Clase", "P6020", "P6040", "Li", "Lp","Nper", "Depto","P5130", "P5140", "P5090", "P5000", "P5010", "P6210")]     # let´s try without "P6040" (edad)
# Create some variables
test_p$Pobre <-NA
test_p$Indigente <- NA
test_p$Ingtot <- NA

# let´s check the missing values (approx. 6% missing of Ingtot, 6% of Ingtot_P, 60% of P5130 and 40% of P5130
colSums(is.na(train_p))/nrow(train_p)*100
# Impute the missing values with the mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#Ingtot missing 
train_p$Ingtot[is.na(train_p$Ingtot)] <- Mode(train_p$Ingtot)
#Ingtot_P missing
#train_p$Ingtot_P[is.na(train_p$Ingtot_P)] <- Mode(train_p$Ingtot_P)
#P5140 missing
#train_p$P5140[is.na(train_p$P5140)] <- Mode(train_p$P5140) #
#P5130 missing
#train_p$P5130[is.na(train_p$P5130)] <- Mode(train_p$P5130)  
###### Join data #######
# create new variables that identify the sample (train or test)
test_p <- test_p %>% mutate(sample = "test")
train_p <- train_p %>% mutate(sample = "train")
# Join both data base
fusion_table <- rbind(test_p, train_p)
# Export the fusion_table
write.csv(fusion_table, file = "fusion_table.csv", row.names = FALSE)

#Let´s check the date
glimpse(train_p) #Ingtot: total income 
glimpse(test_p) #Lp: poverty line

### Join both data ####
# fusion_table <- rbind(train_p, test_p)
# table(fusion_table$sample, 10)  # test 66.168 and train 164.960
# write.csv(fusion_table, file= "stores/fusion_table.csv", row.names = FALSE)
###################################################################################################################################
# 1.1 Prepare dataset train_personas ----
# 1.1.1 Selec the variables
# General variables
- id: identificador del hogar
- Orden: orden de la persona
- Clase: Binaria. 1. Cabecera 2. Resto
- Ingtot: Ingresos totales
- Estrato1: Estrato socioeconómico.
- P6020: Binaria. Sexo.
- P6040: Numérica. Años cumplidos.
- P6090: Binaria. Cotizante de seguridad social.
- P6210: Categórica. Nivel educativo más alto aprobado.
- P6210s1: Numérica. Número de años escolares aprobados.
- P6430: Categórica. Tipo de trabajo.
# Job variables
- oc: Binaria. ocupación.
- des: Binaria. desocupado.
- ina: Binaria. Inactivo.
# Income variables
- P6500: Numérica. Ingresos salario.
- P6800: Numérica. Horas trabajo.
# Other income variables
- p6510: Bainaria. Ingresos por horas extras.
- p6585s1: Binaria.  Auxilio alimentación.
- p6585s2: Binaria. Auxilio trasporte.
- p6585s3: Binaria. Auxilio familiar.
- p6585s4: Binaria. Subsidio de educación.
- p7495: Binaria. Ingreso por arriendo y/o pensiones.
- p7505: Binaria. Ingreso por rendimientos financieros.
- p7510s2: Binaria. Ingreso por transferencias internacionales.
- p7510s3: Binaria. Ingreso instituciones gubernamenales.
- p7510s5: Binaria. Ingreso otro rendimientos financieros.
- p6870: Categorica. Tamaño de empresa.

# 1.2 Prepare dataset train_hogares ----
# 1.2.1 Selec the variables
# Caracteristicas del hogar
- id: identificador del hogar
- Clase: Binaria. 1. Cabecera 2. Resto 
- Dominio. Categórica. zona geográfica
- p5000: Numérica. Cuartos total en el hogar
- P5010: Numérica. Num cuartos donde duermen.
- Npersug: Numérica. Número de personas en la unidad de gasto.
- Ingtotugarr: Numérica. Ingreso total de la unidad de gasto después de la imputación.
- Lp: Línea de pobreza.
- Pobre: Variable objetivo.
- Li: Númerica. Línea de indigencia.
- Indigente: Int. Indigente=1 No indigente=0 
- P5090: Categorica. Clase de ocupación de vivienda 
- Nper: Numérico. Número de personas en el hogar.
- Depto: Discreta. Número de depto.
- P5130: Numerica. Cuanto pagaría de arriendo por la vivienda en el hogar.
- P5140: Numerica. Cuanto pagade arrendamiento. 
### Change variables as factors
fusion_table <- fusion_table %>% 
                mutate(across(c(Depto, Clase, Pobre, P6210, P5090,P5000, P5010, Indigente ), as.factor))

# Checing Pobre variable
prop.table(table(fusion_table$Pobre)) #79.98% of the people are not Pobre and 20.01% are Pobre

#Let´s check the date
head(fusion_table, 10)
datatable(tail(fusion_table, 10))
table(fusion_table$sample) # obs test 66.168 and train 164.960
colnames(fusion_table) <- tolower(colnames(fusion_table))

#fusion_table_selected<- fusion_table %>% select(li, lp, p6020, p6040, pobre, indigente, ingtot, p5090, p6210, p5130, p5000, p5010)
#sumtable(fusion_table_selected, out = "return")

# Change format of date to tibble 
fusion_table <- as_tibble(fusion_table)

########## Inspect data ###########
### TEST DATE
# visualizing Missing values
fusion_table_miss <- skim(fusion_table) %>%
                            select(skim_variable, n_missing)
Nobs=nrow(fusion_table)
fusion_table_miss <- fusion_table_miss %>% mutate(p_missing=n_missing/Nobs)
head(fusion_table_miss)
fusion_table_miss <- fusion_table_miss %>% arrange(-n_missing) # sort in descending order 
fusion_table_miss <- fusion_table_miss %>% filter(n_missing!= 0) # Keep only variables with missing 
ggplot(fusion_table_miss, aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) + # N missing per variable
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings")+ 
  theme(axis.text = element_text(size = 25))  

###Lidear con los missing
# P5140 and P5130 are the variables with the most missing values (60% and 40% respectively)
########## Descriptives ##########################################
### Change variables as numeric
fusion_table <- fusion_table %>% 
                mutate(across(c(p6020, p6040, p6210, pobre), as.numeric))
# Define the some variables as.factor, as.numeric.
var_factor <- fusion_table %>% 
                               select(depto, clase, pobre, p6210, p5090, p5000, p5010, indigente)
var_numeric <- fusion_table %>%
                 select(p6020, p6040, p6210, pobre)

ggplot(var_factor, aes(x = p6210)) +
                   geom_bar(fill="skyblue") +
                   labs(title = "Bar Plot of Education Level", x = "Education Level", y = "Count")

ggplot(var_numeric, aes(x = p6040)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")

###################### Models ################################
















# Check the number of observations in test_p and train_p
nrow(test_p)
nrow(train_p)








# 1.3. Keep only the relevant variables.
glimpse(dm)
head(dm[1:15])

dm <- m[, c("id", "Orden", "Clase.x", "Dominio.x", "Ingtot", "Estrato1", "P6020", "P6040",
               "P6090", "P6210", "P6210s1", "P6430", # general
               "Oc", "Des", "Ina", # Job 
               "P6500", "P6800", # Income
               "P6510", "P6585s1", "P6585s2", "P6585s3", "P6585s4", "P7495", 
               "P7505", "P7510s2", "P7510s3", "P7510s5", "P6870", # Other Income
               "Clase.y", "Dominio.y", "P5000", "P5010", "Npersug", "Ingtotugarr", #household variables 
               "Lp", "Pobre", "Li", "Indigente", "P5090", "Nper", "Depto.x", "Depto.y", "P5130", "P5140")]
dm <- dm %>% unite(unique_id,c(id, Orden, Clase.x, Clase.y))
#################################################
dm <- m %>% 
  select(id, Orden, Clase.x, Dominio.x, Ingtot, Estrato1, P6020, P6040,  
         P6090, P6210, P6210s1, P6430, # general
         Oc, Des, Ina, # Job
         P6500, P6800, # Income
         P6510, P6585s1, P6585s2, P6585s3, P6585s4, P7495, 
         P7505, P7510s2, P7510s3, P7510s5, P6870, # Other Income
         Clase.y, Dominio.y, P5000, P5010, Npersug, Ingtotugarr, # household variables 
         Lp, Pobre) %>%
  mutate(across(c(P6020, P6090, P6430, Oc, Des, Ina, P6510, P6585s1, P6585s2, P6585s3, P6585s4, P7495, 
                 P7505, P7510s2, P7510s3, P7510s5, P6870, Clase.x, Dominio.x, Pobre), as.factor),
         across(c(P6040, P6210s1, P6500, P6800, P5000, P5010, Npersug, Ingtotugarr, Lp), as.numeric))
################################################

dm <- dm %>%
             rename(sexo = p6020,
             edad = p6040,
             cotizante = p6090,
             educacion = p6210,
             tipo_trab = p6430,
             ing_salario = P6500,
             hor_trabo = P6800,
             ing_h_ext = p6510,
             aux_alim = p6585s1,
             aux_tras = p6585s2,
             aux_fam = p6585s3,
             sub_edu = p6585s4,
             ing_arr_pens = p7495,
             ing_fin = p7505,
             ing_internac = p7510s2,
             ing_gub = p7510s3,
             ing_otr_fin = p7510s5,
             tam_empresa = p6870,
             ocup_vivi = p5090,
             pagaria_arriendo = p5130, 
             paga_arriendo = p5140)

arrow: write_parquet(dm, sink="stores/train_db.parquet")
# 1.2 Prepare dataset train_hogares ----

