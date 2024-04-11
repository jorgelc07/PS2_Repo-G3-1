##############################################################################-
# DATE:
#   2024/march/14
# AUTHOR:
#   Gustavo Castilllo
# DESCRIPTION:
#   Merge the training datasets and export 
##############################################################################-

# Prepare workspace
source("scripts/00_packages.R")
gc()

# Set directory paths
here()
# Change following path to where "uniandes-bdml-202410-ps2" directory is. This
# is directory is what is directly obtained when decompressing the .zip file
# downloaded from the competition data page 
# https://www.kaggle.com/competitions/uniandes-bdml-202410-ps2/data
raw_dir <- here("../raw/") 

############################################################################-
# 1. Append house and household databases ----
############################################################################-

train_personas <- fread(here(paste0(raw_dir,"train_personas.csv")))
train_hogares <- fread(here(paste0(raw_dir,"train_hogares.csv")))

test_personas <- fread(here(paste0(raw_dir,"test_personas.csv")))
test_hogares <- fread(here(paste0(raw_dir,"test_hogares.csv")))



tpers <- data.frame(varname=names(train_personas),
                    in_test = ifelse(names(train_personas) %in% names(test_personas),
                                     "yes","no")
                    )

thog <- data.frame(varname=names(train_hogares),
                   in_test = ifelse(names(train_hogares) %in% names(test_hogares),
                                    "yes","no")
)

# Create a blank workbook
OUT <-openxlsx::createWorkbook()
# Add some sheets to the workbook
openxlsx::addWorksheet(OUT, "personas")
openxlsx::addWorksheet(OUT, "hogares")
# Write data to sheets
openxlsx::writeData(wb = OUT, sheet = 'personas', x = tpers)
openxlsx::writeData(wb = OUT, sheet = 'hogares', x = thog)

openxlsx::saveWorkbook(OUT,here("../raw/dataset_variables.xlsx"), overwrite = T)



# End