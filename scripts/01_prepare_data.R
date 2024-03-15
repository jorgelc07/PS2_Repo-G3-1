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


############################################################################-
# 1. Append house and household databases ----
############################################################################-
train_personas <- fread("stores/raw/train_personas.csv")
train_hogares <- fread("stores/raw/train_hogares.csv")

m <- merge.data.table(x = train_personas, y = train_hogares,
                      by = 'id', all = TRUE)
arrow::write_parquet(m, sink = "stores/train_db.parquet")


test_personas <- fread("stores/raw/test_personas.csv")
test_hogares <- fread("stores/raw/test_hogares.csv")

# End