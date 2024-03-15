##############################################################################-
# DATE:
#   2024/feb/10
# AUTHOR:
#   Gustavo Castilllo
# DESCRIPTION:
#   Explore dataset.
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
# End