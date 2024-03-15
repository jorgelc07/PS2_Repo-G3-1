##############################################################################-
# DATE:
#   2024/march/14
# AUTHOR:
#   Gustavo Castilllo
# DESCRIPTION:
#   Train a simple logit model.
##############################################################################-
rm(list = ls())
source("scripts/00_packages.R")
gc()

# Prepre
db <- arrow::read_parquet("stores/train_db.parquet")

tabyl(db$Pobre)
# There are 
# 0        1 
# 74.86306 25.13694
