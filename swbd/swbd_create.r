# Extend df.pairs.rds to distance from 1 to 5
# Yang Xu
# 2/1/2016

library(doMC)
library(foreach)
library(data.table)
library(dplyr)


df = readRDS('df.pairs.rds')
dt = data.table(df, key = )