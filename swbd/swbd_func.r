# Include all the functions called in swbd_rules_probboost.r
# Yang Xu
# 2/3/2016

library(data.table)
library(foreach)
library(doMC)

# func used in aggregating
dbRulesAgg = function(dt) {
    s = paste(dt$subRulesID, sep = ',')
    paste(as.character(unique(as.vector(unlist(strsplit(s, split = ','))))), collapse = ',')
}
