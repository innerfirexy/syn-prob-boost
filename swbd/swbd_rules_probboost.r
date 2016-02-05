# syntactic rules probability boost analysis of Switchboard
# Yang Xu
# 2/3/2016

library(RMySQL)
library(data.table)
library(dplyr)
library(doMC)
library(foreach)
library(ggplot2)

# include swbd_func.r
source('swbd_func.r')

# # read df.db from entropy table of swbd db
# # ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
# conn = dbConnect(MySQL(), host = '127.0.0.1', user = 'yang', port = 1234, password = "05012014", dbname = 'swbd')
# sql = 'SELECT convID, turnID, subRulesID FROM entropy'
# df.db = dbGetQuery(conn, sql)
#
# # compute dt.db.agg
# dt.db = data.table(df.db)
# setkey(dt.db, convID, turnID)
# system.time(dt.db.agg <- dt.db[, .(rules = dbRulesAgg(subRulesID)), by = .(convID, turnID)])
# # takes 5 sec
#
# # save dt.db.agg to rds
# saveRDS(dt.db.agg, 'dt.db.agg.rds')

# read dt.db.agg.rds
dt.db.agg = readRDS('dt.db.agg.rds')

# read df.rulefreq.rds
df.rf = readRDS('df.rulefreq.rds')

# define high and low freq ruleIDs
