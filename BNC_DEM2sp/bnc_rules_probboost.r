# Apply probability boost analysis on syntactic rules, BNC
# Yang Xu
# 1/18/2016

library(RMySQL)
library(data.table)
library(dplyr)
library(doMC)
library(foreach)
library(ggplot2)

# include all functions from bnc_func.r
source('bnc_func.r')


# read df.db from the DEM_2spkr table of bnc db
# ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
conn = dbConnect(MySQL(), host = '127.0.0.1', user = 'yang', port = 1234, password = "05012014", dbname = 'bnc')
sql = 'SELECT xmlID, divIndex, turnID, subRulesID FROM DEM_2spkr WHERE subRulesID IS NOT NULL'
df.db = dbGetQuery(conn, sql)

# read df.pairs
df.pairs = unique(readRDS('df.pairs.rds'))
dt.pairs = data.table(df.pairs)
setkey(dt.pairs, xmlID, divIndex, primeTurnID, targetTurnID)


# convert to data.table
DT.db = data.table(df.db)
system.time(setkey(DT.db, xmlID, divIndex, turnID)) # 0.002 sec

# aggregate
system.time(dt.db.agg <- DT.db[, dbRulesAgg(.SD), by = list(xmlID, divIndex, turnID)])
# takes 2.8 sec -- WAY FASTER than aggregating a data frame!
dt.db.agg = rename(dt.db.agg, rules = V1)

# # save dt.db.agg to rds
# saveRDS(dt.db.agg, 'dt.db.agg.rds')


# read rulefreq.rds
df.rf = readRDS('df.rulefreq.rds')

## compute prob boost for high and low freq ruleIDs, respectively
rule_ids_high = subset(df.rf, freq == 'high')$ruleID
rule_ids_low = subset(df.rf, freq == 'low')$ruleID

rule_ids_low1 = rule_ids_low[1:100]
rule_ids_low2 = rule_ids_low[101:200]
rule_ids_low3 = rule_ids_low[201:300]
rule_ids_low4 = rule_ids_low[301:400]

system.time(pb_high <- ruleProbBoost(dt.pairs, dt.db.agg, rule_ids_high)) # single core: 109 sec, 4-core: 52.5 sec
system.time(pb_low1 <- ruleProbBoost(dt.pairs, dt.db.agg, rule_ids_low1)) # 4-core: 76 sec
system.time(pb_low2 <- ruleProbBoost(dt.pairs, dt.db.agg, rule_ids_low2)) # 4-core: 80 sec
system.time(pb_low3 <- ruleProbBoost(dt.pairs, dt.db.agg, rule_ids_low3)) # 4-core: 77 sec
system.time(pb_low4 <- ruleProbBoost(dt.pairs, dt.db.agg, rule_ids_low4)) # 4-core: 90 sec


## plot
df.plot = data.frame(probBoost = c(pb_high, pb_low1, pb_low2, pb_low3, pb_low4), 
    freq = c(rep('high', length(pb_high)), rep('low1', length(pb_low1)), rep('low2', length(pb_low2)),
        rep('low3', length(pb_low3)), rep('low4', length(pb_low4))))
# convert -Inf to NA
df.plot[is.infinite(df.plot$probBoost),]$probBoost = NA
cc = scales::seq_gradient_pal("blue", "grey", "Lab")(seq(0,1,length.out=5))
p = ggplot(df.plot, aes(x = freq, y = probBoost, fill = freq, color = freq)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', aes(width = .25)) + 
    stat_summary(fun.y = mean, geom = 'bar', aes(width = .5)) + 
    scale_fill_manual(values = cc) + scale_color_manual(values = cc)
pdf('probBoost_bar_binfreq.pdf')
plot(p)
dev.off()

# model
summary(lm(probBoost ~ freq, df.plot))


## probBoost ~ freq, for each freq group: high, low1, low2,...
df.high = data.frame(count = df.rf$count[1:70], probBoost = pb_high, group = 'high')
summary(lm(probBoost ~ log(count), df.high[1:70,])) # t = -1.30, p = 0.198

df.low1 = data.frame(count = subset(df.rf, freq == 'low')$count[1:100], probBoost = pb_low1, group = 'low1')
summary(lm(probBoost ~ log(count), rbind(df.high, df.low1)[1:71,])) # t = -2.039*
# more frequent rules get lower probBoost


df.low2 = data.frame(count = subset(df.rf, freq == 'low')$count[101:200], probBoost = pb_low2, group = 'low2')
df.low3 = data.frame(count = subset(df.rf, freq == 'low')$count[201:300], probBoost = pb_low3, group = 'low3')
df.low4 = data.frame(count = subset(df.rf, freq == 'low')$count[301:400], probBoost = pb_low4, group = 'low4')

# combine high all the way down to low4
df.highlow = rbind(df.high, df.low1, df.low2, df.low3, df.low4)
# convert Inf to NA
df.highlow[is.infinite(df.highlow$probBoost),]$probBoost = NA

summary(lm(probBoost ~ log(count), df.highlow)) # t = 3.403***
# more frequent rules get higher probability boost

# merge with df.rf
df.highlow = cbind(df.highlow, df.rf[1:470, c('ruleID', 'ruleStr')])

df.highlow$floorlogcount = floor(log(df.highlow$count))
df.highlow[df.highlow$floorlogcount > 8,]$floorlogcount = 8
p2 = ggplot(df.highlow[,], aes(x = floorlogcount, y = probBoost)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar') + 
    stat_summary(fun.y = mean, geom = 'line') + 
    scale_x_reverse() + 
    xlab('logarithm of count')
pdf('probBoost_vs_floorlogcount_raw.pdf')
plot(p2)
dev.off()

p3 = ggplot(subset(df.highlow, !ruleID %in% c(5, 14)), aes(x = floorlogcount, y = probBoost)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar') + 
    stat_summary(fun.y = mean, geom = 'line') + 
    scale_x_reverse() + 
    xlab('logarithm of count')
pdf('probBoost_vs_floorlogcount_clean.pdf')
plot(p3)
dev.off()


## frequency of PO and DO constructions
# PO
subset(df.rf, ruleStr == 'VP -> NP PP') # count 3
# DO
subset(df.rf, ruleStr == 'VP -> NP NP') # count 0


## negative probability boost?
# examples:
head(subset(df.highlow, probBoost < 0)) # 95
# NP -> ITJ PUN
# ADJP -> QP
# NP -> ITJ

# positive probBoost examples
head(subset(df.highlow, probBoost > 0))
# S -> NP VP PUN
# NP -> NP PP
# SBARQ -> WHNP SQ PUN
# S -> NP VP