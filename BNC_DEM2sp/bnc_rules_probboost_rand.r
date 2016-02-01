# In this file, we replicate the results in PLOS One paper
# by computing probboost for syntactic rules, distance ranging from 1 to 10
# categorized by self vs other, and real vs random
# Yang Xu
# 1/26/2016

library(RMySQL)
library(data.table)
library(dplyr)
library(doMC)
library(foreach)
library(ggplot2)
library(lme4)

# include all functions from bnc_func.r
source('bnc_func.r')


# read df.ext, df.rand
df.ext = unique(readRDS('df.ext.rds'))
df.ext$chance = FALSE
dt.ext = data.table(df.ext)
setkey(dt.ext, xmlID, divIndex, primeTurnID, targetTurnID)

df.rand = unique(readRDS('df.rand.rds'))
dt.rand = data.table(df.rand)
setkey(dt.rand, xmlID, divIndex, primeTurnID, targetTurnID)

# read rulefreq.rds
df.rf = readRDS('df.rulefreq.rds')

# read dt.db.agg
dt.db.agg = readRDS('dt.db.agg.rds')

# differentiate high and low rule_ids
rule_ids_high = df.rf[1:30,]$ruleID
rule_ids_low = df.rf[31:150,]$ruleID

# some standards
# nrow(subset(df.rf, count > 2000)) # 30
# nrow(subset(df.rf, count > 1000)) # 82


# compute probBoost for each distance (1, ..., 10), each type (self, other)
system.time(pb_other_hi1 <- ruleProbBoost(subset(df.ext, type == 'other' & distance == 1), dt.db.agg, rule_ids_high)) # takes 9.5 sec
pb_other_hi3 <- ruleProbBoost(subset(df.ext, type == 'other' & distance == 3), dt.db.agg, rule_ids_high)
pb_other_hi5 <- ruleProbBoost(subset(df.ext, type == 'other' & distance == 5), dt.db.agg, rule_ids_high)
pb_other_hi7 <- ruleProbBoost(subset(df.ext, type == 'other' & distance == 7), dt.db.agg, rule_ids_high)
pb_other_hi9 <- ruleProbBoost(subset(df.ext, type == 'other' & distance == 9), dt.db.agg, rule_ids_high)

pb_other_lo1 <- ruleProbBoost(subset(df.ext, type == 'other' & distance == 1), dt.db.agg, rule_ids_low)
pb_other_lo3 <- ruleProbBoost(subset(df.ext, type == 'other' & distance == 3), dt.db.agg, rule_ids_low)
pb_other_lo5 <- ruleProbBoost(subset(df.ext, type == 'other' & distance == 5), dt.db.agg, rule_ids_low)
pb_other_lo7 <- ruleProbBoost(subset(df.ext, type == 'other' & distance == 7), dt.db.agg, rule_ids_low)
pb_other_lo9 <- ruleProbBoost(subset(df.ext, type == 'other' & distance == 9), dt.db.agg, rule_ids_low)

pb_self_hi2 <- ruleProbBoost(subset(df.ext, type == 'self' & distance == 2), dt.db.agg, rule_ids_high)
pb_self_hi4 <- ruleProbBoost(subset(df.ext, type == 'self' & distance == 4), dt.db.agg, rule_ids_high)
pb_self_hi6 <- ruleProbBoost(subset(df.ext, type == 'self' & distance == 6), dt.db.agg, rule_ids_high)
pb_self_hi8 <- ruleProbBoost(subset(df.ext, type == 'self' & distance == 8), dt.db.agg, rule_ids_high)
pb_self_hi10 <- ruleProbBoost(subset(df.ext, type == 'self' & distance == 10), dt.db.agg, rule_ids_high)

pb_self_lo2 <- ruleProbBoost(subset(df.ext, type == 'self' & distance == 2), dt.db.agg, rule_ids_low)
pb_self_lo4 <- ruleProbBoost(subset(df.ext, type == 'self' & distance == 4), dt.db.agg, rule_ids_low)
pb_self_lo6 <- ruleProbBoost(subset(df.ext, type == 'self' & distance == 6), dt.db.agg, rule_ids_low)
pb_self_lo8 <- ruleProbBoost(subset(df.ext, type == 'self' & distance == 8), dt.db.agg, rule_ids_low)
pb_self_lo10 <- ruleProbBoost(subset(df.ext, type == 'self' & distance == 10), dt.db.agg, rule_ids_low)

# construct df.plot
df.plot = data.frame(probBoost = c(pb_other_hi1, pb_other_hi3, pb_other_hi5, pb_other_hi7, pb_other_hi9, 
    pb_other_lo1, pb_other_lo3, pb_other_lo5, pb_other_lo7, pb_other_lo9, 
    pb_self_hi2, pb_self_hi4, pb_self_hi6, pb_self_hi8, pb_self_hi10, 
    pb_self_lo2, pb_self_lo4, pb_self_lo6, pb_self_lo8, pb_self_lo10), 
    distance = c(rep(rep(c(1,3,5,7,9), each = length(rule_ids_high)), 2), 
                rep(rep(c(2,4,6,8,10), each = length(rule_ids_low)), 2)), 
    freq = rep(c(rep('high', 5*length(rule_ids_high)), rep('low', 5*length(rule_ids_low))), 2), 
    type = c(rep('other', 5*length(rule_ids_high) + 5*length(rule_ids_low)), 
            rep('self', 5*length(rule_ids_high) + 5*length(rule_ids_low)))
)

# remove infinite values
df.plot[is.infinite(df.plot$probBoost),]$probBoost = NA

# save df.plot to rds
saveRDS(df.plot, 'df.plot.rds')

# read df.plot from rds
df.plot = readRDS('df.plot.rds')

# plot
p = ggplot(df.plot, aes(x = distance, y = probBoost, color = freq, lty = type)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar') + 
    stat_summary(fun.y = mean, geom = 'line')
pdf('probBoost.other_vs_self.high_vs_low.pdf')
plot(p)
dev.off()


# model
summary(lm(probBoost ~ distance, subset(df.plot, type == 'self' & freq == 'low')))
