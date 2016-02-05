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

# read df.ext.rds
df.ext = readRDS('df.ext.rds')
dt.ext = data.table(df.ext)
setkey(dt.ext, convID, primeTurnID, targetTurnID)

# define high and low freq ruleIDs
rule_ids_high = df.rf[1:27,]$ruleID
rule_ids_low = df.rf[28:100,]$ruleID

# compute probBoost for distance from 1 to 10, and for each type (self, other)
system.time(pb_other_hi1 <- ruleProbBoost(subset(dt.ext, type == 'other' & distance == 1), dt.db.agg, rule_ids_high)) # takes 9.5 sec
pb_other_hi3 <- ruleProbBoost(subset(dt.ext, type == 'other' & distance == 3), dt.db.agg, rule_ids_high)
pb_other_hi5 <- ruleProbBoost(subset(dt.ext, type == 'other' & distance == 5), dt.db.agg, rule_ids_high)
pb_other_hi7 <- ruleProbBoost(subset(dt.ext, type == 'other' & distance == 7), dt.db.agg, rule_ids_high)
pb_other_hi9 <- ruleProbBoost(subset(dt.ext, type == 'other' & distance == 9), dt.db.agg, rule_ids_high)

pb_other_lo1 <- ruleProbBoost(subset(dt.ext, type == 'other' & distance == 1), dt.db.agg, rule_ids_low)
pb_other_lo3 <- ruleProbBoost(subset(dt.ext, type == 'other' & distance == 3), dt.db.agg, rule_ids_low)
pb_other_lo5 <- ruleProbBoost(subset(dt.ext, type == 'other' & distance == 5), dt.db.agg, rule_ids_low)
pb_other_lo7 <- ruleProbBoost(subset(dt.ext, type == 'other' & distance == 7), dt.db.agg, rule_ids_low)
pb_other_lo9 <- ruleProbBoost(subset(dt.ext, type == 'other' & distance == 9), dt.db.agg, rule_ids_low)

pb_self_hi2 <- ruleProbBoost(subset(dt.ext, type == 'self' & distance == 2), dt.db.agg, rule_ids_high)
pb_self_hi4 <- ruleProbBoost(subset(dt.ext, type == 'self' & distance == 4), dt.db.agg, rule_ids_high)
pb_self_hi6 <- ruleProbBoost(subset(dt.ext, type == 'self' & distance == 6), dt.db.agg, rule_ids_high)
pb_self_hi8 <- ruleProbBoost(subset(dt.ext, type == 'self' & distance == 8), dt.db.agg, rule_ids_high)
pb_self_hi10 <- ruleProbBoost(subset(dt.ext, type == 'self' & distance == 10), dt.db.agg, rule_ids_high)

pb_self_lo2 <- ruleProbBoost(subset(dt.ext, type == 'self' & distance == 2), dt.db.agg, rule_ids_low)
pb_self_lo4 <- ruleProbBoost(subset(dt.ext, type == 'self' & distance == 4), dt.db.agg, rule_ids_low)
pb_self_lo6 <- ruleProbBoost(subset(dt.ext, type == 'self' & distance == 6), dt.db.agg, rule_ids_low)
pb_self_lo8 <- ruleProbBoost(subset(dt.ext, type == 'self' & distance == 8), dt.db.agg, rule_ids_low)
pb_self_lo10 <- ruleProbBoost(subset(dt.ext, type == 'self' & distance == 10), dt.db.agg, rule_ids_low)

# the whole computation takes 626 sec on brain

# construct df.plot
df.plot = data.frame(probBoost = c(pb_other_hi1, pb_other_hi3, pb_other_hi5, pb_other_hi7, pb_other_hi9,
    pb_other_lo1, pb_other_lo3, pb_other_lo5, pb_other_lo7, pb_other_lo9,
    pb_self_hi2, pb_self_hi4, pb_self_hi6, pb_self_hi8, pb_self_hi10,
    pb_self_lo2, pb_self_lo4, pb_self_lo6, pb_self_lo8, pb_self_lo10),
    distance = c(rep(c(1,3,5,7,9), each = length(rule_ids_high)),
                rep(c(1,3,5,7,9), each = length(rule_ids_low)),
                rep(c(2,4,6,8,10), each = length(rule_ids_high)),
                rep(c(2,4,6,8,10), each = length(rule_ids_low))),
    freq = rep(c(rep('high', 5*length(rule_ids_high)), rep('low', 5*length(rule_ids_low))), 2),
    type = c(rep('other', 5*length(rule_ids_high) + 5*length(rule_ids_low)),
            rep('self', 5*length(rule_ids_high) + 5*length(rule_ids_low)))
)

# remove infinite values
if (nrow(subset(df.plot, is.infinite(probBoost))) > 0) {
    df.plot[is.infinite(df.plot$probBoost),]$probBoost = NA
}

# save df.plot to rds
saveRDS(df.plot, 'df.plot.rds')


## plot
df.plot = readRDS('df.plot.rds')
p = ggplot(df.plot, aes(x = distance, y = probBoost)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', aes(lty = type, color = freq)) +
    stat_summary(fun.y = mean, geom = 'line', aes(lty = type, color = freq)) +
    scale_x_continuous(breaks = 1:10)
pdf('probBoost.other_vs_self.high_vs_low.pdf')
plot(p)
dev.off()
