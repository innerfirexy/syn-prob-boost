# Extend df.pairs.rds to distance from 1 to 5
# Yang Xu
# 2/1/2016

library(doMC)
library(foreach)
library(data.table)
library(dplyr)


df = readRDS('df.pairs.rds')
dt = data.table(df, key = c('convID', 'primeTurnID', 'targetTurnID'))

# split by convID
df.split = split(df, df$convID)
# registerDoMC
registerDoMC(detectCores())

# extend the distance from 1 to 5, and add self alignment pairs
system.time(df.ext <- foreach(data = df.split, .combine = rbind) %dopar% {
    res = data.frame()
    for (i in nrow(data):1) {
        target_tid = data[i,]$targetTurnID
        target_speaker = data[i,]$targetSpeaker
        # get five previous turnIDs from the other speaker
        # and check if they are indeed from the other speaker
        other_prime_tids = seq(target_tid-1, target_tid-1-8, -2)
        if (all(other_prime_tids %in% data$primeTurnID)) {
            other_speaker = unique(subset(data, primeTurnID %in% other_prime_tids)$primeSpeaker)
            if (length(other_speaker) == 1 & other_speaker != target_speaker) {
                d.prime = subset(data, primeTurnID %in% other_prime_tids)[,c(1:4)]
                d.prime = d.prime[!duplicated(d.prime),]
                d.target = data[rep(i, length(other_prime_tids)), c(5:7)]
                d = cbind(d.prime, d.target)
                d$distance = d$targetTurnID - d$primeTurnID
                d$type = 'other'
                res = rbind(res, d)
            }
        }
        # get five previous turnIDs from the same speaker as target_speaker
        # and check if they are indeed from the same speaker
        self_prime_tids = seq(target_tid-2, target_tid-2-8, -2)
        if (all(self_prime_tids %in% data$primeTurnID)) {
            self_speaker = unique(subset(data, primeTurnID %in% self_prime_tids)$primeSpeaker)
            if (length(self_speaker) == 1 & self_speaker == target_speaker) {
                d.prime = subset(data, primeTurnID %in% self_prime_tids)[,c(1:4)]
                d.prime = d.prime[!duplicated(d.prime),]
                d.target = data[rep(i, length(self_prime_tids)), c(5:7)]
                d = cbind(d.prime, d.target)
                d$distance = d$targetTurnID - d$primeTurnID
                d$type = 'self'
                res = rbind(res, d)
            }
        }
    }
    res
})
# elapse 291 sec

# save df.ext to rds
saveRDS(df.ext, 'df.ext.rds')
