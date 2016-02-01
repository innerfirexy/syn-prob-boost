# Replicate the results in the PLOS one paper (PGT Healey, 2014) using BNC-DEM 2speaker
# Yang Xu
# 1/13/2016

library(doMC)
library(foreach)
library(R.utils)
library(ggplot2)
library(reshape)


df = readRDS('df.pairs.rds')


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
                d.prime = subset(data, primeTurnID %in% other_prime_tids)[,c(1:17)]
                d.prime = d.prime[!duplicated(d.prime),]
                d.target = data[rep(i, length(other_prime_tids)), c(18:31)]
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
                d.prime = subset(data, primeTurnID %in% self_prime_tids)[,c(1:17)]
                d.prime = d.prime[!duplicated(d.prime),]
                d.target = data[rep(i, length(self_prime_tids)), c(18:31)]
                d = cbind(d.prime, d.target)
                d$distance = d$targetTurnID - d$primeTurnID
                d$type = 'self'
                res = rbind(res, d)
            }
        }
    }
    res
})
# takes 295.545 sec

# save df.ext to rds
saveRDS(df.ext, 'df.ext.rds')

# read from rds
df.ext = readRDS('df.ext.rds')


# coorTargetAgg3_byMarker, same function as above
# except that it implements the Aggregate 3 method 
# compute for each value in distance column
coorTargetAgg3_byMarker = function(data) {
    markers = c('art', 'auxv', 'conj', 'adv', 'ipron', 'ppron', 'prep', 'quant')
    registerDoMC(detectCores())
    data.split = split(data, data$targetSpeakerOrig)
    results = foreach(d = data.split, .combine = rbind) %dopar% {
        res = data.frame()
        for (t in unique(d$type)) {
            ds = subset(d, type == t)
            for (dist in unique(ds$distance)) {
                dss = subset(ds, distance == dist)
                # compute the probBoost for each marker
                coor = c()
                for (m in markers) {
                    p_col = which(colnames(dss) == paste0('prime', capitalize(m)))
                    t_col = which(colnames(dss) == paste0('target', capitalize(m)))
                    p_prior = nrow(subset(dss, dss[,t_col] > 0)) / nrow(dss)
                    p_cond = nrow(subset(dss, dss[,t_col] > 0 & dss[,p_col] > 0)) / nrow(subset(dss, dss[,p_col] > 0))
                    coor = c(coor, p_cond - p_prior)
                }
                nan_count = length(which(is.nan(coor)))
                if (nan_count < length(coor)) {
                    if (nan_count > 0) {
                        coor[which(is.nan(coor))] = mean(coor[which(!is.nan(coor))])
                    }
                    coor = as.data.frame(matrix(coor, ncol = length(coor)))
                    colnames(coor) = markers
                    # add distance and type column
                    coor$distance = dist
                    coor$type = t
                } else {
                    coor = NA
                }
                # append to res
                res = rbind(res, coor)
            }
        }
        res
    }
    results[complete.cases(results),]
}

# coorGrossMean, the function that does similar thing to coorTargetAgg1,
# except that it does not calculate by distinguished users, but for all usrs as a whole
# we let it return 8 values, corresponding to the mean values of (cond - prior) for the 8 markers
coorGrossMean = function(d) {
    markers = c('art', 'auxv', 'conj', 'adv', 'ipron', 'ppron', 'prep', 'quant')
    results = c()
    for (m in markers) {
        p_col = which(colnames(d) == paste0('prime', capitalize(m)))
        t_col = which(colnames(d) == paste0('target', capitalize(m)))
        p_prior = nrow(subset(d, d[,t_col] > 0)) / nrow(d)
        p_cond = nrow(subset(d, d[,t_col] > 0 & d[,p_col] > 0)) / nrow(subset(d, d[,p_col] > 0))
        results = c(results, p_cond - p_prior)
    }
    results
}


## Use coorTargetAgg3_byMarker
system.time(cta3 <- coorTargetAgg3_byMarker(df.ext)) # takes 11 sec

# plot
cta3.art.plot = ggplot(cta3, aes(x = distance, y = art, color = type, lty = type)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = .2) + 
    stat_summary(fun.y = mean, geom = 'line')
plot(cta3.art.plot)

cta3$mean = apply(cta3, 1, function(x) {
    mean(as.numeric(x[1:8]))
})
cta3.mean.plot = ggplot(cta3, aes(x = distance, y = mean, color = type, lty = type)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = .2) + 
    stat_summary(fun.y = mean, geom = 'line')
plot(cta3.mean.plot)

# conclusion:
# if we use coorTarget*, we face the data sparse problem



## Use coorGrossMean
cg.df = data.frame()
markers = c('art', 'auxv', 'conj', 'adv', 'ipron', 'ppron', 'prep', 'quant')
system.time(for (t in unique(df.ext$type)) {
    ds = subset(df.ext, type == t)
    for (dist in unique(ds$distance)) {
        dss = subset(ds, distance == dist)
        res = coorGrossMean(dss)
        res = as.data.frame(matrix(res, ncol = 8))
        colnames(res) = markers
        res$distance = dist
        res$type = t
        cg.df = rbind(cg.df, res)
    }
}) # takes 5 sec

cg.df.melt = melt(cg.df, id = c('distance', 'type'))
colnames(cg.df.melt) = c('distance', 'type', 'marker', 'probBoost')

cg.plot = ggplot(cg.df.melt, aes(x = distance, y = probBoost, color = marker, lty = type)) + 
    geom_line()
plot(cg.plot)



## resample func used in randomize
resample = function(x) {
    x[sample.int(length(x))]
}


## randomize operation, to add Chance pairs
randomize = function(data, df.ref) {
    conv_id = data$convID[1]
    other_sample_base = subset(df.ref, convID != conv_id)
    res = data.frame()
    # get the randomized self turnID sequences for the two speakers respectively
    speaker1 = unique(data$targetSpeaker)[1]
    sub1 = subset(data, targetSpeaker == speaker1 & type == 'self')
    self_tid_seq1 = c(resample(setdiff(unique(sub1$primeTurnID), unique(sub1$targetTurnID))), 
        resample(intersect(unique(sub1$primeTurnID), unique(sub1$targetTurnID))))
    speaker2 = unique(data$targetSpeaker)[2]
    sub2 = subset(data, targetSpeaker == speaker2 & type == 'self')
    self_tid_seq2 = c(resample(setdiff(unique(sub2$primeTurnID), unique(sub2$targetTurnID))), 
        resample(intersect(unique(sub2$primeTurnID), unique(sub2$targetTurnID))))
    # re-construct
    for (target_tid in unique(data$targetTurnID)) {
        # check if target_tid is in self_tids
        speaker = subset(data, targetTurnID == target_tid)$targetSpeaker[1]
        if (speaker == speaker1) {
            self_tid_seq = self_tid_seq1
        } else {
            self_tid_seq = self_tid_seq2
        }
        if (target_tid %in% self_tid_seq) {
            tid_idx = which(self_tid_seq == target_tid)
            if (tid_idx > 5) {
                tryCatch({
                    # target part
                    target = subset(data, targetTurnID == target_tid)[c(1:5),c(18:31)]
                    # sample prime for chance self
                    self_tids = self_tid_seq[seq(tid_idx-1, tid_idx-5, -1)]
                    self.prime = unique(subset(data, primeTurnID %in% self_tids)[,c(1:17)])
                    self = cbind(self.prime, target)
                    self$distance = seq(2, 10, 2)
                    self$type = 'self'
                    # sample prime for chance other
                    other.prime = other_sample_base[sample(nrow(other_sample_base), 5), c(1:17)]
                    other = cbind(other.prime, target)
                    other$distance = seq(1, 9, 2)
                    other$type = 'other'
                    # append
                    res = rbind(res, rbind(other, self))
                }, 
                error = function(e) {
                    # handle error
                    print(paste0('ERROR convID: ', conv_id))
                    print(paste0('ERROR target_tid: ', target_tid))
                    print(as.character(e))
                })
            }
        } else { # sample from chance other only
            target = subset(data, targetTurnID == target_tid)[c(1:5),c(18:31)]
            other.prime = other_sample_base[sample(nrow(other_sample_base), 5), c(1:17)]
            other = cbind(other.prime, target)
            other$distance = seq(1, 9, 2)
            other$type = 'other'
            res = rbind(res, other)
        }
    }
    # add chance column
    res$chance = TRUE
    res
}

# add randomized part
df.ext.split = split(df.ext, df.ext$convID)
registerDoMC(detectCores())
system.time(df.rand <- foreach(data = df.ext.split, .combine = rbind) %dopar% randomize(data, df.ext)) # takes 142 sec

# save df.rand to rds
saveRDS(df.rand, 'df.rand.rds')

# read df.rand from rds
df.rand = readRDS('df.rand.rds')



## combine df.ext and df.rand, and then plot
df.ext$chance = FALSE
df.full = rbind(df.ext, df.rand)

cg.full = data.frame()
markers = c('art', 'auxv', 'conj', 'adv', 'ipron', 'ppron', 'prep', 'quant')
system.time(for (t in unique(df.full$type)) {
    ds = subset(df.full, type == t)
    for (c in c(TRUE, FALSE)) {
        dss = subset(ds, chance == c)
        for (dist in unique(ds$distance)) {
            dsss = subset(dss, distance == dist)
            res = coorGrossMean(dsss)
            res = as.data.frame(matrix(res, ncol = 8))
            colnames(res) = markers
            res$distance = dist
            res$chance = c
            res$type = t
            cg.full = rbind(cg.full, res)
        }
    }
}) # takes 11 sec

cg.full.melt = melt(cg.full, id = c('distance', 'type', 'chance'))
colnames(cg.full.melt) = c('distance', 'type', 'chance', 'marker', 'probBoost')

cg.full.plot = ggplot(subset(cg.full.melt, type == 'other'), aes(x = distance, y = probBoost, color = marker, lty = chance)) + 
    geom_line() + geom_point(shape = 1, aes(size = 2)) + scale_x_continuous(breaks = seq(1,9,2))
pdf('cg.other.chance_vs_real.pdf')
plot(cg.full.plot)
dev.off()

cg.full.plot2 = ggplot(subset(cg.full.melt, type == 'self'), aes(x = distance, y = probBoost, color = marker, lty = chance)) + 
    geom_line() + geom_point(shape = 4, aes(size = 2)) + scale_x_continuous(breaks = seq(2,10,2))
pdf('cg.self.chance_vs_real.pdf')
plot(cg.full.plot2)
dev.off()

cg.full.plot3 = ggplot(cg.full.melt, aes(x = distance, y = probBoost, color = marker, lty = chance, shape = type)) + 
    geom_line() + geom_point(aes(size = 2)) + 
    scale_shape_manual(values = c(1, 4)) + 
    scale_x_continuous(breaks = seq(1,10,1))
pdf('cg.other_vs_self.chance_vs_real.pdf')
plot(cg.full.plot3)
dev.off()