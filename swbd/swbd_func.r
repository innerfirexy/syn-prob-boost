# Include all the functions called in swbd_rules_probboost.r
# Yang Xu
# 2/3/2016

library(data.table)
library(foreach)
library(doMC)

# func used in aggregating
dbRulesAgg = function(str) {
    s = paste(str, sep = ',')
    paste(as.character(unique(as.vector(unlist(strsplit(s, split = ','))))), collapse = ',')
}

# the func that adds two columns to dt.pairs, inPrime & inTarget,
# indicating whether a give rule_id is in prime and target, respectively
addInLabel = function(df.pairs, dt.db.agg, rule_id) {
    # setkey
    dt.pairs = data.table(df.pairs)
    setkey(dt.pairs, convID, primeTurnID, targetTurnID)

    # add the isRuleIn label to dt.db.agg, indicating whether the rule_id is in a turn (a row in dt.db.agg)

    # # first way
    # dt.db.agg$label = apply(dt.db.agg, 1, function(x) {
    #     as.character(rule_id) %in% unlist(strsplit(x[4], split = ','))
    # }) # takes 1.8* sec
    # dt.db.agg[, rules:=NULL]

    # second way, takes 1.568 sec
    dt.db.agg = dt.db.agg[, {
            rs = unlist(strsplit(rules, ','));
            r = as.character(rule_id);
            list(convID = convID, turnID = turnID, label = r %in% rs)
        }, by = 1:nrow(dt.db.agg)]
    dt.db.agg[, nrow:=NULL]
    setkey(dt.db.agg, convID, turnID)

    # merge dt.db.agg.labeled to dt.pairs sequentially
    prime_turns = dt.pairs[, list(convID, primeTurnID)]
    prime_turns = rename(prime_turns, turnID = primeTurnID)
    setkey(prime_turns, convID, turnID)

    target_turns = dt.pairs[, list(convID, targetTurnID)]
    target_turns = rename(target_turns, turnID = targetTurnID)
    setkey(target_turns, convID, turnID)

    unique_turns = unique(rbindlist(list(prime_turns, target_turns)))
    setkey(unique_turns, convID, turnID)

    # join from dt.db.agg to unique_turns
    unique_turns = unique_turns[dt.db.agg, nomatch = 0]

    # join and back to dt.pairs
    unique_turns = rename(unique_turns, primeTurnID = turnID)
    dt.pairs = dt.pairs[unique_turns, nomatch = 0]
    dt.pairs = rename(dt.pairs, inPrime = label)

    setkey(dt.pairs, convID, targetTurnID)
    unique_turns = rename(unique_turns, targetTurnID = primeTurnID)
    dt.pairs = dt.pairs[unique_turns, nomatch = 0]
    dt.pairs = rename(dt.pairs, inTarget = label)
    # return
    dt.pairs
}

# the func that computes the probability boost for a single or a series of rule IDs
ruleProbBoost = function(dt.pairs, dt.db.agg, rule_ids) {
    registerDoMC(detectCores())
    res = foreach(rule_id = rule_ids, .combine = c) %dopar% {
        dt.labeled = addInLabel(dt.pairs, dt.db.agg, rule_id)
        prior = nrow(subset(dt.labeled, inTarget)) / nrow(dt.labeled)
        cond = nrow(subset(dt.labeled, inTarget & inPrime)) / nrow(subset(dt.labeled, inPrime))
        log(cond) - log(prior) # log odds ratio
    }
    res
}
