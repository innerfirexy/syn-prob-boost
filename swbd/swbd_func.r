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

# the func that adds two columns to dt.pairs, inPrime & inTarget,
# indicating whether a give rule_id is in prime and target, respectively
addInLabel = function(df.pairs, dt.db.agg, rule_id) {
    # setkey
    dt.pairs = data.table(df.pairs)
    setkey(dt.pairs, xmlID, divIndex, primeTurnID, targetTurnID)

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
            list(xmlID = xmlID, divIndex = divIndex, turnID = turnID, label = r %in% rs)
        }, by = 1:nrow(dt.db.agg)]
    dt.db.agg[, nrow:=NULL]
    setkey(dt.db.agg, xmlID, divIndex, turnID)

    # merge dt.db.agg.labeled to dt.pairs sequentially
    prime_turns = dt.pairs[, list(xmlID, divIndex, primeTurnID)]
    prime_turns = rename(prime_turns, turnID = primeTurnID)
    setkey(prime_turns, xmlID, divIndex, turnID)

    target_turns = dt.pairs[, list(xmlID, divIndex, targetTurnID)]
    target_turns = rename(target_turns, turnID = targetTurnID)
    setkey(target_turns, xmlID, divIndex, turnID)

    unique_turns = unique(rbindlist(list(prime_turns, target_turns)))
    setkey(unique_turns, xmlID, divIndex, turnID)

    # merge from dt.db.agg to unique_turns
    unique_turns = merge(unique_turns, dt.db.agg)

    # rename and merge back to dt.pairs
    unique_turns = rename(unique_turns, primeTurnID = turnID)
    dt.pairs = merge(dt.pairs, unique_turns)
    dt.pairs = rename(dt.pairs, inPrime = label)

    setkey(dt.pairs, xmlID, divIndex, targetTurnID)
    unique_turns = rename(unique_turns, targetTurnID = primeTurnID)
    dt.pairs = merge(dt.pairs, unique_turns)
    dt.pairs = rename(dt.pairs, inTarget = label)

    dt.pairs
}
