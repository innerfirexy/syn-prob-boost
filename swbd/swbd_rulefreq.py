#!/usr/local/bin/python3
# Extract syntactic rules from the entropy table in swbd db
# Yang Xu
# 2/2/2016

import MySQLdb
import sys
import pickle

from nltk.tree import *
from nltk.probability import FreqDist

# get db connection
def db_conn(db_name):
    # db init: ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
    conn = MySQLdb.connect(host = "127.0.0.1",
                    user = "yang",
                    port = 3306,
                    passwd = "05012014",
                    db = db_name)
    return conn

# get all sub-rules of a parsing tree
def subrules(tree_str, exclude = None):
    """
    tree_str: a str, parsing tree of a sentence
    exclude: a list of str,
    """
    if tree_str is not None:
        try:
            tree = Tree.fromstring(tree_str)
        except Exception as e:
            print('tree_str: ' + str(tree_str))
            raise e
        subtrees = list(tree.subtrees(filter = lambda t: t.height() > 2))
        rules = [str(s.productions()[0]) for s in subtrees]
        rules = [r.replace(', ', '') for r in rules] # remove the commas in rules
    else:
        rules = []
    # exclude
    if exclude is not None:
        rules = [r for r in rules if r not in exclude]
    return rules

# extract rules from the `parsed` column of entropy table
# and update the subrules to `subRules` column
def extract_rules():
    conn = db_conn('swbd')
    cur = conn.cursor()

    # read primary keys
    sql = 'SELECT convID, globalID FROM entropy'
    cur.execute(sql)
    keys = cur.fetchall()

    # initiate dic object to store sub-rules and their frequency
    rules_dic = FreqDist()

    # extract sub-rules from each sentence
    for i, key in enumerate(keys):
        conv_id, g_id = key
        sql = 'SELECT parsed FROM entropy WHERE convID = %s AND globalID = %s'
        cur.execute(sql, (conv_id, g_id))
        parsed_str = cur.fetchone()[0]
        # extract rules
        try:
            rules = subrules(parsed_str)
        except Exception as e:
            print('convID: {}, globalID: {}'.format(conv_id, g_id))
            raise e
        # update subRules column
        if len(rules) > 0:
            rules_str = '~~~+~~~'.join(rules)
            sql = 'UPDATE entropy SET subRules = %s WHERE convID = %s AND globalID = %s'
            cur.execute(sql, (rules_str, conv_id, g_id))
        # update rules_dic
        for r in rules:
            rules_dic[r] += 1
        # print progress
        if (i % 99 == 0 and i > 0) or i == len(keys):
            sys.stdout.write('\r{}/{} updated'.format(i+1, len(keys)))
            sys.stdout.flush()
            conn.commit()
        # dump rules_dic
        pickle.dump(rules_dic, open('subrules.txt', 'wb'))

# write rules to entropy_ruleFreq table
def write_rules2db():
    conn = db_conn('swbd')
    cur = conn.cursor()
    # create entropy_ruleFreq table
    sql = 'CREATE TABLE entropy_ruleFreq (ruleID INT, ruleStr LONGTEXT, count INT, PRIMARY KEY(ruleID))'
    cur.execute(sql)
    # load the pickled dic
    dic = pickle.load(open('subrules.txt', 'rb'))
    # write the key, val pairs to db
    # before that sort the items of dic by count of sub-rules
    rule_id = 1
    for key, val in sorted(dic.items(), key = lambda item: item[1], reverse = True):
        sql = 'INSERT INTO entropy_ruleFreq VALUES(%s, %s, %s)'
        cur.execute(sql, (rule_id, key, val))
        rule_id += 1
    # commit
    conn.commit()


# main
if __name__ == '__main__':
    # extract_rules()
    write_rules2db()