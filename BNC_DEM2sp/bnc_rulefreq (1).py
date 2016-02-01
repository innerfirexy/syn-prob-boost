#!/usr/local/bin/python3
# A survey of the syntactic rules in BNC-DEM (2 speakers)
# Yang Xu

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


# extract rules
def extract_rules():
    # db conn
    conn = db_conn('bnc')
    cur = conn.cursor()

    # read keys
    sql = 'SELECT xmlID, divIndex, globalID FROM DEM_2spkr'
    cur.execute(sql)
    keys = cur.fetchall()

    # rules_dic
    rules_dic = FreqDist()

    # extract subrules for each sentence
    for i, key in enumerate(keys):
        xml_id, div_idx, g_id = key
        sql = 'SELECT parsedC5 FROM DEM_2spkr WHERE xmlID = %s AND divIndex = %s AND globalID = %s'
        cur.execute(sql, (xml_id, div_idx, g_id))
        parsed_str = cur.fetchone()[0]

        # get rules
        try:
            rules = subrules(parsed_str)
        except Exception as e:
            print('xmlID: {}, divIndex: {}, globalID: {}'.format(xml_id, div_idx, g_id))
            raise e

        # update subRuleC5 column
        if len(rules) > 0:
            rules_str = '~~~+~~~'.join(rules)
            sql = 'UPDATE DEM_2spkr SET subRulesC5 = %s WHERE xmlID = %s AND divIndex = %s AND globalID = %s'
            cur.execute(sql, (rules_str, xml_id, div_idx, g_id))

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


# write rules to db
def write_rules2db():
    # db conn
    conn = db_conn('bnc')
    cur = conn.cursor()
        
    # create DEM_2spker_ruleFreq table
    sql = 'CREATE TABLE DEM_2spkr_ruleFreq (ruleID INT, ruleStr LONGTEXT, count INT, PRIMARY KEY(ruleID))'
    cur.execute(sql)
    
    # load the pickled obj
    dic = pickle.load(open('subrules.txt', 'rb'))

    # write the key, val pairs to db
    rule_id = 1
    for key, val in dic.items():
        sql = 'INSERT INTO DEM_2spkr_ruleFreq VALUES(%s, %s, %s)' 
        cur.execute(sql, (rule_id, key, val))
        rule_id += 1
    # commit
    conn.commit()


# convert rule strings to rule IDs
def rulestr2id():
    # db conn
    conn = db_conn('bnc')
    cur = conn.cursor()

    # select rule IDs
    sql = 'SELECT ruleID, ruleStr FROM DEM_2spkr_ruleFreq'
    cur.execute(sql)
    data1 = cur.fetchall()
    # construct a dict
    dic = {}
    for rule_id, rule_str in data1:
        dic[rule_str] = rule_id

    # select data2
    sql = 'SELECT xmlID, divIndex, globalID, subRulesC5 FROM DEM_2spkr WHERE subRulesC5 IS NOT NULL'
    cur.execute(sql)
    data2 = cur.fetchall()

    # convert each sentence
    for i, d in enumerate(data2):
        xml_id, div_idx, g_id, rules_str = d
        rules = rules_str.split('~~~+~~~')
        id_str = ','.join(map(str, [dic[r] for r in rules]))
        # update
        sql = 'UPDATE DEM_2spkr SET subRulesID = %s WHERE xmlID = %s AND divIndex = %s AND globalID = %s'
        try:
            cur.execute(sql, (id_str, xml_id, div_idx, g_id))
        except Exception as e:
            print('xmlID: {}, divIndex: {}, globalID: {}'.format(xml_id, div_idx, g_id))
            print('id_str: {}'.format(id_str))
            raise e
        # print
        if (i % 99 == 0 and i > 0) or i == len(data2)-1:
            sys.stdout.write('\r{}/{} updated'.format(i+1, len(data2)))
            sys.stdout.flush()

    # commit
    conn.commit()
    



# main
if __name__ == '__main__':
    # extract rules
    # extract_rules()

    # write the dict of rules to db
    # write_rules2db()

    # rule strings to IDs
    rulestr2id()