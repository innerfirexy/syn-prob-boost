#!/usr/local/bin/python3
# Extract syntactic rules from the entropy table in swbd db
# Yang Xu
# 2/2/2016

import MySQLdb
import sys
import pickle
import re
import time

from nltk.tree import *
from nltk.probability import FreqDist

import multiprocessing
from multiprocessing import Pool, Manager


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
        # replace all ',' and '.' with blank character
        rules = [re.sub(r'[,|\.]\s*', '', r).strip() for r in rules]
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
    sql = 'SELECT convID, globalID, parsed FROM entropy'
    cur.execute(sql)
    data = cur.fetchall()

    # initiate dict object to store sub-rules and their frequency
    # rules_dic = FreqDist()

    # initiate pool and manager
    pool = Pool(multiprocessing.cpu_count())
    manager = Manager()
    queue = manager.Queue()
    rules_dic = manager.dict() # to store sub-rules and their frequency count

    # multiprocessing
    args = [(datum, rules_dic, queue) for datum in data]
    result = pool.map_async(extract_rules_worker, args, chunksize = 5000)

    # manager loop
    while True:
        if result.ready():
            print('\nall rows extracted')
            break
        else:
            sys.stdout.write('\r{}/{} extracted'.format(queue.qsize(), len(args)))
            sys.stdout.flush()
            time.sleep(1)

    # update result to subRules column
    real_results = result.get()
    for i, res in enumerate(real_results):
        conv_id, g_id, rules_str = res
        sql = 'UPDATE entropy SET subRules = %s WHERE convID = %s AND globalID = %s'
        cur.execute(sql, (rules_str, conv_id, g_id))
        if (i % 99 == 0 and i > 0) or i == len(real_results):
            sys.stdout.write('\r{}/{} updated'.format(i+1, len(real_results)))
            sys.stdout.flush()
            conn.commit()
    # dump rules_dic
    pickle.dump(rules_dic, open('subrules.txt', 'wb'))

# worker func called in extract_rules
def extract_rules_worker(args):
    (conv_id, g_id, parsed_str), rules_dic, queue = args
    # extract rules
    try:
        rules = subrules(parsed_str)
    except Exception as e:
        print('convID: {}, globalID: {}'.format(conv_id, g_id))
        raise e
    # update rules_dic
    for r in rules:
        if r in rules_dic:
            rules_dic[r] += 1
        else:
            rules_dic[r] = 0
    # update queue
    queue.put(1)
    # return joined rules_str
    return (conv_id, g_id, '~~~+~~~'.join(rules))

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
