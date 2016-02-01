# Quickly check how LLA (logSILLA) changes with distance between prime and target
# Yang Xu
# 1/14/2016

library(RMySQL)

# Run "ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306"
conn <- dbConnect(MySQL(), host = '127.0.0.1', user = 'yang', port = 1234, password = "05012014", dbname = 'bnc')
df <- dbGetQuery(conn, 'SELECT XMLID, Distance, logLILLA, logSILLA FROM pairs_part')

# models
summary(lm(logLILLA ~ log(Distance), df)) # t = -15.32***
summary(lm(logSILLA ~ log(Distance), df)) # t = 1.097