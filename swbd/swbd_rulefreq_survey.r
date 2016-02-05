# Read data from entropy_ruleFreq and save to rds
# Yang Xu
# 2/4/2016

library(RMySQL)
library(ggplot2)

# ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
conn = dbConnect(MySQL(), host = '127.0.0.1', user = 'yang', port = 1234, password = "05012014", dbname = 'swbd')
sql = 'SELECT * FROM entropy_ruleFreq'
df = dbGetQuery(conn, sql)

# save to rds
saveRDS(df, 'df.rulefreq.rds')
