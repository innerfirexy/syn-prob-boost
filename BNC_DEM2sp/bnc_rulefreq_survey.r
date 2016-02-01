# Suvey the frequency of rule strings stored in DEM_2spkr_ruleFreq table
# Yang Xu
# 1/18/2016

library(RMySQL)
library(ggplot2)

# ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
conn = dbConnect(MySQL(), host = '127.0.0.1', user = 'yang', port = 1234, password = "05012014", dbname = 'bnc')
sql = 'SELECT * FROM DEM_2spkr_ruleFreq'
df = dbGetQuery(conn, sql)


summary(df$count) # 1st, 3rd qu == 1
nrow(df) # 44296
nrow(subset(df, count == 1)) # 33660

# total counts
sum(df$count) # 555044
sum(df[1:400,]$count)

# histograms
h1 = ggplot(df[1:200,], aes(x = ruleID, y = count)) + geom_bar(stat = 'identity')
plot(h1)


## split total count by 2 segments: high and low
sum(df$count)/2 # 277522
sum(df[1:70,]$count) # 270536

thres = df[70,]$count # 1106

# label the high and low freq rules
df$freq = 'low'
df[df$count >= thres,]$freq = 'high'

# save df to rds
saveRDS(df, 'df.rulefreq.rds')
