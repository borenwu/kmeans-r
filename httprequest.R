library(httr)
library(jsonlite)
library(data.table)

url <- "http://192.168.0.106:1337/data/kmeans"
body <- list(company_id = '5abb5e65ceac5e882e3fb076')

r <- POST(url, body = body)
content <- content(r, as="text")
new <- fromJSON(content)

result <- new$result
clients <- new$clients

dtresult = data.table(result)
dtclients = data.table(clients)

setkey(dtresult,"_id")
setkey(dtclients,"id")
dt <- dtresult[dtclients,nomatch=NA,mult="all"]
delete_na <- dt[-which(is.na(dt$total) | is.na(dt$taskCount))]
data <- subset(delete_na, select=-c(`_id`,desc,updatedAt,company))
data <- data[,c('client_name','taskCount','createdAt','taskDate','total','receivable')]

data$today <- Sys.Date()
data$L <- data$today - as.Date(data$createdAt)
data$R <- data$today - as.Date(data$taskDate)
data <- subset(data, select=-c(today,createdAt,taskDate))
colnames(data) <- c('客户名','交易频数','交易总额','应收款','客户时长','交易间隔')

datasource <- data[,2:6]
datasource$'客户时长' <- as.numeric(datasource$'客户时长')
datasource$'交易间隔' <- as.numeric(datasource$'交易间隔')
zscoredfile <- scale(datasource)

k = kmeans(zscoredfile,3)

cluster <- k$cluster
table(cluster)
k$center

data$'类别' <- cluster

library(fmsb)
max <- apply(k$centers, 2, max)
min <- apply(k$centers, 2, min)
data.radar <- data.frame(rbind(max, min, k$centers))
radarchart(data.radar, pty = 32, plty = 1, plwd = 2, vlcex = 0.7)
# 给雷达图加图例
L <- 1.2
for(i in 1:3){
  text(1.8, L, labels = paste("--客户群", i), col = i)
  L <- L - 0.2
}


