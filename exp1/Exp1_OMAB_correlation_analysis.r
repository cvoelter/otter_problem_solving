library(gtools)
library(exactRankTests)

source("functions/corr_exact.r")
all.data <- read.table(file = "exp1/data/160628 Otter multiaccess box_data 2016attempts.txt", header = T, sep = "\t")

xdata=subset(all.data, Subject=="Rudi")
xdata=subset(all.data, Subject=="Blacky")
xdata=subset(all.data, Subject=="Twoface")
xdata=subset(all.data, Subject=="James")

corr.exact(xdata$Latency , xdata$transitions)




