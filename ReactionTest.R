## Download data file

install.packages("compare")
library(compare)
## Read in data and convert xlsx to csv 
install.packages("rio")
install.packages("xlsx")
library(xlsx)
library(rio)
getwd()
setwd("/Users/skarzynskimw/Documents/")
convert("rt_acc_averages.xlsx", "dat.csv")
dat<-read.csv("dat.csv", header = FALSE, na.strings = c("",NA))

names(dat) <- as.matrix(dat[3, ])



install.packages("tidyr")
library(tidyr)
dat2<- fill(dat, c(1,2))

dat3 <- dat2[-which(rowMeans(is.na(dat2)) > 0.50), ]
dat4 <- dat3[, -which(colMeans(is.na(dat3)) > 0.50)]

dat5<-dat4[!is.na(as.numeric(as.character(dat4$diff))),]
write.csv(dat5,"tidy.csv")
dat<-read.csv("tidy.csv", header = TRUE)

dfRT <- dat[which(dat$Mean > 1000),]
names(dfRT)[names(dfRT)=="Mean"] <- "MeanReactionTime"
names(dfRT)[names(dfRT)=="Std..Error"] <- "ReactionTimeStandardError"
dfAcc <- dat[which(dat$Mean < 1),]
names(dfAcc)[names(dfAcc)=="Mean"] <- "MeanAccuracy"
names(dfAcc)[names(dfAcc)=="Std..Error"] <- "AccuracyStandardError"
dfAll<-cbind(dfRT,dfAcc[,5:6])
dfAll<- dfAll[,-1]


#install.packages("plotly")
#library(plotly)

brt<-ggplot(data = dfAll, aes(x = dfAll$group,y = dfAll$MeanReactionTime)) + 
  geom_boxplot() + 
  geom_point(size = 2.5, aes(col=factor(reward), shape=factor(diff))) +
  ylab("Reaction Time") +
  theme(axis.line = element_line(colour = "black"), 
        text = element_text(size=20, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x=element_blank(),
  )
brt

#install.packages("plotly")
#library(plotly)
brt<- ggplotly(brt)
brt

bacc<-ggplot(data = dfAll, aes(x = group,y = MeanAccuracy)) + 
  geom_boxplot() + 
  geom_point(size = 2.5, aes(col=factor(reward), shape=factor(diff))) +
  ylab("Reaction Time") +
  theme(axis.line = element_line(colour = "black"), 
        text = element_text(size=20, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x=element_blank(),
        legend.position="none"
  )
bacc

#install.packages("plotly")
#library(plotly)
bacc<- ggplotly(bacc)
bacc

