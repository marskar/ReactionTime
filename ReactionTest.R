## Download data file

## Read in data and convert xlsx to csv 
#install.packages("rio")
library(rio)
convert("rt_acc_averages.xlsx", "dat.csv")
df<-read.csv("dat.csv", header = FALSE, na.strings = c("",NA))
?convert
names(df) <- as.matrix(df[3, ])

## Fill in empty cells

#install.packages("tidyr")
library(tidyr)
df2<- fill(df, c(1,2))

df3 <- df2[-which(rowMeans(is.na(df2)) > 0.50), ]
df4 <- df3[, -which(colMeans(is.na(df3)) > 0.50)]

df5<-df4[!is.na(as.numeric(as.character(df4$diff))),]
write.csv(df5,"tidy.csv")
df<-read.csv("tidy.csv", header = TRUE)

dfRT <- df[which(df$Mean > 1000),]
names(dfRT)[names(dfRT)=="Mean"] <- "MeanReactionTime"
names(dfRT)[names(dfRT)=="Std..Error"] <- "ReactionTimeStandardError"
dfAcc <- df[which(df$Mean < 1),]
names(dfAcc)[names(dfAcc)=="Mean"] <- "MeanAccuracy"
names(dfAcc)[names(dfAcc)=="Std..Error"] <- "AccuracyStandardError"
dfAll<-cbind(dfRT,dfAcc[,5:6])
dfAll<- dfAll[,-1]
write.csv(dfAll,"merged.csv")


#install.packages("plotly")
library(plotly)

brt<-ggplot(dfAll, aes(x = group,y = MeanReactionTime)) + 
  geom_boxplot() + 
  geom_point(size = 2.5, aes(col=factor(reward), shape=factor(diff))) +
  ylab("Reaction Time") +
  theme(axis.line = element_line(colour = "black"), 
        text = element_text(size=20, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x=element_blank()
  )
brt

#install.packages("plotly")
#library(plotly)
brt<- ggplotly(brt)
brt

bacc<-ggplot(dfAll, aes(x = group,y = MeanAccuracy)) + 
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

