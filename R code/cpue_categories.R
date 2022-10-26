#### CPUE Categories ####
#overall and by month

library(ggplot2)
library(tidyr)
library(dplyr)

#load data
cpue_barplot <- read.table(file="cpue_barplot.csv", header=TRUE, sep=",")
head(cpue_barplot)

#Force order of x axis as displayed on .csv
cpue_barplot$cat <- factor(cpue_barplot$cat, levels=unique(cpue_barplot$cat))

## total cpue

#barplot
gg_cpue_barplot <- ggplot(cpue_barplot, aes(x=cat, y=cpue))+
  geom_bar(position=position_dodge(), stat="identity", colour='black', fill="lightblue")+
  geom_errorbar(aes(ymin=cpue, ymax=cpue+sd), width=.2)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  xlab("Category") + ylab("Average CPUE")+
  theme(axis.text=element_text(size=20))+
  theme(axis.title=element_text(size=24))+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  ggtitle('CPUE Summary')+
  theme(plot.title=element_text(hjust=0))+
  theme(plot.title=element_text(size=30))
gg_cpue_barplot
