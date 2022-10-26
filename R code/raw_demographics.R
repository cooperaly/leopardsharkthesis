#### raw demographics ####

bp_log <- read.table(file="bullpoint_log.csv", header=TRUE, sep=",")
head(bp_log)
str(bp_log)

raw <- as.data.frame(table(bp_log$age_bin), na.action=na.omit)
na.omit(raw)
summary(raw)

library(ggplot2)

#barplot
raw_barplot <- ggplot(raw, aes(x=Var1, y=Freq))+
  geom_bar(position=position_dodge(), stat="identity", colour='black', fill="blue")+
  theme_bw() +
  theme(axis.text.x=element_text(angle=0,hjust=1))+
  xlab("Age Class by Bin") + ylab("Raw Count")+
  scale_fill_manual("legend", value = c("40" = "black", "50" = "grey", "60" = "grey", "70" = "grey", "80" = "grey", "90" = "grey", "100" = "lightblue", "110" = "lightblue", "120" = "lightblue", "130" = "lightblue"))+
  theme(axis.text=element_text(size=20))+
  theme(axis.title=element_text(size=24))+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  ggtitle('Raw Demographics')+
  theme(plot.title=element_text(hjust=0))+
  theme(plot.title=element_text(size=30))
raw_barplot