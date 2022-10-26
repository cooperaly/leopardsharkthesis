# TIME OF CAPTURE vs CATCH #

install.packages("lubridate")
install.packages("ggpmisc")

library(tidyr)
library(ggplot2)

#### CPUE x TIME ####

#load data
cpue_timecap <- read.table(file = "time_of_capture.csv",  header = TRUE, na.strings = "na", sep=",")
head(cpue_timecap)
str(cpue_timecap)

cpue_timeplot <- ggplot(data=cpue_timecap, aes(x=hour, y=cpue)) + geom_point(size=4) +
  scale_x_continuous(labels=as.character(cpue_timecap$hour), breaks=(cpue_timecap$hour))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme_bw(base_size = 19, base_family = "")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point()+
  xlab("Time of Capture") + ylab("CPUE (# sharks/hr)") +
  theme(axis.text=element_text(size=16))+
  theme(axis.title=element_text(size=20))
cpue_timeplot
