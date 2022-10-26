#### empa wq station 1 data ####

#load tidyverse
install.packages("tidyverse")
install.packages("tidyquant")

#load data
ctd <- read.table(file="CTD_Drakes1.csv", header=TRUE, sep=",")
head(ctd)
str(ctd)

#install libraries
library(lubridate)
library(ggplot2)
library(scales)
library(tidyverse)
library(tidyquant)

ctd$dt <- mdy_hm(ctd$dt)
ctd$dt


#graph all temp (10-day moving average)
ctd_temp_gg <- ggplot(data = ctd, aes(x=dt, y=temp))+ geom_line() +
  geom_ma(ma_fun = SMA, n=2920, color = "red", size = 1.5, na.rm=TRUE) +  theme_bw() +
  xlab("Month (2021)") + ylab("Temperature (°C)")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(size=20))+
  theme(axis.title = element_text(size=25))+
  ylim(9, 25)+
  scale_x_datetime(date_labels="%b", date_breaks = "1 month")

ctd_temp_gg


#graph snapshot
ctd_snap <- read.table(file="CTD_Drakes1_snap.csv", header=TRUE, sep=",")
ctd_snap$dt <- mdy_hm(ctd_snap$dt)

ctd_temp_snap <- ggplot(data = ctd_snap, aes(x=dt, y=temp))+ geom_point() +
  theme_bw() +
  geom_vline(xintercept=as.numeric(ctd_snap$dt[c(245, 495, 745, 980)]), linetype=5, size=1.3, colour = "red")+
  xlab("Time of Day (7/21/21 to 7/25/21)") + ylab("Temperature (°C)")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=5)) +
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=16))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ylim(14, 21)+
  scale_x_datetime(date_labels="%H:%M", date_breaks = "12 hours", expand=c(0,0))

ctd_temp_snap
