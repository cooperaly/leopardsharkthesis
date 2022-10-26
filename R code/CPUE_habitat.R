#### BULLPOINT DATA BY DAY ####

# SUMMARY #
# All Sharks  NS
# Adults      *
# Juveniles   NS
# Females     NS
# Males       *
# AF          NS
# AM          *
# JF          NS
# JM          NS

#load packages
install.packages(c("tweedie", "statmod", "tidyr", "car"))
library(ggplot2)
library(MASS)
library(magrittr)
library(tidyverse)
library(tidyr)
library(qcc)
library(tweedie)
library(statmod)
library(glmmTMB)
library("car")


#load data
bp_day <- read.table(file="day_sum_BP.csv", header=TRUE, sep=",")
head(bp_day)
str(bp_day)
summary(bp_day)


#### ALL SHARKS #####################################################################################

lm_habitat_all <- lm((log(cpue_day+1))~habitat, data=bp_day)
summary(lm_habitat_all)
plot(lm_habitat_all)
# normal, equal

#run full model glmm with date-string as random
fit_habitat_all <- glmmTMB((log(cpue_day+1)) ~habitat + (1|date_string), data=bp_day,family = gaussian(), verbose = FALSE)
summary(fit_habitat_all) #p = 0.134

#boxplot
min_y<-min(0)
max_y<-max(5)
all_boxplot<-ggplot(data=bp_day, aes(x=habitat, y=cpue_day, fill=habitat)) +
  geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize = .7, fill="black") +
  theme_bw() +
  scale_y_continuous(limits=c(min_y,max_y)) +
  xlab("Habitat") + ylab("CPUE (sharks/hr)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  annotate("text",x=0.55, y=4.9, label= "NS", size=9)+
  theme_bw(base_size = 40, base_family = "")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  ggtitle('All Sharks')+theme(plot.title=element_text(hjust=0))+
  theme(axis.text.x=element_text())+
  scale_x_discrete(limit = c("mud", "eel"), labels = c("Mudflat","Eelgrass"))
all_boxplot

#### ADULTS #####################################################################################################################

lm_habitat_adult<-lm((log(cpue_adults+1))~habitat, data=bp_day)
summary(lm_habitat_adult)
plot(lm_habitat_adult)
#normal, equal

#run full model glmm with date-string as random
fit_habitat_adult <- glmmTMB((log(cpue_adults+1)) ~habitat + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_habitat_adult) #p=0.036*


#boxplot
min_y<-min(0)
max_y<-max(5)
adults_boxplot<-ggplot(data=bp_day, aes(x=habitat, y=cpue_adults, fill=habitat)) +
  geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize = .7, fill="black") +
  theme_bw() +
  scale_y_continuous(limits=c(min_y,max_y)) + 
  xlab("Habitat") + ylab("CPUE (sharks/hr)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  annotate("text",x=0.55, y=4.9, label= "*", size=15)+ 
  theme_bw(base_size = 40, base_family = "")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  ggtitle('Adult Sharks')+theme(plot.title=element_text(hjust=0))+
  theme(axis.text.x=element_text())+
  scale_x_discrete(limit = c("mud", "eel"), labels = c("Mudflat","Eelgrass"))
adults_boxplot



#### JUVENILES ######################################################################################################################

lm_habitat_juv<-lm((log(cpue_juv+1))~habitat, data=bp_day)
summary(lm_habitat_juv)
plot(lm_habitat_juv)
# normal, equal

fit_habitat_juv <- glmmTMB((log(cpue_juv+1)) ~habitat + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_habitat_juv) #p = 0.408

#boxplot
min_y<-min(0)
max_y<-max(5)
juv_boxplot<-ggplot(data=bp_day, aes(x=habitat, y=cpue_juv, fill=habitat)) +
  geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize = .7, fill="black") +
  theme_bw() +
  scale_y_continuous(limits=c(min_y,max_y)) +
  xlab("Habitat") + ylab("CPUE (sharks/hr)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  annotate("text",x=0.55, y=4.9, label= "NS", size=9)+
  theme_bw(base_size = 40, base_family = "")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  ggtitle('Juvenile Sharks')+theme(plot.title=element_text(hjust=0))+
  theme(axis.text.x=element_text())+
  scale_x_discrete(limit = c("mud", "eel"), labels = c("Mudflat","Eelgrass"))
juv_boxplot

#### FEMALES #########################################################################################################################

lm_habitat_F<-lm((log(cpue_F+1))~habitat, data=bp_day)
summary(lm_habitat_F)
plot(lm_habitat_F)
# normal, equal

fit_habitat_F <- glmmTMB((log(cpue_F+1)) ~habitat + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_habitat_F) #p = 0.403

#boxplot
min_y<-min(0)
max_y<-max(5)
F_boxplot<-ggplot(data=bp_day, aes(x=habitat, y=cpue_F, fill=habitat)) +
  geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize = .7, fill="black") +
  theme_bw() +
  scale_y_continuous(limits=c(min_y,max_y)) + 
  xlab("Habitat") + ylab("CPUE (sharks/hr)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  annotate("text",x=0.55, y=4.9, label= "NS", size=9)+ 
  theme_bw(base_size = 40, base_family = "")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  ggtitle('Female Sharks')+theme(plot.title=element_text(hjust=0))+
  theme(axis.text.x=element_text())+
  scale_x_discrete(limit = c("mud", "eel"), labels = c("Mudflat","Eelgrass"))
F_boxplot

#### MALES #########################################################################################################################

lm_habitat_M<-lm((log(cpue_M+1))~habitat, data=bp_day)
summary(lm_habitat_M)
plot(lm_habitat_M)
# not normal, equal 

fit_habitat_M <- glmmTMB((log(cpue_M+1)) ~habitat + (1|date_string), data=bp_day, dispformula = ~ habitat, family = tweedie(),verbose = TRUE)
summary(fit_habitat_M)
#Variance collapses --> use M_habitat_glm

M_habitat_glm <- glm(bp_day$cpue_M ~ bp_day$habitat, data=bp_day,family=tweedie(var.power=2,link.power=0))
summary(M_habitat_glm) #p = 0.018*

#boxplot
min_y<-min(0)
max_y<-max(5)
M_boxplot<-ggplot(data=bp_day, aes(x=habitat, y=cpue_M, fill=habitat)) + 
  geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize = .7, fill="black") +
  theme_bw() +
  scale_y_continuous(limits=c(min_y,max_y)) + 
  xlab("Habitat") + ylab("CPUE (sharks/hr)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  annotate("text",x=0.55, y=4.9, label= "*", size=15)+
  theme_bw(base_size = 40, base_family = "")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  ggtitle('Male Sharks')+theme(plot.title=element_text(hjust=0))+
  theme(axis.text.x=element_text())+
  scale_x_discrete(limit = c("mud", "eel"), labels = c("Mudflat","Eelgrass"))
M_boxplot


#### ADULT FEMALES ##########################################################################################################

lm_habitat_AF<-lm((log(cpue_AF+1))~habitat, data=bp_day)
summary(lm_habitat_AF)
plot(lm_habitat_AF)
#not normal, equal

fit_habitat_AF <- glmmTMB((log(cpue_AF+1)) ~habitat + (1|date_string), data=bp_day, dispformula = ~ habitat, family = tweedie(),verbose = TRUE)
summary(fit_habitat_AF)
#Variance collapses -> use AF_habitat_glm

AF_habitat_glm <- glm(bp_day$cpue_AF ~ bp_day$habitat, data=bp_day,family=tweedie(var.power=2,link.power=0))
summary(AF_habitat_glm)

min_y<-min(0)
max_y<-max(5)
AF_boxplot<-ggplot(data=bp_day, aes(x=habitat, y=cpue_AF, fill=habitat)) +
  geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize = .7, fill="black") +
  theme_bw() +
  scale_y_continuous(limits=c(min_y,max_y)) +
  xlab("Habitat") + ylab("CPUE (sharks/hr)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  annotate("text",x=0.55, y=4.9, label= "NS", size=9)+
  theme_bw(base_size = 40, base_family = "")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  ggtitle('Adult Female Sharks')+theme(plot.title=element_text(hjust=0))+
  theme(axis.text.x=element_text())+
  scale_x_discrete(limit = c("mud", "eel"), labels = c("Mudflat","Eelgrass"))
AF_boxplot

#### ADULT MALES #################################################################################################################

lm_habitat_AM<-lm((log(cpue_AM+1))~habitat, data=bp_day)
summary(lm_habitat_AM)
plot(lm_habitat_AM)
# not normal, not equal

fit_habitat_AM <- glmmTMB((log(cpue_AM+1)) ~habitat + (1|date_string), data=bp_day, dispformula = ~ habitat, family = tweedie(),verbose = TRUE)
summary(fit_habitat_AM)
#Variance collapses -> use AM_habitat_glm

AM_glm <- glm(bp_day$cpue_AM ~ bp_day$habitat, data=bp_day,family=tweedie(var.power=2, link.power=0))
summary(AM_glm) #p = 0.012*

min_y<-min(0)
max_y<-max(5)
AM_boxplot<-ggplot(data=bp_day, aes(x=habitat, y=cpue_AM, fill=habitat)) +
  geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize = .7, fill="black") +
  theme_bw() +
  scale_y_continuous(limits=c(min_y,max_y)) +
  xlab("Habitat") + ylab("CPUE (sharks/hr)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  annotate("text",x=0.55, y=4.9, label= "*", size=15)+ 
  theme_bw(base_size = 40, base_family = "")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  ggtitle('Adult Male Sharks')+theme(plot.title=element_text(hjust=0))+
  theme(axis.text.x=element_text())+
  scale_x_discrete(limit = c("mud", "eel"), labels = c("Mudflat","Eelgrass"))
AM_boxplot

#### JUVENILE FEMALES ###################################################################################################################

lm_eel_mud_JF<-lm((log(cpue_JF+1))~habitat, data=bp_day)
summary(lm_eel_mud_JF)
plot(lm_eel_mud_JF)
# normal, equal

fit_eel_mud_JF <- glmmTMB((log(cpue_JF+1)) ~habitat + (1|date_string), data=bp_day,  family = gaussian(),verbose = TRUE)
summary(fit_eel_mud_JF) # p=0.75

#boxplot
min_y<-min(0)
max_y<-max(5)
JF_boxplot<-ggplot(data=bp_day, aes(x=habitat, y=cpue_JF, fill=habitat)) +
  geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize = .7, fill="black") +
  theme_bw() +
  scale_y_continuous(limits=c(min_y,max_y)) + 
  xlab("Habitat") + ylab("CPUE (sharks/hr)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  annotate("text",x=0.55, y=4.9, label= "NS", size=9)+ 
  theme_bw(base_size = 40, base_family = "")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  ggtitle('Juvenile Female Sharks')+theme(plot.title=element_text(hjust=0))+
  theme(axis.text.x=element_text())+
  scale_x_discrete(limit = c("mud", "eel"), labels = c("Mudflat","Eelgrass"))
JF_boxplot

#### JUVENILE MALES ################################################################################################################

JM_glm <- glm(bp_day_data$cpue_JM ~ bp_day_data$habitat, family = tweedie(link.power=0, var.power=2))
summary(JM_glm) #NS p = 0.242

#run a linear model first
lm_habitat_JM<-lm((log(cpue_JM+1))~habitat, data=bp_day)
summary(lm_habitat_JM)
plot(lm_habitat_JM)
# not normal, equal

fit_habitat_JM <- glmmTMB((log(cpue_JM+1)) ~habitat + (1|date_string), data=bp_day, dispformula = ~ habitat, family = tweedie(),verbose = TRUE)
summary(fit_habitat_JM) #p=0.331

#boxplot
min_y<-min(0)
max_y<-max(5)
JM_boxplot<-ggplot(data=bp_day, aes(x=habitat, y=cpue_JM, fill=habitat)) +
  geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize = .7, fill="black") +
  theme_bw() +
  scale_y_continuous(limits=c(min_y,max_y)) + 
  xlab("Habitat") + ylab("CPUE (sharks/hr)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  annotate("text",x=0.55, y=4.9, label= "NS", size=9)+
  theme_bw(base_size = 40, base_family = "")+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="none") +
  ggtitle('Juvenile Male Sharks')+theme(plot.title=element_text(hjust=0))+
  theme(axis.text.x=element_text())+
  scale_x_discrete(limit = c("mud", "eel"), labels = c("Mudflat","Eelgrass"))
JM_boxplot
