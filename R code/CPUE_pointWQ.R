#### BULLPOINT_DAY DATA ####

#load packages
install.packages(c("tweedie", "statmod", "tidyr", "ggplot2", "glmmTMB", "car"))
install.packages("glmmTMB")
library(glmmTMB)
library(tweedie)
library(statmod)
library("car")


#load data
bp_day <- read.table(file="day_sum_BP.csv", header=TRUE, sep=",")
head(bp_day)
str(bp_day)


#### AGE CLASS / SEX CPUE vs TEMPERATURE #### (NO SIG CORRELATIONS) #####################################################

#all sharks vs TEMP ######################################################################## ALL (TEMP)

lm_temp_all<-lm((log(cpue_day+1))~temp, data=bp_day)
summary(lm_temp_all)
plot(lm_temp_all)
#normal, equal

fit_temp_all <- glmmTMB((log(cpue_day+1)) ~temp + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_temp_all) #p = 0.197


#adults vs TEMP ############################################################################## ADULTS (TEMP)

lm_temp_adults<-lm((log(cpue_adults+1))~temp, data=bp_day)
summary(lm_temp_adults)
plot(lm_temp_adults)
#normal, unequal

fit_temp_adults <- glmmTMB((log(cpue_adults+1)) ~temp + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_temp_adults) #p = 0.086


#juveniles vs TEMP ########################################################################### JUVENILES (TEMP)

lm_temp_juv<-lm((log(cpue_juv+1))~temp, data=bp_day)
summary(lm_temp_juv)
plot(lm_temp_juv)
#normal, equal

fit_temp_juv <- glmmTMB((log(cpue_juv+1)) ~temp + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_temp_juv) #p = 0.441


#females vs TEMP ############################################################################# FEMALES (TEMP)

lm_temp_F<-lm((log(cpue_F+1))~temp, data=bp_day)
summary(lm_temp_F)
plot(lm_temp_F)
#normal, equal

fit_temp_F <- glmmTMB((log(cpue_F+1)) ~temp + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_temp_F) #p = 0.347

#males vs TEMP ############################################################################### MALES (TEMP)

lm_temp_M<-lm((log(cpue_M+1))~temp, data=bp_day)
summary(lm_temp_M)
plot(lm_temp_M)
#not normal, equal

fit_temp_M <- glmmTMB((log(cpue_M+1)) ~temp + (1|date_string), data=bp_day, dispformula = ~ temp, family=tweedie(), verbose=TRUE)
summary(fit_temp_M) #p = 0.207
#Variance collapses to 0 -> use M_temp_glm

M_temp_glm <- glm(bp_day$cpue_M ~ bp_day$temp, family=tweedie(var.power=2, link.power=0))
summary(M_temp_glm) #p=0.236


#adult females vs TEMP ##################################################################### ADULT FEMALES (TEMP)

lm_temp_AF<-lm((log(cpue_AF+1))~temp, data=bp_day)
summary(lm_temp_AF)
plot(lm_temp_AF)
#not normal, unequal

fit_temp_AF <- glmmTMB((log(cpue_AF+1)) ~temp + (1|date_string), data=bp_day, dispformula = ~ temp, family=tweedie(), verbose=TRUE)
summary(fit_temp_AF) #p = 0.207
#Variance collapses to 0 -> use AF_temp_glm

AF_temp_glm <- glm(bp_day$cpue_AF ~ bp_day$temp, family=tweedie(var.power=2, link.power=0))
summary(AF_temp_glm) #p=.392

#adult males vs TEMP ##################################################################### ADULT MALES (TEMP)

lm_temp_AM<-lm((log(cpue_AM+1))~temp, data=bp_day)
summary(lm_temp_AM)
plot(lm_temp_AM)
#not normal, unequal

fit_temp_AM <- glmmTMB((log(cpue_AM+1)) ~temp + (1|date_string), data=bp_day, dispformula = ~ temp, family=tweedie(), verbose=TRUE)
summary(fit_temp_AM)
#Variance collapses to 0 -> use AM_do_glm

AM_temp_glm <- glm(bp_day$cpue_AM ~ bp_day$temp, family=tweedie(var.power=2, link.power=0))
summary(AM_temp_glm) #0.082

#juvenile females vs TEMP ##################################################################### JUVENILE FEMALES (TEMP)

lm_temp_JF<-lm((log(cpue_JF+1))~temp, data=bp_day)
summary(lm_temp_JF)
plot(lm_temp_JF)
# normal, equal

fit_temp_JF <- glmmTMB((log(cpue_JF+1)) ~temp + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_temp_JF) #p = 0.436


#juvenile males vs TEMP ##################################################################### JUVENILE MALES (TEMP)

lm_temp_JM<-lm((log(cpue_JM+1))~temp, data=bp_day)
summary(lm_temp_JM)
plot(lm_temp_JM)
#not normal, equal

fit_temp_JM <- glmmTMB((log(cpue_JM+1)) ~temp + (1|date_string), data=bp_day, dispformula = ~ temp, family=tweedie(), verbose=TRUE)
summary(fit_temp_JM) #p = 0.707


#### AGE CLASS / SEX CPUE vs DO ######################################################################################################

#all sharks vs DO ########################################################################## ALL (DO)

lm_do_all<-lm((log(cpue_day+1))~do_mgl, data=bp_day)
summary(lm_do_all)
plot(lm_do_all)
# normal, equal

fit_do_all <- glmmTMB((log(cpue_day+1)) ~do_mgl + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_do_all) #p = 0.012


#adults vs DO ############################################################################## ADULTS (DO)

lm_do_adults<-lm((log(cpue_adults+1))~do_mgl, data=bp_day)
summary(lm_do_adults)
plot(lm_do_adults)
# normal, unequal

fit_do_adults <- glmmTMB((log(cpue_adults+1)) ~do_mgl + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_do_adults) #p = 0.011


#juveniles vs DO ########################################################################### JUVENILES (DO)

lm_do_juv<-lm((log(cpue_juv+1))~do_mgl, data=bp_day)
summary(lm_do_juv)
plot(lm_do_juv)
# normal, equal

fit_do_juv <- glmmTMB((log(cpue_juv+1)) ~do_mgl + (1|date_string), data=bp_day,family = gaussian(), verbose = FALSE)
#remove interaction term


fit_do_juv <- glmmTMB((log(cpue_juv+1)) ~do_mgl, data=bp_day,family = gaussian(), verbose = FALSE)
summary(fit_do_juv) #p = 0.047


#females vs DO ############################################################################# FEMALES (DO)

lm_do_F<-lm((log(cpue_F+1))~do_mgl, data=bp_day)
summary(lm_do_F)
plot(lm_do_F)
# normal, equal

fit_do_F <- glmmTMB((log(cpue_F+1)) ~do_mgl + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_do_F) #p = 0.123


#males vs DO ############################################################################## MALES (DO)

lm_do_M<-lm((log(cpue_M+1))~do_mgl, data=bp_day)
summary(lm_do_M)
plot(lm_do_M)
# not normal, equal

fit_do_M <- glmmTMB((log(cpue_M+1)) ~do_mgl + (1|date_string), data=bp_day, dispformula = ~ do_mgl, family = tweedie(),verbose = TRUE)
summary(fit_do_M)
# variance collapses -> use M_do_glm

M_do_glm <- glm(bp_day$cpue_M ~ bp_day$do_mgl, data=bp_day, family = tweedie(var.power=2, link.power=0))
summary(M_do_glm) #p=7.07e-5***


#adult females vs DO ############################################################################## ADULT FEMALES (DO)

lm_do_AF<-lm((log(cpue_AF+1))~do_mgl, data=bp_day)
summary(lm_do_AF)
plot(lm_do_AF)
# not normal, unequal

fit_do_AF <- glmmTMB((log(cpue_AF+1)) ~do_mgl + (1|date_string), data=bp_day, dispformula = ~ do_mgl, family = tweedie(),verbose = TRUE)
summary(fit_do_AF)
# variance collapses; use AF_do_glm

AF_do_glm <- glm(bp_day$cpue_AF ~ bp_day$do_mgl, data=bp_day, family = tweedie(var.power=2, link.power=0))
summary(AF_do_glm) #p=0.099


#adult males vs DO ############################################################################## ADULT MALES (DO)

lm_do_AM<-lm((log(cpue_AM+1))~do_mgl, data=bp_day)
summary(lm_do_AM)
plot(lm_do_AM)
# normal, unequal

fit_do_AM <- glmmTMB((log(cpue_AM+1)) ~do_mgl + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_do_AM) #p=0.0001***

#juvenile females vs DO ############################################################################## JUVENILE FEMALES (DO)

lm_do_JF<-lm((log(cpue_JF+1))~do_mgl, data=bp_day)
summary(lm_do_JF)
plot(lm_do_JF)
# not normal, unequal

fit_do_JF <- glmmTMB((log(cpue_JF+1)) ~do_mgl + (1|date_string), data=bp_day, dispformula = ~ do_mgl, family = tweedie(),verbose = TRUE)
summary(fit_do_JF) #p=0.253

#juvenile males vs DO ############################################################################## JUVENILE MALES (DO)

lm_do_JM<-lm((log(cpue_JM+1))~do_mgl, data=bp_day)
summary(lm_do_JM)
plot(lm_do_JM)
# not normal, unequal

fit_do_JM <- glmmTMB((log(cpue_JM+1)) ~do_mgl + (1|date_string), data=bp_day, dispformula = ~ do_mgl, family = tweedie(),verbose = TRUE)
summary(fit_do_JM) #p=0.485


#### AGE CLASS / SEX vs SALINITY ####################################################################################################

#all sharks vs SAL ########################################################################## ALL (SAL)

lm_sal_all<-lm((log(cpue_day+1))~sal, data=bp_day)
summary(lm_sal_all)
plot(lm_sal_all)
# normal, unequal

fit_sal_all <- glmmTMB((log(cpue_day+1)) ~sal + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_sal_all) #p=0.485


#adults vs SAL ############################################################################## ADULTS (SAL)

lm_sal_adults<-lm((log(cpue_adults+1))~sal, data=bp_day)
summary(lm_sal_adults)
plot(lm_sal_adults)
# normal, equal

fit_sal_adults <- glmmTMB((log(cpue_adults+1)) ~sal + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_sal_adults) #p=0.863


#juveniles vs SAL ########################################################################### JUVENILES (SAL)

lm_sal_juv<-lm((log(cpue_juv+1))~sal, data=bp_day)
summary(lm_sal_juv)
plot(lm_sal_juv)
# normal, unequal

fit_sal_juv <- glmmTMB((log(cpue_juv+1)) ~sal + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_sal_juv) #p=0.347


#females vs SAL ############################################################################# FEMALES (SAL)

lm_sal_F<-lm((log(cpue_F+1))~sal, data=bp_day)
summary(lm_sal_F)
plot(lm_sal_F)
# normal, unequal

fit_sal_F <- glmmTMB((log(cpue_F+1)) ~sal + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_sal_F) #p=0.514


#males vs SAL ############################################################################## MALES (SAL)

lm_sal_M<-lm((log(cpue_M+1))~sal, data=bp_day)
summary(lm_sal_M)
plot(lm_sal_M)
# not normal, unequal

fit_sal_M <- glmmTMB((log(cpue_M+1)) ~sal + (1|date_string), data=bp_day, dispformula = ~ do_mgl, family = tweedie(),verbose = TRUE)
summary(fit_sal_M) 
#variance collapses -> use M_sal_glm

M_sal_glm <- glm(bp_day$cpue_M ~ bp_day$sal, family=tweedie(var.power=2, link.power=0))
summary(M_sal_glm) #p=0.013*


#adult females vs SAL ########################################################################### ADULT FEMALES (SAL)

lm_sal_AF<-lm((log(cpue_AF+1))~sal, data=bp_day)
summary(lm_sal_AF)
plot(lm_sal_AF)
# not normal, equal

fit_sal_AF <- glmmTMB((log(cpue_AF+1)) ~sal + (1|date_string), data=bp_day, dispformula = ~ do_mgl, family = tweedie(),verbose = TRUE)
summary(fit_sal_AF)
#variance collapses -> use AF_sal_glm

AF_sal_glm <- glm(bp_day$cpue_AF ~ bp_day$sal, family=tweedie(var.power=2, link.power=0))
summary(AF_sal_glm) #p=0.266


#adult males vs SAL ########################################################################### ADULT MALES (SAL)

lm_sal_AM<-lm((log(cpue_AM+1))~sal, data=bp_day)
summary(lm_sal_AM)
plot(lm_sal_AM)
# not normal, unequal

fit_sal_AM <- glmmTMB((log(cpue_AM+1)) ~sal + (1|date_string), data=bp_day, dispformula = ~ do_mgl, family = tweedie(),verbose = TRUE)
summary(fit_sal_AM) #p=0.304

#juvenile females vs SAL ########################################################################### JUVENILE FEMALES (SAL)

lm_sal_JF<-lm((log(cpue_JF+1))~sal, data=bp_day)
summary(lm_sal_JF)
plot(lm_sal_JF)
# normal, unequal

fit_sal_JF <- glmmTMB((log(cpue_JF+1)) ~sal + (1|date_string), data=bp_day,family = gaussian(), verbose = TRUE)
summary(fit_sal_JF) #p=0.576

#juvenile males vs SAL ########################################################################### JUVENILE MALES (SAL)

lm_sal_JM<-lm((log(cpue_JM+1))~sal, data=bp_day)
summary(lm_sal_JM)
plot(lm_sal_JM)
# not normal, equal

fit_sal_JM <- glmmTMB((log(cpue_JM+1)) ~sal + (1|date_string), data=bp_day, dispformula = ~ do_mgl, family = tweedie(),verbose = TRUE)
summary(fit_sal_JM)
#variance collapses -> use AF_sal_glm

JM_sal_glm <- glm(bp_day$cpue_JM ~ bp_day$sal, family=tweedie(var.power=2, link.power=0))
summary(JM_sal_glm) #p=0.012*











#### POINT WQ ALL SITES ####

library(glmmTMB)

#load data
all_day <- read.table(file="day_sum_all.csv", header=TRUE, sep=",")
head(all_day)
str(all_day)

# all vs. temp
lm_day_temp_all<-lm((log(cpue_day+1))~temp, data=all_day)
summary(lm_day_temp_all)
plot(lm_day_temp_all)
#not normal, not equal

fit_temp_day_all<- glmmTMB((log(cpue_day+1)) ~temp + (1|site), data=all_day, dispformula = ~ temp, family = tweedie(),verbose = FALSE)
summary(fit_temp_day_all) #p = 0.057


# all vs. sal
lm_day_sal_all<-lm((log(cpue_day+1))~sal, data=all_day)
summary(lm_day_sal_all)
plot(lm_day_sal_all)
#not normal, not equal

fit_sal_day_all<- glmmTMB((log(cpue_day+1)) ~sal + (1|site), data=all_day, dispformula = ~ sal, family = tweedie(),verbose = FALSE)
summary(fit_sal_day_all) #p = 0.344


# all vs. do
lm_day_do_mgl_all<-lm((log(cpue_day+1))~do_mgl, data=all_day)
summary(lm_day_do_mgl_all)
plot(lm_day_do_mgl_all)
#not normal, not equal

fit_do_mgl_day_all<- glmmTMB((log(cpue_day+1)) ~do_mgl + (1|site), data=all_day, dispformula = ~ do_mgl, family = tweedie(),verbose = FALSE)
summary(fit_do_mgl_day_all) #p = 0.036
