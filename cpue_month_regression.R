#### cpue month regression ####

cpue_month <- read.table(file="cpue_month.csv", header=TRUE, sep=",")
str(cpue_month)

#all
cpue_month_all <- lm(cpue_month$all ~ cpue_month$date_string, data=cpue_month)
plot(cpue_month_all)

cpue_month_all_glm <- glm(cpue_month$all ~ cpue_month$date_string, data=cpue_month, family=tweedie(var.power=2, link.power=0))
summary(cpue_month_all_glm) #p=0.0398


#adults
cpue_month_adults <- lm(cpue_month$A ~ cpue_month$date_string, data=cpue_month)
plot(cpue_month_adults)

cpue_month_adults_glm <- glm(cpue_month$A ~ cpue_month$date_string, data=cpue_month, family=tweedie(var.power=2, link.power=0))
summary(cpue_month_adults_glm) #p=0.0949


#juveniles
cpue_month_juv <- lm(cpue_month$J ~ cpue_month$date_string, data=cpue_month)
plot(cpue_month_juv)

cpue_month_juv_glm <- glm(cpue_month$J ~ cpue_month$date_string, data=cpue_month, family=tweedie(var.power=2, link.power=0))
summary(cpue_month_juv_glm) #p=0.0202

#females
cpue_month_F <- lm(cpue_month$F ~ cpue_month$date_string, data=cpue_month)
plot(cpue_month_F)

cpue_month_F_glm <- glm(cpue_month$F ~ cpue_month$date_string, data=cpue_month, family=tweedie(var.power=2, link.power=0))
summary(cpue_month_F_glm) #p=0.0485

#males
cpue_month_M <- lm(cpue_month$M ~ cpue_month$date_string, data=cpue_month)
plot(cpue_month_M)

cpue_month_M_glm <- glm(cpue_month$M ~ cpue_month$date_string, data=cpue_month, family=tweedie(var.power=2, link.power=0))
summary(cpue_month_M_glm) #p=0.206

#adult females
cpue_month_AF <- lm(cpue_month$AF ~ cpue_month$date_string, data=cpue_month)
plot(cpue_month_AF)

cpue_month_AF_glm <- glm(cpue_month$AF ~ cpue_month$date_string, data=cpue_month, family=tweedie(var.power=2, link.power=0))
summary(cpue_month_AF_glm) #p=0.191


#adult males
cpue_month_AM <- lm(cpue_month$AM ~ cpue_month$date_string, data=cpue_month)
plot(cpue_month_AM)

cpue_month_AM_glm <- glm(cpue_month$AM ~ cpue_month$date_string, data=cpue_month, family=tweedie(var.power=2, link.power=0))
summary(cpue_month_AM_glm) #p=0.315

#juvenile females
cpue_month_JF <- lm(cpue_month$JF ~ cpue_month$date_string, data=cpue_month)
plot(cpue_month_JF)

cpue_month_JF_glm <- glm(cpue_month$JF ~ cpue_month$date_string, data=cpue_month, family=tweedie(var.power=2, link.power=0))
summary(cpue_month_JF_glm) #p=0.0332 


#juvenile males
cpue_month_JM <- lm(cpue_month$JM ~ cpue_month$date_string, data=cpue_month)
plot(cpue_month_JM)

cpue_month_JM_glm <- glm(cpue_month$JM ~ cpue_month$date_string, data=cpue_month, family=tweedie(var.power=2, link.power=0))
summary(cpue_month_JM_glm) #p=0.134
