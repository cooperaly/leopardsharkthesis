remove.packages("rlang")
install.packages("rlang")

library(rlang)
library(devtools)
library(ggbiplot)


diet<-read.table(file="diet_sum.csv", header=TRUE, sep=',')
str(diet)

diet$age_class = as.factor(as.character(diet$age_class))
diet$sex = as.factor(as.character(diet$sex))
diet$sample_id = as.factor(as.character(diet$sample_id))
diet$new_sample_id = as.factor(as.character(diet$new_sample_id))
diet$date_collected = as.factor(as.character(diet$date_collected))


diet_pca<-prcomp(diet[,c(7:13)], center = TRUE,scale = TRUE)
loadings <- diet_pca$rotation
loadings
scores<-diet_pca$x
scores

#basic pca plot
ggbiplot(diet_pca,ellipse=FALSE)

#ageclass pca plot
ggbiplot(diet_pca,ellipse=TRUE, groups=diet$age_class) +
  theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank())+
  labs(y= "Standardized PC2 (22% explained var.)", x = "Standardized PC1 (33% explained var.)")+
  labs(colour = "Age Class")+
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank()) +
  theme(legend.title=element_text(size=18))+
  theme(legend.text=element_text(size=14))+
  theme(axis.text = element_text(size=14))+
  theme(axis.title = element_text(size=16))+
  ylim(-2.8, 2.3)+
  xlim(-2.7,2.4)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

#sex pca plot
ggbiplot(diet_pca,ellipse=TRUE, groups=diet$sex)+
theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank())+
  labs(y= "Standardized PC2 (19.8% explained var.)", x = "Standardized PC1 (29.3% explained var.)")+
  labs(colour = "Sex")+
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank()) +
  theme(legend.title=element_text(size=18))+
  theme(legend.text=element_text(size=14))+
  theme(axis.text = element_text(size=14))+
  theme(axis.title = element_text(size=16))+
  ylim(-2.8, 2.3)+
  xlim(-2.7,2.4)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
