install.packages("BiodiversityR")
install.packages("ggsci")
install.packages("readxl")

library(BiodiversityR) # also loads vegan
library(ggplot2)
library(ggsci)
library(readxl)

#Lower Order Combos
  # mud shrimp / crab_spp  / Crangon / detritus / eelgrass / FIworms / Macoma / misc digested / red octo / unid_worm / teleost
  # crab_spp: unid crab, Dungeness crab, kelp crab
  # unid_worm: unid worm, polychaete spp
  # teleost: staghorn sculpin, threespine stickleback, shiner surfperch, unid_fish
prey_curve_low <-read.table(file="prey_curve_low.csv", header=TRUE, sep=",")

PClow <- specaccum(prey_curve_low)

plot(PClow, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", ylab="# Species",  xlab = "# Individuals",
     cex.lab = 2, cex.axis = 1.5)




