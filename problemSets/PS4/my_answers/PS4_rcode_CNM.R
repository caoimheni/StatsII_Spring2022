#####################
#Problem Set 4
#Caoimhe Ni Mhaonaigh
#Due 4th April
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

# set wd for current folder
setwd("~/Desktop/Applied Social Data Science /Stats2_Spring2021")

#We’re interested in modeling the historical causes of infant mortality.
#We have data from 5641 first-born in seven Swedish parishes 1820-1895.
#Using the ”infants” dataset in the eha library, fit a Cox Proportional Hazard model using mother’s age
#and infant’s gender as covariates. Present and interpret the output.

#Read in the data 
data(infants)
str(infants)

#Use Surv()
infants_surv <- with(infants, Surv(enter, exit, event))
stargazer(infants)

#Plot Kaplan Meier Plot 
km <- survfit(infants_surv ~ 1, data = infants)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan_Meier Plot", xlab = "Days", ylim = c(0.5, 1))
autoplot(km)

#For sex co-variate 
km_sex <- survfit(infants_surv ~ sex, data = infants)
autoplot(km_sex)

#Run a Cox Proportional Hazard Model, using mothers age and infants gender as covariates. 
add_surv <- coxph(Surv(enter, exit, event) ~ age + sex, data = infants)
summary(add_surv)
stargazer(add_surv)