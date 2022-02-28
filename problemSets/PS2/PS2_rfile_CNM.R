#######################
#Problem Set 2 
#Caoimhe Ni Mhaonaigh
#Due: February 28th 2022
######################

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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

setwd("~/Desktop/Applied Social Data Science /Stats2_Spring2021")
#####################
# Problem 1
#####################

options(scipen = 999)
# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true"))

head(climateSupport)
str(climateSupport)
View(climateSupport)

#coercing from a character vector to a logical vector

climateSupport$choice <- as.numeric(as.factor(climateSupport$choice))-1
climateSupport$countries <- as.numeric(as.factor(climateSupport$countries))-1
climateSupport$sanctions <- as.numeric(as.factor(climateSupport$sanctions))-1

library(ggplot2)

#The Explanatory Variable is plotted on the X-axis and the Y-axis represents the Response Variable.
ggplot(data = climateSupport, mapping = aes(x= as.numeric(choice)-1, y = countries)) +
  geom_jitter() + 
  coord_flip()

ggplot(data = climateSupport, mapping = aes(x= as.numeric(choice)-1, y = sanctions)) +
  geom_jitter() + 
  coord_flip()


#Fit an additive model. Provide the summary output, the global null hypothesis, 
#and p-value. Please describe the results and provide a conclusion.

reg <- glm(choice ~ ., data = climateSupport, family = "binomial")
summary(reg)
coef(reg)

#(Intercept)   countries   sanctions 
#-0.1445813   0.3243567  -0.1235335

#Then we get this prediction formula 
yhat <- - 0.1445813 + 0.3243567(countries) - 0.1235335(sanctions) + error 

#To write what the categories mean 
#sanctions
#0 = None 
#1 = 5% 
#2 = 15% 

#countries 
#0 = 20 of 192 
#1 = 80 of 192 
#2 = 160 of 192 



#For the policy in which nearly all countries participate [160 of 192], 
#how does increasing sanctions from 5% to 15% change the odds that an individual will support the policy?
#(Interpretation of a coefficient)
#The regression formula 

#we want coutries to be in the category 160 of 192, so we will use 2. 

yhat <- - 0.1445813 + 0.3243567(countries) - 0.1235335(sanctions) + error
yhat <- - 0.1445813 + 0.3243567*2 - 0.1235335(sanctions) + error 

yhat_5<- - 0.1445813 + 0.3243567*2 - 0.1235335*1
yhat_15 <- - 0.1445813 + 0.3243567*2 - 0.1235335*2

diff = exp(yhat_15) - exp(yhat_5)
diff
#(b) For the policy in which very few countries participate [20 of 192], 
#how does increasing sanctions from 5% to 15% change the odds that an individual 
#will support the policy? (Interpretation of a coefficient)
yhat<- - 0.1445813 + 0.3243567(countries) - 0.1235335(sanctions) + error

yhat_l <- - 0.1445813 + 0.3243567*0 - 0.1235335*1 
yhat_m <- - 0.1445813 + 0.3243567*0 - 0.1235335*2

diff_2 <- exp(yhat_m) - exp(yhat_l)
diff_2


#(c) What is the estimated probability that an individual will support a policy 
#if there are 80 of 192 countries participating with no sanctions?
  
yhat_none <- - 0.1445813 + 0.3243567*1 - 0.1235335*0 
yhat_none <- exp(yhat_none)

x <- yhat_none/(1 + yhat_none)
x
# 0.5448232

#(d) Would the answers to 2a and 2b potentially change if we included the interaction term 
#in this model? Why?

#An interactive model 
reg2 <- glm(choice ~ countries + sanctions +countries:sanctions, data = climateSupport, family = "binomial")
summary(reg2)

coef(reg2)

#(Intercept)           countries           sanctions          countries:sanctions 
#-0.148144162         0.328007182        -0.121110523        -0.002454648

#the predictive formula 
yhat_int <- -0.148144162 + 0.328007182(countries) - 0.121110523(sanctions) - 0.002454648(countries*sanctions)



