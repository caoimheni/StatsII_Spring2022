#########################
#Problem Set 3 
#Caoimhe Ni Mhaonaigh
#Due: March 28th 2022
########################

#remove objects
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

lapply(c("stringr", "tidyverse", "MASS", "nnet", "ggplot2", "stargazer", "car"),  pkgTest)


#set the working directory 
setwd("~/Desktop/Applied Social Data Science /Stats2_Spring2021/PS3")
##############################################################################
#Question One 
#how governments’ management of public resources impacts economic prosperity.
##############################################################################

#First load in the data 

gdp_data <- read.csv("gdpChange.csv", stringsAsFactors = FALSE)

#Next assess the data 
head(gdp_data)
str(gdp_data)
summary(gdp_data)

#Some information about the data 
#Response variable = GDPWdiff = our Y, outcome variable, this is going to be categorical for 
#postive, negative and no change 


#Explanatory variables = X1 = REG: 1=Democracy; 0=Non-Democracy
#                      = X2 = OIL: 1=if the average ratio of fuel exports to total exports in 1984-86 
#                                  exceeded 50%;
#                                  0= otherwise



#######################################################################################################
#  1. Construct and interpret an unordered multinomial logit with GDPWdiff as the output and ”no change” 
#as the reference category, including the estimated cutoff points and coefficients.
#######################################################################################################

# We need to change the GDPWdiff category: ”positive”, ”negative”, or ”no change”

gdp_data <- within(gdp_data, {   
  GDPWdiff.cat <- NA # need to initialize variable
  GDPWdiff.cat[GDPWdiff < 0] <- "negative"
  GDPWdiff.cat[GDPWdiff == 0 ] <- "no change"
  GDPWdiff.cat[GDPWdiff > 0] <- "positive"
} )


#Next we need to make this data into factors 
gdp_data$GDPWdiff.cat <- as.factor(gdp_data$GDPWdiff.cat)


gdp_data$REG <- as.integer(as.logical(gdp_data$REG))
gdp_data$REG <- factor(gdp_data$REG,
                           levels = c(0,1),
                           labels = c("Non Democracy", "Democracy"))

gdp_data$OIL <- as.integer(as.logical(gdp_data$OIL))
gdp_data$OIL <- factor(gdp_data$OIL, 
                          levels = c(0,1), 
                          labels = c("Otherwise", "Ratio exceeded 50%"))

str(gdp_data)
summary(gdp_data)
stargazer(gdp_data)


gdp_data$GDPWdiff.cat <- relevel(gdp_data$GDPWdiff.cat, ref = "no change")


#Visualization of the dataset 

ggplot(gdp_data, aes(x = OIL , y = GDPWdiff.cat)) +
  geom_jitter(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(gdp_data, aes(x = REG, y = GDPWdiff.cat)) +
  geom_jitter(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



#The regression model 
model <- multinom(GDPWdiff.cat ~ REG + OIL, data = gdp_data)
summary(model)

stargazer(model, title = "Unordered Multinomial Logit")

#To get the P values and cut off points 

ctable <- coef(summary(model))
ctable


z <- summary(model)$coefficients/summary(model)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(model))

stargazer(ctable)

#######################################################################################################
#2. Construct and interpret an ordered multinomial logit with GDPWdiff as the outcome variable, including 
#the estimated cutoff points and coefficients.
#######################################################################################################
gdp_data <- read.csv("gdpChange.csv", stringsAsFactors = FALSE)

gdp_data <- within(gdp_data, {   
  GDPWdiff.cat <- NA # need to initialize variable
  GDPWdiff.cat[GDPWdiff < 0] <- "negative"
  GDPWdiff.cat[GDPWdiff == 0 ] <- "no change"
  GDPWdiff.cat[GDPWdiff > 0] <- "positive"
} )


#Next we need to make this data into factors 
gdp_data$GDPWdiff.cat <- as.factor(gdp_data$GDPWdiff.cat)


gdp_data$REG <- as.integer(as.logical(gdp_data$REG))
gdp_data$REG <- factor(gdp_data$REG,
                       levels = c(0,1),
                       labels = c("Non Democracy", "Democracy"))


gdp_data$OIL <- as.integer(as.logical(gdp_data$OIL))
gdp_data$OIL <- factor(gdp_data$OIL, 
                       levels = c(0,1), 
                       labels = c("Otherwise", "Ratio exceeded 50%"))


ordinal_model <- polr(GDPWdiff.cat ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(ordinal_model)

stargazer(ordinal_model, title = "Ordered Multinomial Logit")


#P value and confidence intervals 

ctable <- coef(summary(ordinal_model))
ctable

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 

(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(ordinal_model))

stargazer(ci)

#########################################
#Question 2 
#Municipal-level information from Mexico
#########################################

#First load in the data 
mexico_data <- read.csv("MexicoMuniData.csv", stringsAsFactors = FALSE)


#Look at the data 
head(mexico_data)
str(mexico_data)
summary(mexico_data)
stargazer(mexico_data)


#Variables of interest: outcome = Pan.visits.06
#main predictor of interest is whether the district was contested, 
#plus poverty measure 
#plus pan governor 

#PAN.governor.06 and competitive district are binary 

mexico_data <- within(mexico_data, {
  PAN.governor.06 <- as.logical(PAN.governor.06)
  competitive.district <- as.logical(competitive.district)
})


#Run a Poisson regression because the outcome is a count variable. 
#Is there evidence that PAN presidential candidates visit swing districts more? 
#Provide a test statistic and p-value.

poisson_model  <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_data, family = poisson)
summary(poisson_model)

stargazer(poisson_model)

ggplot(data = NULL, aes(x = poisson_model$fitted.values, y = mexico_data$PAN.visits.06)) +
  geom_jitter(alpha = 0.5) +
  geom_abline(color = "blue") 


#Is there evidence that PAN presidential candidates visit swing districts more? 

#In latex 

#Interpret the marginality.06 and PAN.governor.06 coefficients.

#Provide the estimated mean number of visits from the winning PAN presidential candidate 
#for a hypothetical district that was competitive (competitive.district=1), had
#an average poverty level (marginality.06 = 0), and a PAN governor (PAN.governor.06=1).

#                       Estimate    Questions 
#(Intercept)            -4.20317   
#Competitive.district0  -0.08135      1
#marginality.06         -2.08014     0
#PAN.governor.060       -0.31158      1
  
lamda <- exp((-4.20317*1) + (-0.8135*1) + (-2.08014*0) + (-0.31158*1))
lamda

#This means the estimated mean for the number of times the winning PAN presidential candidate in 2006 
#is [1] 0.004852555


