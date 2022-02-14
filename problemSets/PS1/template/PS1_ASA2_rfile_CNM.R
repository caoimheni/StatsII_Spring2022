################################
#Applied Statistical Analysis 2 
#Problem Set 1
#Caoimhe Ni Mhaonaigh
#14th February 2022
################################

#####################
# load libraries
# set wd
# clear global .envir
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

lapply(c("stringr", "ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###########
#Question 1 
###########

#Write an R function that implements this test where the reference distribution is normal.
#As a hint, you can create the empirical distribution and theoretical CDF using this code:

set.seed(123)
options(scipen = 999) 
data <- rcauchy(1000, location = 0, scale = 1)
ECDF<- ecdf(data) 
empiricalCDF <- ECDF(data)

#plot the data 
plot(ECDF, verticals=TRUE, do.points=FALSE, col="blue") 

# generate test statistic
D<- max(abs(empiricalCDF - pnorm(data)))
D 

#Built in function 
ks.test(data, "pnorm")
#D^- = 0.13222, p-value = 0.0000000000000006523

#Writing my own function
 kolmogorov_smirnov <- function(data) {
  D<- max(abs(empiricalCDF - pnorm(data)))
  for(k in range(1:length(data))){  
    q <- -((2*k-1)^2)*(pi^2)
    z <- sqrt(2*pi)
    for (x in data){
      p <- ((z/x)*sum(exp(q)/(8*(x^2))))
      return(c(D, p))
      }
    }
  }

kolmogorov_smirnov(data)


###########
#Question 2 
###########
#Estimate an OLS regression in R that uses the Newton-Raphson algorithm
#(specifically BFGS, which is a quasi-Newton method), and show that you get 
#the equivalent results to using lm. Use the code below to create your data.
set.seed(123)
data <- data.frame(x = runif(200, 1, 10)) 
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)


#Show that the estimate gives the equivalent of this. 
coef(lm(data$y ~ data$x))


#OLS regression in R that uses the Newton-Raphson algorithm: 
OLS_estimate <- function(outcome, input, parameter) {
  n      <- nrow(input)
  k      <- ncol(input)
  beta   <- parameter[1:k]
  sigma2 <- parameter[k+1]^2
  e      <- outcome - input%*%beta
  logl   <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )
  return(-logl)
}

results_norm <- optim(fn=OLS_estimate,
                      outcome=data$y,
                      input=cbind(1, data$x),
                      par=c(1,1,1),
                      hessian=T,
                      method="BFGS")
results_norm$par



