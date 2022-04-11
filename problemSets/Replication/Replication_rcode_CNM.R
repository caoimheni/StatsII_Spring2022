#########################
#Replication Presentation
#Caoimhe Ni Mhaonaigh
#11 April 2022
#########################


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

lapply(c("tidyverse", "VGAM", "ggplot2"),  pkgTest)
options(scipen = 999)

#Set working directory 
setwd("~/Desktop/Applied Social Data Science /Stats2_Spring2021")


#Load in the data 
data <- read.csv("AmesHousing.csv", stringsAsFactors = TRUE)
str(data)

########################################################################################
#There was no code for the paper, so i replcaited the methedology performed in the study
########################################################################################

#Examine Data Completeness and determine the percent of the Null/or Missing Values. 

# calculating the product of dimensions of dataframe 
totalcells = prod(dim(data))
# calculating the number of cells with na
missingcells = sum(is.na(data))
# calculating percentage of missing values
percentage = (missingcells * 100 )/(totalcells)
print (percentage)

#This shows we have 5.8% of missing cells, next we will remove coumns with more than 40% missing data 

#Remove features with more than 40% Missing/or Null Value

missing_values <- lapply(data,function(x) { length(which(is.na(x)))})
missing_values
#$Pool.QC
#[1] 2917
#$Misc.Feature
#[1] 2824
#$Fence
#[1] 2358
#$Fireplace.Qu
#[1] 1422
#$Alley
#[1] 2732

#Remove these features from the dataset
drop <- c("Pool.QC","Misc.Feature", "Fence", "Fireplace.Qu", "Alley")
data = data[,!(names(data) %in% drop)]


#################################################
#to replace NA's in data with means of the column. 
#################################################

#The Mean for Numeric Data 
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

#Below is for Categorical data 

calc_mode <- function(x){
  distinct_values <- unique(x)
  distinct_tabulate <- tabulate(match(x, distinct_values))
  distinct_values[which.max(distinct_tabulate)]
}

#Need to put in the mode for the categorical data 

data$Garage.Qual[is.na(data$Garage.Qual)]<-calc_mode(data$Garage.Qual)
data$Garage.Cond[is.na(data$Garage.Cond)]<-calc_mode(data$Garage.Cond)
data$Garage.Finish[is.na(data$Garage.Finish)]<-calc_mode(data$Garage.Finish)
data$Garage.Type[is.na(data$Garage.Type)]<-calc_mode(data$Garage.Type)
data$BsmtFin.Type.2[is.na(data$BsmtFin.Type.2)]<-calc_mode(data$BsmtFin.Type.2)
data$BsmtFin.Type.1[is.na(data$BsmtFin.Type.1)]<-calc_mode(data$BsmtFin.Type.1)
data$Bsmt.Exposure[is.na(data$Bsmt.Exposure)]<-calc_mode(data$Bsmt.Exposure)
data$Bsmt.Cond[is.na(data$Bsmt.Cond)]<-calc_mode(data$Bsmt.Cond)
data$Bsmt.Qual[is.na(data$Bsmt.Qual)]<-calc_mode(data$Bsmt.Qual)

#Now there are no NA's in the data 
sum(is.na(data))

###########################################################################################
#Conduct Exploratory Data Analysis (EDA) for the response variable and the features as well
###########################################################################################

library(ggplot2)
ggplot(data = data, aes(x=SalePrice)) + geom_histogram(color="blue", fill= "blue", bins = 70)

#To log transform SalePrice
data$SalePrice <- log(data$SalePrice) 

#Plot log transformed data 
ggplot(data = data, aes(x=SalePrice)) + geom_histogram(color="blue", fill= "blue", bins = 70)


#Box plot of price vs neighborhood 

data %>%
  mutate(Neighborhood = fct_reorder(Neighborhood, SalePrice)) %>%
  ggplot( aes(x=Neighborhood, y=SalePrice, fill=Neighborhood)) +
  geom_boxplot() +
  xlab("") +
  theme_bw()

############
#Correlation
############

#Multicollinearity assumption: linear regression model assumes that there is 
#little or no multicollinearity in the data. Multicollinearity occurs when the 
#independent variables are highly correlated with each other. 
#We have checked the correlation between all predictors. 
#Some predictors were found to be highly correlated with each other and with 
#the response variable SalePrice. We have identified these highly correlated 
#predictors and removed the predictors with more than 50% correlation from the 
#dataset in Step 10. So, this assumption holds.


#Simple correlation plot, for variables over 0.5
library(corrplot)
corr_simple <- function(data=data,sig=0.5){
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  corr <- cor(df_cor)
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  corr[corr == 1] <- NA 
  corr <- as.data.frame(as.table(corr))
  corr <- na.omit(corr) 
  corr <- subset(corr, abs(Freq) > sig) 
  corr <- corr[order(-abs(corr$Freq)),] 
  print(corr)
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")

  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}
corr_simple(data)


#Here we should remove the features that are too highly correlated with eachother, but the author did not, so i 
#will run the RF and return to it 

#remove features with near zero variance 
library(caret)
names(data)[nearZeroVar(data)]

#These are the columns that the study found 
#BsmtFinSF1,
#X1stFlrSF,
#GarageCars,
#X2ndFlrsSF,
#BsmtUnfSF,
#CentralAir,
#BedroomAbvGr,
#GarageFinish.


#Checking Linearity 

ggplot(data, aes(x=Overall.Qual, y=SalePrice)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data, aes(x=Total.Bsmt.SF, y=SalePrice)) + 
  geom_point()+
  geom_smooth(method=lm)


#Run a random forest to look at feature selection 
library(randomForest)

set.seed(100)
train <- sample(nrow(data), 0.5*nrow(data), replace = FALSE)
TrainSet <- data[train,]
ValidSet <- data[-train,]
summary(TrainSet)
summary(ValidSet)

RF_model <- randomForest(SalePrice ~ ., data = TrainSet, importance = TRUE)
RF_model

importance(RF_model)        
varImpPlot(RF_model)   


#This shows what the Study got, but if we remove the columns that are highly correlated and removal of the zero 
#variance objects 
drop <- c("Overall.Qual",
          "Gr.Liv.Area",
          "Garage.Cars",
          "Garage.Area",
          "Total.Bsmt.Sf",
          "X1st.Flr.SF",
          "Full.Bath",
          "Tot.Rms.Abv.Grd",
          "Year.Built",
          "Year.Remod.Add", 
          "BsmtFin.SF.1", 
          "X1st.Flr.SF", 
          "Garage.Cars", 
          "X2nd.Flr.SF",
          "Bsmt.Unf.SF", 
          "Central.Air", 
          "Bedroom.AbvGr", 
          "Garage.Finish")

data = data[,!(names(data) %in% drop)]


#Rerun the random forest now that the highly correlated columns are dropped. 
set.seed(100)
train <- sample(nrow(data), 0.5*nrow(data), replace = FALSE)
TrainSet <- data[train,]
ValidSet <- data[-train,]
summary(TrainSet)
summary(ValidSet)

RF_model2 <- randomForest(SalePrice ~ ., data = TrainSet, importance = TRUE)
RF_model2

importance(RF_model2)        
varImpPlot(RF_model2)   


#I will now run the model using the 12 most important features using the 
#refined dataset from the intial steps. 

data <- data[, c("SalePrice",
                 "TotRms.AbvGrd",
                 "Neighborhood",
                 "Fireplaces",
                 "MS.SubClass",
                 "Total.Bsmt.SF",
                 "Exter.Qual",
                 "Lot.Area",
                 "Kitchen.Qual", 
                 "House.Style", 
                 "Mas.Vnr.Area", 
                 "PID", 
                 "Lot.Frontage"
                 )]



model1 <- lm(SalePrice ~., data = data)
summary(model1)

#Changing the neighborhood variable according to the residuals, as according to Tutorial 10 in Term 1 

zip_groups <- data %>% # our dplyr code for creating a 3-level grouping for zipcode
  mutate(resid = resid(model1)) %>%
  group_by(Neighborhood) %>%
  summarise(median = median(resid),
            n = n()) %>%
  arrange(desc(median)) %>%
  mutate(sum_n = cumsum(n),
         ZipGroup = ntile(sum_n, 3)) 

data <- data %>% # joining the zipcode groups to our original dataset
  left_join(select(zip_groups, Neighborhood, ZipGroup), by = "Neighborhood")


#Regression with altered Neighborhood category. 

data <- data[, c("SalePrice",
                 "TotRms.AbvGrd",
                 "ZipGroup",
                 "Fireplaces",
                 "MS.SubClass",
                 "Total.Bsmt.SF",
                 "Exter.Qual",
                 "Lot.Area",
                 "Kitchen.Qual", 
                 "House.Style", 
                 "Mas.Vnr.Area", 
                 "PID", 
                 "Lot.Frontage"
)]

model2 <- lm(SalePrice ~., data = data)
summary(model2)

#Ran a mode using the top 6 feautres instead
data <- data[, c("SalePrice",
                 "TotRms.AbvGrd",
                 "ZipGroup",
                 "Fireplaces",
                 "MS.SubClass",
                 "Total.Bsmt.SF",
                 "Lot.Area" 
)]

model3 <- lm(SalePrice ~., data = data)
summary(model3)

#load car package
library(car)

#Plots 
avPlots(model1)
avPlots(model2)
avPlots(model2)
