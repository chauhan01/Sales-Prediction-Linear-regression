

#-------------------setting up working directory------------------------

getwd()
path = "C:/Users/win10/Downloads/shubham data/MLRM Case Study/MLRM Case Study"
setwd(path)
getwd()


#----------preparing the enviroment for MLRM-----


list.of.packages <- c("boot", "car","QuantPsyc","lmtest","sandwich","vars","nortest","MASS","caTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caTools)
library(MASS)
library(boot)
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)


# reading the csv file


data = read.csv("Fn-UseC_-Marketing-Campaign-Eff-UseC_-FastF.csv")
data1<- data  #creating backup of orignal data



#------------------------------------Basic Exploration of the data--

str(data1)
summary(data1)


#converting MarketID, AgeOfStore, Promotion and week as factor

data1$MarketID<- as.factor(data1$MarketID)
data1$AgeOfStore<- as.factor(data1$AgeOfStore)
data1$Promotion<- as.factor(data1$Promotion)
data1$week<-as.factor(data1$week)



# checking levels in factor variables
levels(data1$MarketID)
levels(data1$MarketSize)
levels(data1$Promotion)
levels(data1$week)

# renaming the dependent variable

colnames(data1)[which(names(data) == "SalesInThousands")] <- "sit"


# detecting outliers using boxplot

boxplot(data1$sit)

outlier_values <- as.vector(boxplot.stats(data1$sit)$out)  # outlier values.



# Treating outliers using quantile method

quantile(data1$sit,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
data2 <- data1[data1$sit > 30,]


quantile(data2$sit,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
data3 <- data2[data2$sit < 65,]


quantile(data3$sit,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))


nrow(data1)-nrow(data3)  
  
  


# checking for missing values

colSums(is.na(data1))


#--------------------------Splitting the data into training and test data set------------------------#

set.seed(1)#This is used to produce reproducible results, everytime we run the model

spl <- sample.split(data3$sit, 0.7)

train <- subset(data3, spl == T)
test <- subset(data3, spl == F)

dim(train)

dim(test)

#------------------------------------------Fitting the model---------------------------------------#
#Iteration.1 We start with testing all variables

Model0 <- lm(sit~., train)
summary(Model0)

# Iteration 2 removing insignificant variables

Model1 <- lm(sit~ MarketID + MarketSize+ AgeOfStore + Promotion +week , train)
summary(Model1)

# Iterarion 3 

Model2 <- lm(sit~  MarketID+ MarketSize+ AgeOfStore + Promotion, train)
summary(Model2)

# Iteration 4

Model3 <- lm(sit~  I(MarketID== 2 )+I(MarketID== 5 )+I(MarketID== 7 )+I(MarketID== 8 )+I(MarketID== 9 )+ I(MarketID== 10 )
             + I(AgeOfStore== 3)+ I(AgeOfStore== 4)+ I(AgeOfStore== 5)+ I(AgeOfStore== 6)+ I(AgeOfStore== 7)+ I(AgeOfStore== 8)+ I(AgeOfStore== 9)+ I(AgeOfStore== 10)+ I(AgeOfStore== 11)+ I(AgeOfStore== 12)+ I(AgeOfStore== 13)
             + I(AgeOfStore== 14)+ I(AgeOfStore== 19)+ I(AgeOfStore== 25)+ Promotion + MarketSize , train )
summary(Model3)

# Iteration 5

Model4 <- lm(sit~  I(MarketID== 2 )+I(MarketID== 5 )+I(MarketID== 7 )+I(MarketID== 8 )+I(MarketID== 9 ) + I(MarketID== 10 )
             + I(AgeOfStore== 3)+ I(AgeOfStore== 4)+ I(AgeOfStore== 5)+ I(AgeOfStore== 6)+ I(AgeOfStore== 7)+ I(AgeOfStore== 8)+ I(AgeOfStore== 9)+ I(AgeOfStore== 10)+ I(AgeOfStore== 11)+ I(AgeOfStore== 13)
             + I(AgeOfStore== 14)+ I(AgeOfStore== 25)+ Promotion + MarketSize , train )
summary(Model4)

# Iteration 6

Model5 <- lm(sit~  I(MarketID== 2 )+I(MarketID== 5 )+I(MarketID== 7 )+I(MarketID== 8 )+I(MarketID== 9 ) + + I(MarketID== 10 )
             + I(AgeOfStore== 3)+ I(AgeOfStore== 4)+ I(AgeOfStore== 5)+ I(AgeOfStore== 6)+ I(AgeOfStore== 7)+ I(AgeOfStore== 8)+ I(AgeOfStore== 9)+ I(AgeOfStore== 10)+ I(AgeOfStore== 13)
             + I(AgeOfStore== 14)+ I(AgeOfStore== 25)+ Promotion + MarketSize , train )
summary(Model5)


# Iteration 7

Model6 <- lm(sit~  I(MarketID== 2 )+I(MarketID== 5 )+I(MarketID== 7 )+I(MarketID== 8 )+I(MarketID== 9 )+ + I(MarketID== 10 )
             + I(AgeOfStore== 3)+ I(AgeOfStore== 4)+ I(AgeOfStore== 5)+ I(AgeOfStore== 6)+ I(AgeOfStore== 7)+ I(AgeOfStore== 8)+ I(AgeOfStore== 9)+ I(AgeOfStore== 10)+ I(AgeOfStore== 13)
             + I(AgeOfStore== 14)+ Promotion + I(MarketSize == "Medium") , train )
summary(Model6)

# Iteration 8

Model7 <- lm(sit~  I(MarketID== 2 )+I(MarketID== 5 )+I(MarketID== 7 )+I(MarketID== 8 )+I(MarketID== 9 )+ + I(MarketID== 10 )
             + I(AgeOfStore== 3)+ I(AgeOfStore== 4)+ I(AgeOfStore== 5)+ I(AgeOfStore== 6)+ I(AgeOfStore== 7)+ I(AgeOfStore== 8)+ I(AgeOfStore== 9)+ I(AgeOfStore== 10)+ I(AgeOfStore== 13)
             + Promotion + I(MarketSize == "Medium") , train )
summary(Model7)

# Iteration 9

Model8 <- lm(sit~  I(MarketID== 2 )+I(MarketID== 5 )+I(MarketID== 7 )+I(MarketID== 8 )+I(MarketID== 9 )
             + I(AgeOfStore== 3)+ I(AgeOfStore== 4)+ I(AgeOfStore== 5)+ I(AgeOfStore== 7)+ I(AgeOfStore== 8)+ I(AgeOfStore== 9)+ I(AgeOfStore== 10)+ I(AgeOfStore== 13)
             + Promotion + I(MarketSize == "Medium") , train )
summary(Model8)

# Iteration 10

Model9 <- lm(sit~  I(MarketID== 2 )+I(MarketID== 5 )+I(MarketID== 7 )+I(MarketID== 8 )+I(MarketID== 9 )
             + I(AgeOfStore== 3)+ I(AgeOfStore== 5)+ I(AgeOfStore== 7)+ I(AgeOfStore== 8)+ I(AgeOfStore== 9)+ I(AgeOfStore== 10)+ I(AgeOfStore== 13)
             + Promotion + I(MarketSize == "Medium") , train )
summary(Model9)

par(mfrow=c(2,2))
plot(Model9)

#FinalModel
FinalModel <- lm(sit~  I(MarketID== 2 )+I(MarketID== 5 )+I(MarketID== 7 )+I(MarketID== 8 )+I(MarketID== 9 )
             + I(AgeOfStore== 3)+ I(AgeOfStore== 5)+  I(AgeOfStore== 7)+ I(AgeOfStore== 8)+ I(AgeOfStore== 9)+ I(AgeOfStore== 10)+ I(AgeOfStore== 13)
             + Promotion + I(MarketSize == "Medium"), train )
summary(FinalModel)




#------------------------------------Checking Assumptions----------------------------------------------

#Multicolinearity

vif(FinalModel) #no evidence of multicollinearity found

# Autocorrelation
durbinWatsonTest(FinalModel) #Since, the p-value is >0.05, we fail to reject H0: (No Autocorrelation) 


# Breusch-Pagan test
bptest(FinalModel)


#Cook-Weisberg test
ncvTest(FinalModel) 

##----------------------------------------------------------------------------------------------------------


## Get the predicted or fitted values
fitted(FinalModel)


#getting fitted values
train$pred<-fitted(FinalModel)
write.csv(train,file = "pred_data.csv")



#Calculating MAPE
attach(train)
MAPE<-print((sum((abs(sit-pred))/sit))/nrow(train))



############ Residual Analysis ############################################################################

res <- train

res$stu_res <- studres(FinalModel) ##Studentized residuals
res$stud.del.resids <- rstudent(FinalModel) ##studentized deleted residuals
res$leverage <- hatvalues(FinalModel) ## leverage values (hi)
res$cooks_dis <- cooks.distance(FinalModel) ## Cook's distance
res$dffits <- dffits(FinalModel) ## Dffit
res$dfbetas <- dfbetas(FinalModel) ## Dfbetas
res$cov_ratio <- covratio(FinalModel) ## Covariance Ratio



## Normality testing Null hypothesis is data is normal.

resids <- FinalModel$residuals


ad.test(resids) #get Anderson-Darling test for normality 
cvm.test(resids) #get Cramer-von Mises test for normaility 
lillie.test(resids) #get Lilliefors (Kolmogorov-Smirnov) test for normality 
pearson.test(resids) #get Pearson chi-square test for normaility 
sf.test(resids) #get Shapiro-Francia test for normaility 

qqnorm(resids)




##################################### Testing the model on test data ############################################################################





#Iteration 1

fit1 <- lm(sit~  I(MarketID== 2 )+I(MarketID== 5 )+I(MarketID== 7 )+I(MarketID== 8 )+I(MarketID== 9 )
             + I(AgeOfStore== 3)+I(AgeOfStore== 5)+ I(AgeOfStore== 7)+ I(AgeOfStore== 8)+ I(AgeOfStore== 9)+ I(AgeOfStore== 10)+ I(AgeOfStore== 13)
             + Promotion + I(MarketSize == "Medium") , test )
summary(fit1)

#Iteration 2

fit2<- lm(sit~  I(MarketID== 2 )+I(MarketID== 5 )+I(MarketID== 7 )+I(MarketID== 8 )+I(MarketID== 9 )
          + Promotion +I(MarketSize == "Medium"), test )
summary(fit2)



par(mfrow=c(2,2))
plot(fit2)        

#------------------------------------Checking Assumptions----------------------------------------------

#Multicolinearity

vif(fit2) #no evidence of multicollinearity found

# Autocorrelation
durbinWatsonTest(fit2) #No autocorrelation found 


# Breusch-Pagan test
bptest(fit2)  # Null hypothesis -> error is homogenious (p value should be more than 0.05)


#Cook-Weisberg test
ncvTest(fit2)       # p value greater than 0.05


#getting fitted values
test$pred<-fitted(fit2)
write.csv(test,file = "test_data_pred.csv")


#MAPE Test Data
attach(test)
MAPE<-print((sum((abs(sit-pred))/sit))/nrow(test))





############ Residual Analysis ############################################################################

res1 <- test

res1$stu_res <- studres(fit2) ##Studentized residuals
res1$stud.del.resids <- rstudent(fit2) ##studentized deleted residuals
res1$leverage <- hatvalues(fit2) ## leverage values (hi)
res1$cooks_dis <- cooks.distance(fit2) ## Cook's distance
res1$dffits <- dffits(fit2) ## Dffit
res1$dfbetas <- dfbetas(fit2) ## Dfbetas
res1$cov_ratio <- covratio(fit2) ## Covariance Ratio


## Normality testing Null hypothesis is data is normal.

resids1 <- fit2$residuals


ad.test(resids1) #get Anderson-Darling test for normality 
cvm.test(resids1) #get Cramer-von Mises test for normaility 
lillie.test(resids1) #get Lilliefors (Kolmogorov-Smirnov) test for normality 
pearson.test(resids1) #get Pearson chi-square test for normaility 
sf.test(resids1) #get Shapiro-Francia test for normaility 

qqnorm(resids1)



#Final model which fit the test data and can be used on unseen data 

FinalFit<- lm(sit~  I(MarketID== 2 )+I(MarketID== 5 )+I(MarketID== 7 )+I(MarketID== 8 )+I(MarketID== 9 )
          + Promotion +I(MarketSize == "Medium"), test )
