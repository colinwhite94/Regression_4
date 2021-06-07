library(MASS)     ## stdres
library(car)      ## added-variable plots
library(normtest) ## jb.norm.test
library(lmtest)   ## bptest


## Read in Supervisor Data
setwd("~/Desktop/1A School/1A Winter 2021/STAT330/HW4")
super = read.table("fileDownload.txt",header = TRUE)

head(super)
names(super)
n = nrow(super)

## Explore the Data

plot(super,cex = 0.5,pch =20)
pairs(super,cex = 0.8,pch =20)


pairs(super[,c(1,2:4)],cex = 0.8,pch =20)
pairs(super[,c(1,5:7)],cex = 0.8,pch =20)

round(cov(super),2)
round(cor(super),2)

cor(super)[1,]

## Fit a MLR Model

super.lm = lm(brozek ~ age + weight + height + neck +
                chest + abdom + hip + thigh + knee + ankle +
                biceps + forearm + wrist, data=super)
super.lm = lm(brozek ~ .,data=super)

summary(super.lm)

## Check model assumptions


## Added variable plot to check linearity assumption
###### For the pth variable
#### y-axis - plot residuals of a regression model y ~ all X but x_p
## the part of the response that isn't explained by another covariate
#### x-axis - plot residuals of a regression model x_p ~ all X but x_p 
## the part of the pth covariante that isn't explained by another covariate


##each of these should be linear
avPlots(super.lm,pch = 20,cex = 0.8) 

plot(super.lm$fitted.values,super.lm$residuals,pch=19, ylab="Standardized Residuals", xlab="Fitted values")
abline(a=0,b=0)

## equal variance

plot(super.lm$fitted.values,super.lm$residuals,pch=19)
abline(a=0,b=0)

plot(super$neck,super.lm$residuals,pch=19)
abline(a=0,b=0,col = "red")

bptest(super.lm)

## Normality

hist(stdres(super.lm),freq = FALSE, xlab="Standardized Residuals", ylab="Density", ylim=c(0,.4), main="Histogram of STD. Residuals")
curve(dnorm,from = -3,to = 4,add = TRUE)

qqnorm(stdres(super.lm))
abline(0,1)


jb.norm.test(stdres(super.lm),nrepl = 1e5)
ks.test(stdres(super.lm),"pnorm")


## Predict for a new supervisor where 
##Complaints=100,Privileges=0,Learn=100,Raises=100,Critical=100,Advance=0

predict.lm(super.lm,newdata=data.frame(age= 50, weight= 203, 
                                       height= 67, neck= 40.2, 
                                       chest=114.8, abdom=108.1,
                                       hip=102.5, thigh=61.3,
                                       knee= 41.1, ankle= 24.7,
                                       biceps= 34.1, forearm= 31,
                                       wrist= 18.3),
                                      interval = "prediction",level = 0.95)

apply(super,2,range)

## Perform a a series of cross-validation studies
n.test = 4
n.cv = 1e4

bias = numeric(n.cv)
rpmse = numeric(n.cv)
coverage = numeric(n.cv)
width = numeric(n.cv)


for(i in 1:n.cv){
  test.obs = sample(1:nrow(super),n.test)
  super.test = super[test.obs,]
  super.train = super[-test.obs,]
  train.lm = lm(brozek~.,data=super.train)
  preds = predict.lm(train.lm,newdata=super.test,interval = "prediction",
                     level = 0.95)
  bias[i] = mean(preds[,1]-super.test$brozek)
  rpmse[i] = sqrt(mean((preds[,1]-super.test$brozek)^2))
  width[i] = mean(preds[,3]-preds[,2])
  coverage[i] = mean(preds[,2] < super.test$brozek & preds[,3] > super.test$brozek)
  
}



mean(bias)
mean(rpmse)
mean(coverage)
mean(width)


range(super$brozek)
sd(super$brozek)






