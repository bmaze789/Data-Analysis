speed<-read.table(file="StoppingDistance.txt",header=TRUE)  #read in data
spd<-speed[,1]
dist <- speed[,2]
library(ggplot2)
ggplot(speed,aes(spd,dist))+geom_point()+geom_smooth(se=FALSE)  #plots scatter plot with smooth curve
cor(spd,dist)

slr <- lm(dist~spd,data=speed) #slr model
summary(slr) 

resid <- slr$residuals
fitted <- slr$fitted.values
plot(fitted, resid)+abline(h = 0, lty = 2)

library(lmtest)
bptest(slr)
library(MASS)

stresid<-stdres(slr)
##############################transform
sqdist<-sqrt(dist)

ggplot(speed,aes(sqspd,sqdist))+geom_point()+geom_smooth(se=FALSE, method = "lm")  #plots scatter plot with smooth curve
cor(lnspd,lndist)

sqslr <- lm(sqrt(dist)~(spd),data=speed)
resid <- sqslr$residuals
fitted <- sqslr$fitted.values
plot(fitted, resid)+abline(h = 0, lty = 2)

summary(sqslr)$r.squared 

library(lmtest)
bptest(sqslr)
library(MASS)


stresid<-stdres(sqslr)

ggplot(speed)+geom_histogram(aes(x=stresid))
ggplot(speed)+geom_density(aes(x=stresid))

ks.test(stresid,"pnorm") 

ListSpeeds <-(seq(0,40, length=62))  #makes list of random speeds 0-40
predicts<-predict.lm(sqslr, ListSpeeds)           #makes predictions based off of speeds
transpredicts <-predicts^2                            #transform the distance by squaring
predDF<-data.frame(S=ListSpeeds, D=transpredicts)         #add new data to dataframe


ggplot()+geom_point(data=speed,aes(x=Speed,y=Distance))+geom_line(data=predDF,aes(x=S,y=D))

slrPred<-lm(D~S, predDF)

summary(slrPred)
coef(slrPred)











myData<- sample(1:62(speed),5)
n.test<-10
bias<- rep(NA, 100)
for(i in 1:100){
  test.obs<-sample(1:62(myData), n.test)
  
  test.set <- mydata[test.obs,]
  train.set <- mydata[-test.obs,]
  
  train.lm <- lm(y~x,data=train.set)
  test.preds <- predict.lm()
  
  bias[i] <- mean(test.preds-test.set$y)
  
  
}

