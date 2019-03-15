pm<-read.table(file="PM.txt",header=TRUE)  #read in the data 
particles<-pm[,1]
cars<-pm[,2]

library(ggplot2)
ggplot(pm,aes(cars,particles))+geom_point()+ggtitle("Particle Pollution Scatterplot")+theme(plot.title = element_text(hjust = 0.5))+xlab("Number of Cars")+ylab("PM Level")

cov(particles, cars) #covariance
cor(cars, particles) #correlation
slr<-lm((particles)~(cars), data=pm)
summary(slr)

#testing for linearity
resid <- slr$residuals
fitted <- slr$fitted.values
plot(fitted, resid, main = "Residuals vs. Fitted Values Scatterplot", xlab = "Fitted Values", ylab="Residuals")+abline(h = 0, lty = 2)

library(lmtest)
bptest(slr)       #test for homoskedacity(equal variance) it's not equal here needs transformation

library(MASS)
std.res<-stdres(slr)
ggplot()+geom_histogram(aes(x=std.res))+xlab("Standardized Residuals")+ylab("Frequency")+ggtitle("  Histogram of standardized residuals")
ks.test(std.res,"pnorm")  #test for normality a high pvalue means it's normal

cd<-cooks.distance(slr)   #tests for outliers distance from
plot(cd, type = "h")



#transformation of data

ggplot(pm,aes(sqrt(cars),log(particles)))+geom_point()+geom_smooth(se=FALSE, method="lm")+xlab("Number of Cars")+ylab("PM Level")+ggtitle("Fitted Transformed Model")+theme(plot.title = element_text(hjust = 0.5 ))
cor(sqrt(cars),log(particles))

slrTrans<-lm(log(particles)~sqrt(cars), data=pm)
summary(slrTrans)

coef(slrTrans) ##slope/ intercept values

resid <- slrTrans$residuals
fitted <- slrTrans$fitted.values
plot(fitted, resid, main = "Residuals vs. Fitted Values Scatterplot (Transformed)", xlab = "Fitted Values", ylab="Residuals", cex.main=0.95)+abline(h = 0, lty = 2)

library(lmtest)
## Ho: the variance is constant            
##  Ha: the variance is not constant  
bptest(slrTrans)       #test for homoskedacity(equal variance) if small pvalue reject null hypo and infer that heterosk is present
library(MASS)
std.res<-stdres(slrTrans)
ggplot()+geom_histogram(aes(x=std.res))+xlab("Standardized Residuals")+ylab("Frequency")+ggtitle("Histogram of standardized residuals")+theme(plot.title = element_text(hjust = 0.5 ))
ks.test(std.res,"pnorm")  #test for normality a high pvalue means it's normal

cd<-cooks.distance(slrTrans)   #tests for outliers distance from
plot(cd, type = "h")




pred.df <-data.frame(cars=seq(min(sqrt(pm$Cars)),max(sqrt(pm$Cars)),length=100))   #makes data frame consisting of numbers between the max and min co2 levels
pred<-predict.lm(slrTrans, newdata = pred.df, interval = "prediction",level = .95)
pred<-as.data.frame(pred)

ggplot(pm,aes(sqrt(cars),log(particles)))+geom_point()+geom_smooth(method="lm")+geom_line(data=pred, aes(x=pred.df$cars, y=lwr,col="red"))+geom_line(data=pred, aes(x=pred.df$cars, y=upr,col="red")) #plots the predicition interval

n.cv <-500
bias<-rep(NA, n.cv)
rpmse<-rep(NA,n.cv)
cvg <- rep(NA,n.cv)
wid<-rep(NA, n.cv)

n.test<-50
for(cv in 1:n.cv){
  test.obs<-sample(1:nrow(pm), n.test)
  test.set<-pm[test.obs,]
  train.set<-pm[-test.obs,]
  
  train.lm<-lm(log(Particles)~sqrt(Cars), data = train.set)
  
  pred<-predict.lm(train.lm, newdata = test.set, interval = "prediction", level = .95)
  
  bias[cv]<-mean(pred[,"fit"]-test.set[,"Particles"])
  rpmse[cv]<-sqrt(mean((pred[,"fit"]-test.set[,"Particles"])^2))
  cvg[cv]<-mean(pred[,"lwr"])<test.set[,"Particles"] & pred[,"upr"]>test.set[,"Particles"]
  wid<-mean(pred[,"upr"]-pred[,"lwr"])
  
}

mean(bias)
mean(cvg)
mean(rpmse)
mean(wid)
ggplot()+geom_histogram(aes(x=rpmse))

confint(slrTrans, level = .95)   #C.I. for 95%
predict.lm(slrTrans, newdata = data.frame(cars=1800), interval = "prediction", level = .95)  #PI for precip=400


