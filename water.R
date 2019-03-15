water<-read.table(file="water.txt",header=TRUE)  #read in the data 
precip<-water[,1]
runoff<-water[,2]

library(ggplot2)
ggplot(water,aes((precip), (runoff)))+geom_point()+geom_smooth(se=FALSE, method="lm")
cor(precip,runoff)

slr <- lm(runoff~precip, data=water)

coef(slr)
summary(slr)



resid <- slr$residuals
fitted <- slr$fitted.values
plot(fitted, resid)+abline(h = 0, lty = 2)   #test for linearity & equal variance

library(lmtest)
bptest(slr)   #equal var is met
library(MASS)
std.res<-stdres(slr)
ggplot()+geom_histogram(aes(x=std.res))    #graphs histo
ks.test(std.res,"pnorm")  #test for normality

cd<-cooks.distance(slr)   #tests for outliers distance 
plot(cd, type = "h")
which(cooks.distance(slr)>.07)  #returns index for outliers



#####################Cross Val. ####################3


n.cv <-250
bias<-rep(NA, n.cv)
rpmse<-rep(NA,n.cv)
cvg <- rep(NA,n.cv)
wid<-rep(NA, n.cv)

n.test<-6
for(cv in 1:n.cv){
  test.obs<-sample(1:nrow(water), n.test)
  test.set<-water[test.obs,]
  train.set<-water[-test.obs,]
  
  train.lm<-lm(runoff~precip, data = train.set)
  
  pred<-predict.lm(train.lm, newdata = test.set, interval = "prediction", level = .95)
  
  bias[cv]<-mean(pred[,"fit"]-test.set[,"Runoff"])
  rpmse[cv]<-sqrt(mean((pred[,"fit"]-test.set[,"Runoff"])^2))
  cvg[cv]<-mean(pred[,"lwr"])<test.set[,"Runoff"] & pred[,"upr"]>test.set[,"Runoff"]
  wid<-mean(pred[,"upr"]-pred[,"lwr"])
  
}
mean(bias)
mean(cvg)
mean(rpmse)
ggplot()+geom_histogram(aes(x=rpmse))
#############################################3
confint(slr, level = .95)   #C.I. for 95%
predict.lm(slr, newdata = data.frame(precip=4.5), interval = "prediction", level = .95) 



