bodyfat<-read.table(file="BodyFat.txt",header=TRUE)  #read in the data 

pairs(~brozek+age+weight+height+hip+abdom+chest,bodyfat)
cor(bodyfat)
model<-lm(brozek~.,data=bodyfat)
coef(model)
install.packages("car")
library(car)
avPlots(model)
resid <- model$residuals
fitted <- model$fitted.values
plot(fitted, resid, main = "Residuals vs. Fitted Values Scatterplot", xlab = "Fitted Values", ylab="Residuals")+abline(h = 0, lty = 2)
library(lmtest)
bptest(model)

cd<-cooks.distance(model)   #tests for outliers distance from
plot(cd, type = "h")
cd
library(MASS)
std.res<-stdres(model)
ks.test(std.res,"pnorm")

pred.df <-data.frame(age=seq(min(bodyfat$age),max(bodyfat$age),length=100),
                     weight=seq(min(bodyfat$weight),max(bodyfat$weight),length=100),
                     height=seq(min(bodyfat$height),max(bodyfat$height),length=100),
                     neck=seq(min(bodyfat$neck),max(bodyfat$neck),length=100),
                     chest=seq(min(bodyfat$chest),max(bodyfat$chest),length=100),
                     abdom=seq(min(bodyfat$abdom),max(bodyfat$abdom),length=100),
                     hip=seq(min(bodyfat$hip),max(bodyfat$hip),length=100),
                     thigh=seq(min(bodyfat$age),max(bodyfat$age),length=100),
                     knee=seq(min(bodyfat$knee),max(bodyfat$knee),length=100),
                     ankle=seq(min(bodyfat$ankle),max(bodyfat$ankle),length=100),
                     biceps=seq(min(bodyfat$biceps),max(bodyfat$biceps),length=100),
                     forearm=seq(min(bodyfat$forearm),max(bodyfat$forearm),length=100),
                     wrist=seq(min(bodyfat$wrist),max(bodyfat$wrist),length=100))
pred<-predict.lm(model, newdata = pred.df, interval = "prediction",level = .95)
pred<-as.data.frame(pred)

n.cv <-250
bias<-rep(NA, n.cv)
rpmse<-rep(NA,n.cv)
cvg <- rep(NA,n.cv)
wid<-rep(NA, n.cv)

n.test<-20
for(cv in 1:n.cv){
  test.obs<-sample(1:nrow(bodyfat), n.test)
  test.set<-bodyfat[test.obs,]
  train.set<-bodyfat[-test.obs,]
  
  train.lm<-lm(brozek~age, data = train.set)
  
  pred<-predict.lm(train.lm, newdata = test.set, interval = "prediction", level = .95)
  
  bias[cv]<-mean(pred[,"fit"]-test.set[,"brozek"])
  rpmse[cv]<-sqrt(mean((pred[,"fit"]-test.set[,"brozek"])^2))
  cvg[cv]<-mean(pred[,"lwr"])<test.set[,"brozek"] & pred[,"upr"]>test.set[,"brozek"]
  wid<-mean(pred[,"upr"]-pred[,"lwr"])
  
summary(model)$r.squared
mean(bias)
mean(cvg)
mean(rpmse)
mean(wid)
ggplot()+geom_histogram(aes(x=rpmse))

confint(model, level = .95)   #C.I. for 95%
predict.lm(model, newdata = data.frame(age=50,weight= 203, height= 67,
                                neck= 40.2, chest=114.8, abdom=108.1,
                                hip=102.5, thigh=61.3,knee= 41.1, ankle= 24.7, 
                               biceps= 34.1, forearm= 31, wrist= 18.3), 
                              interval = "confidence", level = .95)  #CI 
predict.lm(model, newdata = data.frame(age=50,weight= 203, height= 67,
                                       neck= 40.2, chest=114.8, abdom=108.1,
                                       hip=102.5, thigh=61.3,knee= 41.1, ankle= 24.7, 
                                       biceps= 34.1, forearm= 31, wrist= 18.3), 
                                      ,interval = "prediction", level = .95)  #PI 

