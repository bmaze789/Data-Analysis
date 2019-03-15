farm<-read.table(file="Farms3.txt",header=TRUE) 
pairs(farm)
farm<-farm[,c(2:ncol(farm),1)]

mlr<-lm(farm$acrePrice~.,data=farm)
resid <- mlr$residuals
fitted <- mlr$fitted.values
plot(fitted, resid)+abline(h = 0, lty = 2)

library(lmtest)
bptest(mlr)   #equal var is not met

farm$acrePrice<- log(farm$acrePrice)
vs.ref<- bestglm(farm, IC="AIC", method = "exhaustive")
vs.ref$BestModel
plot(vs.ref$Subsets$AIC, type = "b",pch=19,xlab = "# of Vars",ylab="AIC")

library(car)
avPlots(vs.ref$BestModel)  #linear

bptest(vs.ref$BestModel)   #equal var is met
std.res<-stdres(vs.ref$BestModel)
ggplot()+geom_histogram(aes(x=std.res))    #graphs histo
ks.test(std.res,"pnorm")  #test for normality

summary(vs.ref$BestModel)




farm2<-read.table(file="Farms3.txt",header=TRUE) 
farm2<-farm[,c(2:ncol(farm),1)]
n.cv <- 250 
bias<-rep(NA, n.cv)
rpmse<-rep(NA,n.cv)
cvg <- rep(NA,n.cv)
wid<-rep(NA, n.cv)

n.test<-42
for(i in  1:n.cv){

  test.obs <- sample(1:nrow(farm2),n.test)
  test.set <- farm2[test.obs,]
  train.set <- farm2[-test.obs,]

  train.lm <- lm(log(acrePrice)~improvements+tillable+crpPct+WC+productivity+NW,data=train.set)
  pred <- exp(predict.lm(train.lm,newdata=test.set,interval="prediction",level=.95))
  
  
  bias[i] <- mean((pred[,"fit"]-test.set$acrePrice))
  rpmse[i] <- sqrt(mean((pred[,"fit"]-test.set$acrePrice)^2))
  cvg[i] <- mean(pred[,"lwr"]<test.set$acrePrice & pred[,"upr"]>test.set$acrePrice)
  wid[i] <-mean(pred[,"upr"]-pred[,"lwr"])
  
}


mean(bias)
mean(cvg)
mean(rpmse)
mean(wid)

inter <- lm(acrePrice~improvements+tillable+crpPct+WC+productivity*NW,data=farm)
summary(inter)
confint(inter,level=.95)

t.test(farm$productivity~farm$NW)

pred.df <- data.frame(improvements=0,tillable=94,crpPct=0,productivity=96,NW="Yes",WC="No")
pred<-predict.lm(inter, newdata = pred.df, interval = "prediction",level = .95)
exp(pred)
summary(vs.ref$BestModel)
