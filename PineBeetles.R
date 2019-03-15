pine <- read.csv("PineBeetle2.csv", header=TRUE)


#explore data 
boxplot(Precip~Infested,data=pine,xlab="Infested",ylab="Annual precip(inches)")
boxplot(August_max~Infested,data=pine,xlab="Infested",ylab="Max August Temperature")

pine$Infested <- as.integer(pine$Infested)-1
plot(pine$Elev,pine$Infested)
plot(pine$Elev,jitter(pine$Infested,amount=.5))


library(bestglm)
best.pine <- bestglm(pine,family=binomial ,method="exhaustive",IC="AIC")
best.mod <- best.pine$BestModel
best.mod

#check assumptions
with(pine,scatter.smooth(Precip,Infested,xlab="Precip",
                         ylab="Infested",pch=19))
with(pine,scatter.smooth(January,Infested,xlab="January",
                         ylab="Infested",pch=19))
with(pine,scatter.smooth(August_max,Infested,xlab="August_Max",
                         ylab="Infested",pch=19,ylim=c(0,1)))
with(pine,scatter.smooth(Slope,Infested,xlab="Slope",
                         ylab="Infested",pch=19))
with(pine,scatter.smooth(Elev,Infested,xlab="Elev",
                         ylab="Infested",pch=19))


#confidence interval
be <- confint(best.mod,level=.95)
multiple.conf <- exp(be)

# cutoff
pred.probs <- predict.glm(best.mod,type="response")
thresh <- seq(0,1,length=100)
misclass <- rep(NA, length=length(thresh))

for(i in 1:length(thresh)){
 
  my.classification <- ifelse(pred.probs>thresh[i],1,0)
  misclass[i] <- mean(my.classification!= pine$Infested)
}

min.thre <- thresh[which.min(misclass)]
min.thre
plot(thresh,misclass,type="l",main="Expected Threshold Plot")


#check fit of model

pred.class <- (pred.probs > min.thre)
true.class <- pine$Infested
table(pred.class,true.class)
#################################################33


n.cv <- 500 
n.test <- round(.1*nrow(pine))
cutoff <- min.thre

sens <- rep(NA,n.cv)
spec <- rep(NA,n.cv)
ppv <- rep(NA,n.cv)
npv <- rep(NA,n.cv)

for(cv in 1:n.cv) {
  #seperate into test and training sets
  obs.test <- sample(1:2310,231)
  test.set <- pine[obs.test,]
  train.set <- pine[-obs.test,]
  #fit best model to training set 
  train.model <- glm(Infested~January+August_max+Slope+Elev+Precip+NC+SE+SW,data=train.set,family=binomial)
  #use fitted model to predict test set 
  pred.probs <- predict.glm(train.model,newdata=test.set,type="response")
  #classify according to threshold
  test.class <- ifelse(pred.probs>cutoff,1,0)
  #create a confusion matrix
  conf.mat <- addmargins(table(factor(test.set$Infested,levels=c(0,1)),factor(test.class,levels=c(0,1))))
 
  sens[cv] <- conf.mat[2,2]/conf.mat[2,3]
  spec[cv] <- conf.mat[1,1]/conf.mat[1,3]
  ppv[cv] <- conf.mat[2,2]/conf.mat[3,2]
  npv[cv] <- conf.mat[1,1]/conf.mat[1,3]
  
}
mean(sens)
mean(spec)
mean(ppv)
mean(npv)

#predict for new plot
yr.one <- data.frame(Year=2018,January=-13.98,August_max=15.89,Precip=771.13,Elev=1901.95,SE="Yes",Slope=18.07,SW="No",NC="No")
pred.log <- predict.glm(best.mod,newdata=yr.one)
pred.prob <- exp(pred.log)/(1+exp(pred.log))
pred.pro<- predict.glm(best.mod,newdata=yr.one,type="response")
first<-pred.pro

yr.two <-data.frame(Year=2019,January=-17.8,August_max=18.07,Precip=788.54,Elev=1901.95,SE="Yes",Slope=18.07,SW="No",NC="No")
pred.pro<- predict.glm(best.mod,newdata=yr.two,type="response")
second<-pred.pro

yr.three <-data.frame(Year=2020,January=-17.27,August_max=16.74,Precip=677.63,Elev=1901.95,SE="Yes",Slope=18.07,SW="No",NC="No")
pred.pro<- predict.glm(best.mod,newdata=yr.two,type="response")
third<-pred.pro

yr.five <-data.frame(Year=2022,January=-15.99,August_max=18.23,Precip=732.32,Elev=1901.95,SE="Yes",Slope=18.07,SW="No",NC="No")
pred.pro<- predict.glm(best.mod,newdata=yr.five,type="response")
five<-pred.pro

yr.ten <- data.frame(Year=2027,January=-12.44,August_max=16.96,Precip=801.22,Elev=1901.95,SE="Yes",Slope=18.07,SW="No",NC="No")
pred.pro<- predict.glm(best.mod,newdata=yr.ten,type="response")
ten<-pred.pro
