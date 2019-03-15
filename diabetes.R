diabetes<-read.table(file="Diabetes.txt",header=TRUE) 

delete <- which(diabetes$diastolic==0 | diabetes$glucose==0 | diabetes$triceps==0 | diabetes$insulin==0|diabetes$bmi==0)
diabetes <- diabetes[-delete,]

plot(diabetes$glucose,jitter(diabetes$diabetes,amount=.3))
plot(diabetes$diastolic,jitter(diabetes$diabetes,amount=.3))

scatter.smooth(diabetes$glucose,jitter(diabetes$diabetes,amount=.3),ylab="Diabetes",xlab="Glucose")
scatter.smooth(diabetes$diastolic,jitter(diabetes$diabetes,amount=.3),ylab="Diabetes",xlab="Diastolic")
boxplot(pregnant~diabetes,data=diabetes,xlab="Diabetes",ylab="Pregnant")
##############################################

library(bestglm)
best.diabetes<- bestglm(diabetes,family=binomial,method="exhaustive",IC="AIC")

mod <- glm(diabetes ~., data=diabetes, family=binomial)
mod <- best.diabetes$BestModel


scatter.smooth(diabetes$glucose,jitter(diabetes$diabetes,amount=.3),ylab="Diabetes",xlab="Glucose")
scatter.smooth(diabetes$bmi,jitter(diabetes$diabetes,amount=.3),ylab="Diabetes",xlab="BMI")
scatter.smooth(diabetes$pedigree,jitter(diabetes$diabetes,amount=.3),ylab="Diabetes",xlab="Pedigree")
scatter.smooth(diabetes$age,jitter(diabetes$diabetes,amount=.3),ylab="Diabetes",xlab="Age")
scatter.smooth(diabetes$pregnant,jitter(diabetes$diabetes,amount=.3),ylab="Diabetes",xlab="Pregnant")

#############Confi. intervals###############
exp.conf <- exp(confint(mod,level=.95))
exp.conf
percent.conf <- 100*(exp.conf-1)


pred.probs <- predict.glm(mod,type="response")
thresh <- seq(0,1,length=100)
misclass <- rep(NA,length=length(thresh))
for(i in 1:length(thresh)) {
  #If probability greater than threshold then 1 else 0
  my.classification <- ifelse(pred.probs>thresh[i],1,0)
  # calculate the pct where my classification not eq truth
  misclass[i] <- mean(my.classification!=diabetes$diabetes)

}

min.thresh<-thresh[which.min(misclass)]
plot(thresh,misclass,type="l",xlab="Cutoff",ylab="Misclass",main="Misclass Vs. Cutoff")

pred.class <- (pred.probs > min.thresh)
true.class <- diabetes$diabetes
table(pred.class,true.class)
addmargins(table(pred.class,true.class))
######################################

library(AUC) 
n.cv <- 500 
n.test <- round(.1*nrow(diabetes))
cutoff <- min.thresh
#initialize matrices to hold CV results
sens <- rep(NA,n.cv)
spec <- rep(NA,n.cv)
ppv <- rep(NA,n.cv)
npv <- rep(NA,n.cv)
#begin for loop
for(cv in 1:n.cv) {
  obs.test <- sample(1:392,39)
  test.set <- diabetes[obs.test,]
  train.set <- diabetes[-obs.test,]
 train.model <- glm(diabetes~glucose+bmi+pedigree+age,data=train.set,family=binomial)
 pred.probs <- predict.glm(train.model,newdata=test.set,type="response")
 test.class <- ifelse(pred.probs>cutoff,1,0)
  #create a confusion matrix
  conf.mat <- addmargins(table(factor(test.set$diabetes,levels=c(0,1)),factor(test.class,levels=c(0,1))))
  # pull of sensitivity,specificity,ppv and npv
  #using bracket notation
  sens[cv] <- conf.mat[2,2]/conf.mat[2,3]
  spec[cv] <- conf.mat[1,1]/conf.mat[1,3]
  ppv[cv] <- conf.mat[2,2]/conf.mat[3,2]
  npv[cv] <- conf.mat[1,1]/conf.mat[1,3]

  auc[cv] <- auc(roc(test.set$diabetes,pred.probs))

  
}
mean(sens)
mean(spec)
mean(ppv)
mean(npv)

#prediction
patient <- data.frame(pregnant= 1, glucose= 90, diastolic=62, triceps= 18, 
                      insulin= 59, bmi= 25.1, pedigree= 1.268, age= 25)
pred.log.odds <- predict.glm(mod,newdata=patient)
pred.prob <-exp(pred.log.odds)/(1+exp(pred.log.odds))
pred.probb <- predict.glm(mod,newdata=patient,type="response")
pred.probb



