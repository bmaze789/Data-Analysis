rate<-read.table(file="RateMyProfessor.txt",header=TRUE)
pairs(~numYears+numRaters+numCourses+easiness+raterInterest+quality,rate)

library(car)
mlr<-lm(rate$quality~.,rate)
vif(mlr)

rate<- rate[,c(1:7,9:ncol(rate),8)]


library(bestglm)
vs.ref<- bestglm(rate, IC="AIC", method = "exhaustive")
vs.ref$BestModel
plot(vs.ref$Subsets$AIC, type = "b",pch=19,xlab = "# of Vars",ylab="AIC")


confint(vs.ref$BestModel, level = .95) 
avPlots(vs.ref$BestModel)


resid <- vs.ref$BestModel$residuals
fitted <- vs.ref$BestModel$fitted.values
plot(fitted, resid)+abline(h = 0, lty = 2) 

library(lmtest)
bptest(vs.ref$BestModel)   #equal var is met
library(MASS)
std.res<-stdres(vs.ref$BestModel)
ggplot()+geom_histogram(aes(x=std.res))    #graphs histo
ks.test(std.res,"pnorm")  #test for normality

summary(vs.ref$BestModel)

pred.df <-data.frame(numYears=5,numRaters=54, numCourses=2,
                     pepper="no", disciplineSTEM="Yes", easiness=2.5, raterInterest=4.2,
                     deptMath="Yes",deptBusiness="No",deptEnglish="No",
                     deptGeology="No", deptLanguages="No", deptPhysics="No",
                     deptPolySci="No",deptPsychology="No")


pred<-predict.lm(vs.ref$BestModel, newdata = pred.df, interval = "prediction",level = .95)


