life<-read.table(file="LifeExp.txt",header=TRUE)  #read in the data

life$Group<-factor(life$Group, levels = c("other","africa","oecd"))
boxplot(life$LifeExp~life$Group) 
library(ggplot2)

ggplot(life,aes(y=life$LifeExp,x=log(life$PPGDP),shape=Group, color=Group))+geom_point()+xlab("Log(PPGDP)")+ylab("Life Expectancy")   

mlr<-lm(life$LifeExp~log(life$PPGDP)*life$Group)
coef(mlr)

resid <- mlr$residuals
fitted <- mlr$fitted.values
plot(fitted, resid)+abline(h = 0, lty = 2)   #test for linearity & equal variance

library(lmtest)
bptest(mlr)   #equal var is met
library(MASS)
std.res<-stdres(mlr)
ggplot()+geom_histogram(aes(x=std.res))    #graphs histo
ks.test(std.res,"pnorm")  #test for normality

#####################################################
summary(mlr)  #f-statistic for overall regression
#f-test
full.lm<-lm(life$LifeExp~life$Group*log(life$PPGDP))
reduced.lm<-lm(life$LifeExp~life$Group+log(life$PPGDP))
av<-anova(full.lm,reduced.lm) 
summary(av)
confint(mlr)    #confid interval

#fitted plot
ggplot(life,aes(y=life$LifeExp,x=log(life$PPGDP),color=Group))+geom_point()+ geom_smooth(method="lm",se=FALSE)+xlab("Log(PPGDP)")+ylab("Life Expectancy")
