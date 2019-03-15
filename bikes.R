bikes <- read.csv("Bikes.csv", header=TRUE)
bikes$yr<-factor(bikes$yr)
hist(bikes$cnt,xlab="Bikes Rented")

library(bestglm)
best.bike <- bestglm(bikes,family=poisson,method="exhaustive",IC="AIC")
mod <- best.bike$BestModel


####################################################################

scatter.smooth(bikes$windspeed,log(bikes$cnt+1),ylab="log(cnt)+1",xlab="windspeed")
scatter.smooth(bikes$hum,log(bikes$cnt+1),ylab="log(cnt)+1",xlab="hum")
scatter.smooth(bikes$temp,log(bikes$cnt+1),ylab="log(cnt)+1",xlab="temp")

#scatter.smooth(bikes$yr,log(bikes$cnt+1),ylab="log(cnt)+1",xlab="year")

int <- confint(mod,level=.95)
trans <- (exp(int)-1)

library(xtable)
newobject<-xtable(trans)



str(bikes)
day <- data.frame(season= "Spring", yr="2012", holiday="No", workingday="Yes",
                  weathersit="Misty", temp=0.34, hum=0.80, windspeed=0.18)
l.mean <- predict.glm(mod,newdata=day,se.fit=TRUE)
l.mean
exp(l.mean$fit)
lower <- l.mean$fit - qnorm(.975)*l.mean$se.fit
upper<- l.mean$fit + qnorm(.975)*l.mean$se.fit

interval <- c(exp(lower),exp(upper))
interval
