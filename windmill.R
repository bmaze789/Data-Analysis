wind <- read.table(file="Windmill.txt",header=TRUE)
CSpd<-wind[,1]
RSpd<-wind[,2]

ggplot(wind,aes(RSpd,CSpd))+geom_point()
ggplot(wind,aes(RSpd,CSpd))+geom_point()+geom_smooth(se=FALSE, method = "lm")
cor(RSpd,CSpd)


slr <- lm(CSpd~RSpd, wind)
summary(slr)
coef(slr)
predDF<- data.frame(RSpd=c(12,30)) 
predict.lm(slr,predDF)
