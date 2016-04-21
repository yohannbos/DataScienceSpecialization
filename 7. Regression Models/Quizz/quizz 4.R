#QUizz 4
#Question 1
library(MASS)
data(shuttle)
shuttle2<-shuttle
shuttle2$use2<- as.numeric(shuttle2$use=='auto')
fit<-glm(use2~factor(wind) - 1, data=shuttle2, family = "binomial")
summary(fit)
exp(coef(fit))
exp(coef(fit)[1])/exp(coef(fit)[2])

#Question 2
fit2<-glm(use2 ~ factor(wind) + factor(magn) - 1, family = binomial, data = shuttle2)
summary(fit2)$coef
exp(coef(fit2))
exp(coef(fit2)[1])/exp(coef(fit2)[2])

#question3
fit<-glm(use2 ~ factor(wind), family = binomial, data = shuttle2)
summary(fit)$coef
fit<-glm(1-use2 ~ factor(wind), family = binomial, data = shuttle2)
summary(fit)$coef

#Question4
data("InsectSprays")
fit3<-glm(count~factor(spray)-1, data=InsectSprays, family = poisson)
summary(fit3)$coef
exp(coef(fit3))
exp(coef(fit3)[1])/exp(coef(fit3)[2])

#Question5
fit4<-glm(count ~ factor(spray), family = poisson,data=InsectSprays,offset = log(count + 1))
summary(fit4)$coef
fit5<-glm(count ~ factor(spray), family = poisson,data=InsectSprays,offset = log(10)+log(count+1))
summary(fit5)$coef

#Question6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

knots<-c(0)
splineTerms<-sapply(knots,function(knot) (x>knot)*(x-knot))
xmat<-cbind(1,x,splineTerms)
fit6<-lm(y~xmat-1)
yhat<-predict(fit6)
(yhat[10]-yhat[6])/4
plot(x,y)
lines(x,yhat,col="red")
