#question 1 Fstat and :
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

fit<-lm(y~x)
plot(fit)
f.stat<-summary(fit)$fstatistic
pvalue<-1-pf(f.stat["value"], f.stat["numdf"],f.stat["dendf"])

#question 2:
n<-length(y)
summary(fit)$sigma #1st solution
sqrt(sum(resid(fit)^2) / (n - 2)) #2nd solution
plot(x, resid(fit))
abline(h=0, col="red")

#question 3
x <- mtcars$wt
y <- mtcars$mpg
fit2 <- lm(y ~ x)
coef <- summary(fit2)$coefficients
newdata <- data.frame(x=c(mean(x)))
p1 <- predict(fit2, newdata, interval = ("confidence"))
print(p1) #b1+ou-tstar*std(b1)

#question 5
newdata <- data.frame(x=3000/1000)
p2 <- predict(fit2, newdata, interval = ("prediction"))
print(p2)

#question 9
x <- mtcars$wt
y <- mtcars$mpg
fit2 <- lm(y ~ x)
ssx2<-sum(resid(fit2)^2)
ssx2/sum((y-mean(y))^2)