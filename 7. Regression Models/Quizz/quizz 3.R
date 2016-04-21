#quiz 3

#question 1
data(mtcars)
str(mtcars)
mtcars$cyl<-factor(mtcars$cyl)
fit_adjusted<-lm(mpg~cyl+wt, data=mtcars)
summary(fit_adjusted)$coef[3,1]

#question 2
fit_unadjusted<-lm(mpg~cyl, data=mtcars)
anova(fit_unadjusted, fit_adjusted)
# --> Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded.

#question 3
#method 1
fit_adjusted<-lm(mpg~cyl+wt, data=mtcars)
fit_adjusted_int<-lm(mpg~cyl:wt, data=mtcars)
anova(fit_adjusted, fit_adjusted_int)$Pr

#method 2
suppressMessages(library(lmtest))
fit_non_interaction <- lm(mpg ~ cyl + wt, mtcars)
summary(fit_non_interaction)$adj.r.squared
fit_interaction <- lm(mpg ~ cyl + wt + cyl:wt, mtcars)
summary(fit_interaction)$adj.r.squared
lrtest(fit_interaction, fit_non_interaction)
# --> The P-value is larger than 0.05. So, according to our criterion, we would fail to reject, which suggests that the interaction terms may not be necessary.

#question 4
#---> The estimated expected change in MPG per half ton increase in weight for for a specific number of cylinders (4, 6, 8).

#question 5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
max(hatvalues(fit))


#question 6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
dfbetas(fit)

#question 7
#It is possible for the coefficient to reverse sign after adjustment. For example, it can be strongly significant and positive before adjustment and strongly significant and negative after adjustment.

