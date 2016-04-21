library(UsingR);
data(galton);
library(reshape);
long<- melt(galton)
g<-ggplot(long, aes(x=value, fill=variable))
g<-g +geom_histogram(colour= "black", binwidth=1)
g<-g+facet_grid(.~variable)
g

lm(I(child-mean(child))~I(parent-mean(parent))-1,data=galton)

#question 1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
y<-x*w
sum(y)/7

#question 2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

lm(y~x-1)

#question 3
data(mtcars)
lm(mpg~wt,mtcars)

#question 4
sdrate<-1/0.5
corr<-sdrate*0.5
corr
#question 5
score<-1.5*0.4
score

#question 6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x[1]-mean(x))/sd(x)

#question 7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y~x)
library(ggplot2)
df<-data.frame(x,y)
g<-ggplot(df, aes(x=x,y=y))
g<-g+geom_point(size=7,colour='black', alpha=.2)
g<-g+geom_point(size=4, colour='red', alpha=.7)
g<-g+geom_smooth(method= 'lm', colour='green')
g<-g+
#question 8

#question 9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)