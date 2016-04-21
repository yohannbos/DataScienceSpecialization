#question 1
m<-1100
sd<-30
n<-9
int<- m + c(-1,1)*qt(0.975, df=n-1)*sd/sqrt(n)

#question 2
m<- -2
#sd<-?
n<-9
sd<- -m*sqrt(n)/qt(0.975, df=n-1)

#question 4
m<-3-5
v1<-0.60
v2<-0.68
n1<-10
n2<-10
sd<-sqrt(((n1-1)*v1+(n2-1)*v2)/(n1+n2-2))
int4<- m + c(-1,1)*qt(0.975, df=n1+n2-2)*sd*sqrt(1/n1+1/n2)

#question 6
m<-6-4
s1<-0.50
s2<-2
n1<-100
n2<-100
int4<- m + c(-1,1)*qnorm(0.975)*sqrt(s1^2/n1+s2^2/n2)

#question 7
m<--3-1
s1<-1.5
s2<-1.8
n1<-9
n2<-9
sd<-sqrt((s1^2+s2^2)/2)
int4<- m + c(-1,1)*qt(0.95, df=n1+n2-2)*sd*sqrt(1/n1+1/n2)
