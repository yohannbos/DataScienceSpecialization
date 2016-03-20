
best<-function(st, obj){
  library (stringr)
  library(dplyr)
  
  df<-read.csv("outcome-of-care-measures.csv", colClasses = "character")

  if (length(grep(st, df$State))==0) {stop("invalid state")}

  a=c("Heart Attack", "Heart Failure", "Pneumonia")
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  obj<-simpleCap(obj)
  if (length(grep(obj,a))==0){stop("invalid outcome")}
  
  df.st<-df[df$State==st,]
  
  
  obj<-str_replace_all(obj, " ", ".")
  b<-paste0("Hospital.30.Day.Death..Mortality..Rates.from.", obj)
  df.st<-df.st[df.st[,b]!="Not Available",]
  df.st[,b]<-as.numeric(df.st[,b])
  
  #x<- df.st[which(df.st[,b] == min(df.st[,b])), ]
  y<-df.st[order(df.st[,b], df.st$Hospital.Name, na.last = NA),]
  z<- (y$Hospital.Name[1])
  #z<-with(y, y$Hospital.Name[y[,b]])
  
  return (z)
}


#best("TX", "heart attack")
#best("TX", "heart failure")
#best("MD", "heart attack")
#best("MD", "pneumonia")
#best("BB", "heart attack")
#best("NY", "hert attack")