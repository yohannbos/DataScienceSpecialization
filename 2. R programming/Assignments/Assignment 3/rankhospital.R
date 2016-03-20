
rankhospital<-function(st, obj, rk){
  library (stringr)
  library(dplyr)
  
  #read csv file
  df<-read.csv("outcome-of-care-measures.csv", colClasses = "character")

  #argument verifications (st & obj)
  if (length(grep(st, df$State))==0) {stop("invalid state")}

  a=c("Heart Attack", "Heart Failure", "Pneumonia")
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  obj<-simpleCap(obj)
  if (length(grep(obj,a))==0){stop("invalid outcome")}
  
  #subsetting by state, ordering, clear NAs
  df.st<-df[df$State==st,]
  
  obj<-str_replace_all(obj, " ", ".")
  b<-paste0("Hospital.30.Day.Death..Mortality..Rates.from.", obj)
  df.st<-df.st[df.st[,b]!="Not Available",]
  df.st[,b]<-as.numeric(df.st[,b])
  y<-df.st[order(df.st[,b], df.st$Hospital.Name, na.last = NA),]
  
  #test on rk value
  if(rk=="worst") {
    rk<-length(y[,b])
  }
  else if (rk=="best") {
    rk<-1
  }
  else if (rk>length(df.st[,b])) {
    return (NA)
  }
  z<- (y$Hospital.Name[rk])

  return (z)
  }

#rankhospital("TX", "heart failure", 4)
#rankhospital("MD", "heart attack", "worst")
#rankhospital("MN", "heart attack", 5000)
