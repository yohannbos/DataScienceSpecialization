
rankall<-function(obj, rk){
  library (stringr)
  library(plyr)
  library(dplyr)


  #read csv file
  df<-read.csv("outcome-of-care-measures.csv", colClasses = "character")

  # obj argument verification
  a=c("Heart Attack", "Heart Failure", "Pneumonia")
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  obj<-simpleCap(obj)
  if (length(grep(obj,a))==0){stop("invalid outcome")}

  #finding and selecting obj column
  obj<-str_replace_all(obj, " ", ".")
  b<-paste0("Hospital.30.Day.Death..Mortality..Rates.from.", obj)

  #remove NAs from obj column
  df<-df[df[,b]!="Not Available",]
  df[,b]<-as.numeric(df[,b])

  allstates = sort(unique(df[,"State"]))
  hospital<-character(0)
  for (i in seq_along(allstates)){

    dft<-df[df$State==allstates[i],]
    dft<-dft[order(as.numeric(dft[,b]), dft$Hospital.Name, decreasing = FALSE, na.last = NA ),]

    this.num = rk
    if (this.num=="best") this.num = 1
    if (this.num=='worst') this.num = nrow(dft)
    hospital[i]<-dft[this.num,"Hospital.Name"]
  }
  data.frame(hospital=hospital,state=allstates, row.names=allstates)
}


#head(rankall("heart attack", 20), 10)
