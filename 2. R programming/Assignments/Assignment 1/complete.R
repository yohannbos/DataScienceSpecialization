complete <- function(directory, id = 1:332) {
  #function that reads a directory full of files and reports the number
  #of completely observed cases in each data file. The function should
  #return a data frame where the first column is the name of the file and
  #the second column is the number of complete cases.
  setwd(directory)
  q<-data.frame(id=numeric(),nobs=numeric())

  for (i in id) {
    x <- read.csv(as.character(paste0(sprintf("%03d",i),".csv")))
    y <- x[complete.cases(x), ]
    count1<-nrow(y)
    z<-data.frame(id=i,nobs=count1)
    q<-rbind(q,z)
  }
  return(q)

  }
