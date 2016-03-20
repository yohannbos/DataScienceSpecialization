corr <- function(directory, threshold = 0) {
  #function that takes a directory of data files and a threshold for
  #complete cases and calculates the correlation between sulfate and nitrate
  #for monitor locations where the number of completely observed cases
  #(on all variables) is greater than the threshold. The function should
  #return a vector of correlations for the monitors that meet the threshold
  #requirement
  setwd(directory)
  z<-numeric()
  for (i in 1:332){
    x <- read.csv(as.character(paste0(directory,sprintf("/%03d",i),".csv")))
    y <- x[complete.cases(x), ]
    count1<-nrow(y)
    if (count1>=threshold){
      z<- c(z,cor(y$sulfate,y$nitrate))
    }
  }
  return (z)
  }
