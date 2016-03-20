pollutantmean <- function(directory, pollutant, id = 1:332) {
  #This function set a new directory, create an empty dataframe, add the csv files to it
  #and calculate the mean of the given pollutant
  setwd(directory)
  dat <- data.frame()
  for (i in id) {
    x <- read.csv(as.character(paste0(sprintf("%03d",i),".csv")),header = TRUE)
    dat<-rbind(dat, x)
  }
  mean(dat[,pollutant], na.rm = TRUE)
}

