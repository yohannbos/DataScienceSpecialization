# Exploratory data analysis: Course Project 1


library(data.table)

#download data file
d<-getwd()
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
if (!file.exists(d)){
  dir.create(d)
}
download.file(url,file.path(d, "dataset.zip"), mode="wb")

#unzip data file
f<-unzip("dataset.zip", exdir=d)
df<-data.frame()

#check the unziped file
lf<-list.files(d, recursive = TRUE, include.dirs = TRUE)

#read file
df<-read.table("household_power_consumption.txt", sep=";", header=TRUE)
df$Date<-as.Date(df$Date, format="%d/%m/%Y" )

#subset data table
df1<-df[df$Date=="2007-02-01"|df$Date=="2007-02-02",]

#convert variables
df1$Global_active_power<-as.numeric(as.character(df1$Global_active_power))
df1$Sub_metering_1<-as.numeric(as.character(df1$Sub_metering_1))
df1$Sub_metering_2<-as.numeric(as.character(df1$Sub_metering_2))
df1$Sub_metering_3<-as.numeric(as.character(df1$Sub_metering_3))
df1 <- transform(df1, Moment=as.POSIXct(paste(Date, Time)))

#plot 3
png( file="plot3.png", width=480, height=480)
plot(df1$Moment, df1$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
lines(df1$Moment, df1$Sub_metering_2, col="red")
lines(df1$Moment, df1$Sub_metering_3, col="blue")
legend("topright", legend =c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black","red","blue"),lty=1, cex=.75)

dev.off()
cat("file saved as png in", getwd())