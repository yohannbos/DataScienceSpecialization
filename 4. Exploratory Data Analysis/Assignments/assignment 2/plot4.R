#Exploratory Data Analysis: Assignment 2 - plot 4

#downloading and extracting data
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if (!file.exists("NEI")){dir.create("NEI")}
download.file(url, destfile=file.path("NEI","data.zip"), mode="wb")
unzip(zipfile =file.path("NEI","data.zip"), exdir = "NEI")
list.files("NEI", recursive= TRUE, include.dirs = TRUE)

NEI<-readRDS("NEI/summarySCC_PM25.rds")
Classification <- readRDS("NEI/Source_Classification_Code.rds")

#Get accurate data set
Class.coal<-subset(Classification, grepl("- Coal", EI.Sector), select= c(SCC,EI.Sector))
l<-as.list(Class.mv[,"SCC"])
x<-merge(x=Class.coal, y=NEI, by="SCC")
Emi.coal<-aggregate(Emissions~year, data=x, FUN=sum, na.rm=TRUE)

#Produce ad Save plot 4
library(ggplot2)
png(filename="plot4.png", width=1200)
ggplot(data=Emi.coal, aes(x=year, y=Emissions))+geom_point(col="red")+geom_line()+ggtitle("Coal Combustion Emissions in the USA")
dev.off()
cat("file saved as plot4.png in", getwd())