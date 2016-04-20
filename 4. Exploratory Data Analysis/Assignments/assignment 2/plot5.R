#Exploratory Data Analysis: Assignment 2 - plot 5

#downloading and extracting data
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if (!file.exists("NEI")){dir.create("NEI")}
download.file(url, destfile=file.path("NEI","data.zip"), mode="wb")
unzip(zipfile =file.path("NEI","data.zip"), exdir = "NEI")
list.files("NEI", recursive= TRUE, include.dirs = TRUE)

NEI<-readRDS("NEI/summarySCC_PM25.rds")
Classification <- readRDS("NEI/Source_Classification_Code.rds")

#get accurate data
p<-"Motor"
Class.mv<-subset(Classification, grepl(p,Short.Name)&Data.Category=="Onroad", select= c(SCC:Short.Name))
l<-as.list(Class.mv[,"SCC"])
NEI.Balt<-subset(NEI,fips=="24510")
x<-merge(x=Class.mv, y=NEI.Balt, by="SCC")
Emi.Balt.mv<-aggregate(Emissions~year, data=x, FUN=sum, na.rm=TRUE)

#produce and save png plot 5
library(ggplot2)
png(filename="plot5.png", width=1200)
ggplot(data=Emi.Balt.mv, aes(x=year, y=Emissions))+geom_point(col="red")+geom_line()+ggtitle("Motor Vehicules Emissions in Baltimore")
dev.off()
cat("file saved as plot5.png in", getwd())