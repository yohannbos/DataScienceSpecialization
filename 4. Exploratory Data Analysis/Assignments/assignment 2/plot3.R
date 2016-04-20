#Exploratory Data Analysis: Assignment 2 - plot 3

#downloading and extracting data
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if (!file.exists("NEI")){dir.create("NEI")}
download.file(url, destfile=file.path("NEI","data.zip"), mode="wb")
unzip(zipfile =file.path("NEI","data.zip"), exdir = "NEI")
list.files("NEI", recursive= TRUE, include.dirs = TRUE)

NEI<-readRDS("NEI/summarySCC_PM25.rds")
SCC <- readRDS("NEI/Source_Classification_Code.rds")

#Getting accurate dataset
NEI.Balt<-subset(NEI,fips=="24510")
Emi.by.type.Balt<-aggregate(Emissions~year+type, data=NEI.Balt, FUN=sum, na.rm=TRUE)

#Produce and save png plot 3
library(ggplot2)
png(filename="plot3.png", width=1200)
ggplot(data=Emi.by.type.Balt, aes(x=year, y=Emissions))+geom_point(col="red")+facet_grid(.~type)+geom_line()+ggtitle("Emissions in Baltimore per type and year")
dev.off()
cat("file saved as plot3.png in", getwd())