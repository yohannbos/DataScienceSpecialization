#Exploratory Data Analysis: Assignment 2 - plot 6

#downloading and extracting data
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if (!file.exists("NEI")){dir.create("NEI")}
download.file(url, destfile=file.path("NEI","data.zip"), mode="wb")
unzip(zipfile =file.path("NEI","data.zip"), exdir = "NEI")
list.files("NEI", recursive= TRUE, include.dirs = TRUE)

NEI<-readRDS("NEI/summarySCC_PM25.rds")
Classification <- readRDS("NEI/Source_Classification_Code.rds")

#Manipulate Data

#select Classification codes corresponding to "Motor Vehicules"
p<-"Motor"
Class.coal<-subset(Classification, grepl(p,Short.Name)&Data.Category=="Onroad", select= c(SCC:Short.Name))

#select only data of Baltimore and LA
NEI.Balt.LA<-subset(NEI,fips=="24510"|fips=="06037")
NEI.Balt.LA$fips<-as.factor(NEI.Balt.LA$fips)
levels(NEI.Balt.LA$fips)<-c("LA", "Baltimore")

#get accurate data set
x<-merge(Class.coal, y=NEI.Balt.LA, by="SCC")
Emi.coal.Bt.LA<-aggregate(Emissions~year+fips, data=x, FUN=sum, na.rm=TRUE)

#produce and save png plot 6
library(ggplot2)
png(filename="plot6.png", width=1200)
ggplot(data=Emi.coal.Bt.LA, aes(x=year, y=Emissions))+geom_point(col="red")+geom_line()+facet_grid(. ~ fips)+ggtitle("Motor Vehicules Emissions in Baltimore and LA")
+
dev.off()
cat("file saved as plot6.png in", getwd())