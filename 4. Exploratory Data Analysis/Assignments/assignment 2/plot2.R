#Exploratory Data Analysis: Assignment 2 - plot 2

#downloading and extracting data
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if (!file.exists("NEI")){dir.create("NEI")}
download.file(url, destfile=file.path("NEI","data.zip"), mode="wb")
unzip(zipfile =file.path("NEI","data.zip"), exdir = "NEI")
list.files("NEI", recursive= TRUE, include.dirs = TRUE)

NEI<-readRDS("NEI/summarySCC_PM25.rds")
SCC <- readRDS("NEI/Source_Classification_Code.rds")

#Get accurate data set
NEI.Balt<-subset(NEI,fips=="24510")
total.emi.Balt<-aggregate(x=list(Emissions=NEI.Balt$Emissions),by=list(Year=NEI.Balt$year), FUN=sum, na.rm=TRUE)

#Produce and save png plot 2
png(filename="plot2.png")
with(total.emi.Balt, plot(x=Year, y=Emissions, col="red", type="b", main = "Total PM[2.5] emissions for all sources per year in Baltimore"))
dev.off()
cat("file saved as plot2.png in", getwd())