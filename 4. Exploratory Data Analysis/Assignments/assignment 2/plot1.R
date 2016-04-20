#Exploratory Data Analysis: Assignment 2 - plot 1

#downloading and extracting data
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if (!file.exists("NEI")){dir.create("NEI")}
download.file(url, destfile=file.path("NEI","data.zip"), mode="wb")
unzip(zipfile =file.path("NEI","data.zip"), exdir = "NEI")
list.files("NEI", recursive= TRUE, include.dirs = TRUE)

NEI<-readRDS("NEI/summarySCC_PM25.rds")
SCC <- readRDS("NEI/Source_Classification_Code.rds")

#get accurate data set
total.emi<-aggregate(x=list(Emissions=NEI$Emissions), by=list(Year=NEI$year), FUN=sum, na.rm=TRUE)

# produce and save png plot 1
png(filename="plot1.png")
with(total.emi, plot(x=Year, y=Emissions, col="red", type="b", main = "Total PM[2.5] emissions for all sources per year"))
dev.off()
cat("file saved as plot1.png in", getwd())