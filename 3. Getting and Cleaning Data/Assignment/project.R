#Courseproject

library(data.table)
library(readr)
library(plyr)
library(reshape2)
library(gdata)
library(knitr)

#download dataset

d<-setwd("D:/Documents/Data Science/DS_spe_JH/3. Getting and Cleaning Data/Project/")
url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
if (!file.exists(d)) {
  dir.create(d)
}
download.file(url, file.path(d, "dataset.zip"), mode="wb")

#unzip dataset
unzip("dataset.zip", exdir=d)

#check the unziped folder
pathUCI <- file.path(d, "UCI HAR Dataset")
list.files(pathUCI, recursive = TRUE, include.dirs = TRUE)

#read subject (subjects ID), activity (activity type) and data files
subjecttest<-fread(file.path(pathUCI,"test", "subject_test.txt"))
subjecttrain<-fread(file.path(pathUCI,"train", "subject_train.txt"))

activitytest<-fread(file.path(pathUCI,"test", "y_test.txt"))
activitytrain<-fread(file.path(pathUCI,"train", "y_train.txt"))



dttest<-read_fwf(file.path(pathUCI,"test", "X_test.txt"), fwf_empty(file.path(pathUCI,"test", "X_test.txt")))
dttrain<-read_fwf(file.path(pathUCI,"train", "X_train.txt"),fwf_empty(file.path(pathUCI,"train", "X_train.txt")))

#Merge respective Test and Train files (by raws)

allsubjects<-rbind(subjecttest, subjecttrain)
setnames(allsubjects, "V1", "Subject_Id")

allactivities<-rbind(activitytest, activitytrain)
setnames(allactivities, "V1", "Activity_Type")

dt<-rbind(dttest, dttrain)
#(dt columns names are described in the features.txt file)
dtFeatures <- fread(file.path(pathUCI, "features.txt"))
dtcolnames<- dtFeatures[[2]]
colnames(dt)<-dtcolnames

#Merge subject, activities and data tables (by columns) 
mergeddt<-cbind(allsubjects, cbind(allactivities, dt))
setkeyv(mergeddt,c("Subject_Id","Activity_Type"))

#extract means and standard deviations columns from mergeddt (exclude meanFreq())
selectmean<-matchcols(mergeddt, with="mean()", without="Freq")
selectstd<- matchcols(mergeddt, with="std()")
extractdt<-subset(mergeddt,select=c("Subject_Id","Activity_Type",selectmean,selectstd))

#replace activity numbers by respective activity names found in activity_labels.txt
dtactivity <- fread(file.path(pathUCI, "activity_labels.txt"))
extractdt$Activity_Type<-as.factor(extractdt$Activity_Type)
levels(extractdt$Activity_Type)<-dtactivity[[2]]

#create new data table with average of each variable for each combination of subject and activity
averagedt<-melt(extractdt, id.var=c("Subject_Id", "Activity_Type")) 
averagedt2<-dcast(averagedt, Subject_Id + Activity_Type ~ variable,fun=mean)

#save as text file
tidydata<-write.table(averagedt2, file="D:/Documents/Data Science/DS_spe_JH/3. Getting and Cleaning Data/Project/tidydata.txt", row.name=FALSE)
