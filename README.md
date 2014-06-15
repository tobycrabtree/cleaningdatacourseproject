getting-and-cleaning-data
=========================

Getting and Cleaning Data

## Getting and Cleaning Data Course Project
## Tobias Crabtree  June 2014

## use reshape2 package
library(reshape2) 

## read the training set for X, Y, and subject
trainSetX <- read.table("./UCI HAR Dataset/train/X_train.txt", quote="") 
trainSetY <- read.table("./UCI HAR Dataset/train/Y_train.txt", col.names="activity",quote="") 
trainSetSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names="subject", quote="") 

## merge the training sets
trainSet <- cbind(trainSetX, trainSetY, trainSetSubject) 

## read the test set for X, Y, and subject
testSetX <- read.table("./UCI HAR Dataset/test/X_test.txt", quote="")
testSetY <- read.table("./UCI HAR Dataset/test/Y_test.txt", col.names="activity", quote="")
testSetSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names="subject", quote="")
## merge the test sets
testSet <- cbind(testSetX, testSetY, testSetSubject) 

## Merge the training and test set
mergeSet <- rbind(trainSet, testSet)

## Label the data set with descriptive variable names
labels <- read.table("./UCI HAR Dataset/features.txt", quote="") 
labelsCorrect <- c(make.names(labels$V2), "activity", "subject")
names(mergeSet) <- labelsCorrect

## Extracts only the measurements of the mean and standard deviation for each measurement
colMean <- grep("mean()", labels[,2]) ## Select mean measurements
colSD <- grep("std()", labels[,2]) ## Select standard deviation measurements

## create extract set with activity and subject
extractSet <- subset(mergeSet, select=c(colMean, colSD, 562, 563)) 

## Use descriptive activity names to name the activities in the data set
activityFunction <- function (x) {
        if(x==1)
                return("walking")
        if(x==2)
                return("walking_upstairs")
        if(x==3)
                return("walking_downstairs")
        if(x==4)
                return("sitting")
        if(x==5)
                return("standing")
        if(x==6)
                return("laying")
        else
                return(NA)
}

extractSet$activity <- sapply(extractSet$activity, activityFunction)

## Create a second, independent tidy data set with the average of each variable for each activity and each subject
tidyMelt <- melt(extractSet, id=c("activity","subject"))
tidyData <- dcast(tidyMelt, activity+subject~variable, mean)
tidyDataMelt <- melt(tidyData, id=c("activity","subject"))
names(tidyDataMelt) <- c("activity","subject","variable","mean")

## create text file with tidy data set
write.table(tidyDataMelt, file="tidyData.txt", quote=FALSE, row.names=FALSE)
