getting-and-cleaning-data
=========================

Getting and Cleaning Data
Course Project
Tobias Crabtree  
June 2014

Objective to create a tidy data set with the average of each variable for each activity and each subject using the data collected from the accelerometers from the Samsung Galaxy S smartphone. Data was downloaded June 2014 from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.

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

## Label the data set with descriptive variable names and correct for invalid characters in the descriptive variable names such as "("
labels <- read.table("./UCI HAR Dataset/features.txt", quote="") 
labelsCorrect <- c(make.names(labels$V2), "activity", "subject")
names(mergeSet) <- labelsCorrect

## Extracts only the measurements of the mean and standard deviation for each measurement from the 561 variables
## Select mean measurements from the 561 variables
colMean <- grep("mean()", labels[,2]) 
## Select standard deviation measurements from the 561 variables
colSD <- grep("std()", labels[,2]) 

## create the extract set with mean and standard deviation measurements along with activity (column 562) and subject (column 563)
extractSet <- subset(mergeSet, select=c(colMean, colSD, 562, 563)) 

## Use descriptive activity names to name the activities in the data set. Six activities were provided in the data set.
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

## Apply the activity names to the extract set
extractSet$activity <- sapply(extractSet$activity, activityFunction)

## Create a second, independent tidy data set with the average of each variable for each activity and each subject. Using the reshape2 package melt the extract set, and then use dcast to generate the mean for each variable for each activity and each subject. Finally, we generate a tidy data set with 4 columns for the variables activity, subject, variable, and mean. Each row represents an observation, the result of one mean calculation for one activity on one subject, and each column is a variable.

tidyMelt <- melt(extractSet, id=c("activity","subject"))
tidyData <- dcast(tidyMelt, activity+subject~variable, mean)
tidyDataMelt <- melt(tidyData, id=c("activity","subject"))

## add the 4 variable names - activity, subject, variable, mean - to tidy data set
names(tidyDataMelt) <- c("activity","subject","variable","mean")

## Create text file with tidy data set. 
write.table(tidyDataMelt, file="tidyData.txt", quote=FALSE, row.names=FALSE)
