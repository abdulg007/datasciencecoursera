##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
#        runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
#      https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
# 1. Merge the training and test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################

# Clean up workspace
rm(list=ls())

# Load required packages
library(data.table)


# Files have been downloaded and unzipped at the following working directory
filePath <- "C:/-AG/-Schools/-Coursera/DataScientist/RFiles/DataCleanup/CourseProject/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset"

# Set the working directory to filePath
setwd(filePath);


# >>> 1. Merge the training and the test sets to create one data set.  <<<

# From the downloaded files located at filePath, read feature and activity info
features = read.table('./features.txt',header=FALSE)
actType  = read.table('./activity_labels.txt',header=FALSE, col.names=c('activityId','actType'))


# Read training data and label columns as needed
trainingSubject = read.table('./train/subject_train.txt',header=FALSE, col.names="subjectId")
xTrainingData   = read.table('./train/x_train.txt',header=FALSE, col.names=features[,2])
trainingActID   = read.table('./train/y_train.txt',header=FALSE, col.names="activityId")

# Merge columns (cbind) to build "training" data by merging trainingActID, trainingSubject, and xTrainingData
trainingData = cbind(trainingActID,trainingSubject,xTrainingData);


# Read test data and label columns as needed
testSubject = read.table('./test/subject_test.txt',header=FALSE, col.names="subjectId")
xTestData   = read.table('./test/x_test.txt',header=FALSE, col.names=features[,2])
testActID   = read.table('./test/y_test.txt',header=FALSE, col.names="activityId")

# Merge columns (cbind) to build "test" data by merging testActID, testSubject, and xTestData
testData = cbind(testActID,testSubject,xTestData);


# Create new data set by row-combining trainingData and testData  
finalData = rbind(trainingData,testData);

# Display finalData to review
# str(finalData)


# >>> 2. Extract measurements mean and standard deviation <<<<  

# Extract the column names from finalData to select desired columns like mean(), std(), etc.
colNames  = colnames(finalData); 


# Create a logicalVector that contains TRUE values for activity, subject, mean & stddev() columns
desiredCol = (grepl("activity..",colNames) | grepl("subject..",colNames) | 
              grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & 
              !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[desiredCol==TRUE];


# >>> 3. Use descriptive activity names to name the activities in the data set <<<

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData, actType, by='activityId', all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 

# >>>  4. Appropriately label the data set with descriptive activity names. <<<

# Renamig the variable names to more human readable format
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# >>> 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. <<<

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,actType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');








