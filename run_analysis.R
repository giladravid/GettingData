setwd("~/Desktop/coursera")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
               destfile="a.zip",method="curl")
unzip("a.zip")

# 1. Merges the training and the test sets to create one data set.

# Read in the data from files
features<-read.table('UCI HAR Dataset/features.txt',header=FALSE); 
activityType<-read.table('UCI HAR Dataset/activity_labels.txt',header=FALSE); 
subjectTrain<-read.table('UCI HAR Dataset/train/subject_train.txt',header=FALSE); 
xTrain<-read.table('UCI HAR Dataset/train/X_train.txt',header=FALSE); 
yTrain<-read.table('UCI HAR Dataset/train/y_train.txt',header=FALSE);
subjectTest<-read.table('UCI HAR Dataset/test/subject_test.txt',header=FALSE);
xTest<-read.table('UCI HAR Dataset/test/X_test.txt',header=FALSE); 
yTest<-read.table('UCI HAR Dataset/test/y_test.txt',header=FALSE);

# Assigin column names 
colnames(activityType)<-c('activityId','activityType');
colnames(subjectTrain)<-"subjectId";
colnames(xTrain)<-features[,2]; 
colnames(yTrain)<-"activityId";
colnames(subjectTest)<-"subjectId";
colnames(xTest)<-features[,2]; 
colnames(yTest)<-"activityId";

# Create the final sets
trainingData<-cbind(yTrain,subjectTrain,xTrain);
testData<-cbind(yTest,subjectTest,xTest);

# Combine training and test data to create a final data set
finalData<-rbind(trainingData,testData);
colNames<-colnames(finalData); 

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
Q2Data<-finalData[,c(1:2,grep('mean..-',colNames),grep("std..-",colNames))];

#3. Uses descriptive activity names to name the activities in the data set
Q3Data = merge(Q2Data,activityType,by='activityId',all.x=TRUE);
colNames<-colnames(Q3Data); 

#4. Appropriately labels the data set with descriptive variable names. 
colNames<-gsub("\\()","",colNames)
colNames<-gsub("-std","StdDev",colNames)
colNames<-gsub("-mean","Mean",colNames)
colNames<-gsub("-X","X",colNames)
colNames<-gsub("-Y","Y",colNames)
colNames<-gsub("-Z","Z",colNames)
colNames<-gsub("^t","time",colNames)
colNames<-gsub("^f","frequency",colNames)

colnames(Q3Data)<- colNames;

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
#    variable for each activity and each subject.

Q5Data  = Q3Data[,names(Q3Data) != 'activityType'];
tidyData    = aggregate(Q5Data[,-(1:2)],by=list(activityId=Q5Data$activityId,
                                                subjectId=Q5Data$subjectId),mean);
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');