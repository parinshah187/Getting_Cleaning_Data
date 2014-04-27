activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header=FALSE)
features <- read.table("./UCI HAR Dataset/features.txt", header=FALSE)
colnames(activity_labels) <- c("Activity_ID", "Activity")
colnames(features) <- c("Feature_ID","Feature")

# 1. Merges the training and the test sets to create one data set.

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
colnames(X_train) <- features[,2]
Y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", sep="", header=FALSE)
colnames(Y_train) <- c("Activity_ID")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
colnames(subject_train) <- c("Subject_ID")

trainingData <- cbind(X_train, Y_train, subject_train)

X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
colnames(X_test) <- features[,2]
Y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", sep="", header=FALSE)
colnames(Y_test) <- c("Activity_ID")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
colnames(subject_test) <- c("Subject_ID")

testingData <- cbind(X_test, Y_test, subject_test)
finalData <- rbind(trainingData, testingData)
#------------------------------------------------------

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

colNames <- colnames(finalData)
meanLogical <- grepl("mean()", colNames) & !grepl("meanFreq()", colNames)
stdLogical <- grepl("std()", colNames)
idLogical <- grepl("Activity..", colNames) | grepl("Subject..", colNames)

finalData <- finalData[,colNames[meanLogical | stdLogical | idLogical]]
#------------------------------------------------------

# 3. Uses descriptive activity names to name the activities in the data set

finalData <- merge(finalData,activity_labels,by="Activity_ID",all.x=TRUE);
colNames <- colnames(finalData)
#------------------------------------------------------

# 4. Appropriately labels the data set with descriptive activity names.

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StandardDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","Time",colNames[i])
  colNames[i] = gsub("^(f)","Freq",colNames[i])
  colNames[i] = gsub("Mag","Magnitude",colNames[i])
};
colNames <- colnames(finalData)

#------------------------------------------------------

#6. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(reshape2)
finalData <- melt(finalData,id=c("Activity", "Subject_ID"))
finalData <- dcast(mymelt, Activity + Subject_ID ~ variable,mean)
write.table(finalData, "./tidyData.txt" ,sep=" ");

#------------------------------------------------------


