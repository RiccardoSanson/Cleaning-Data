# Peer-graded Assignment: Getting and Cleaning Data Course Project
# = = = = = = = = = = = =

# packages needed
library(data.table)

# directory with the train and test data
# change if needed
dir =  "UCI HAR Dataset/"

# load datasets
features <- read.table(paste(dir,"features.txt",sep=""), 
	header= FALSE, stringsAsFactors = FALSE) 
activity <- read.table(paste(dir,"activity_labels.txt",sep=""),
	header= FALSE, stringsAsFactors = FALSE)
train_X <-  read.table(paste(dir,"train/X_train.txt",sep=""))
trainLabels <- read.table(paste(dir,"train/y_train.txt",sep=""))
trainSubject <- read.table(paste(dir,"train/subject_train.txt",sep=""))
test_X <-  read.table(paste(dir,"test/X_test.txt",sep=""))
testLabels <- read.table(paste(dir,"test/y_test.txt",sep=""))
testSubject <- read.table(paste(dir,"test/subject_test.txt",sep=""))

# - - - -
# 1
# Merges the training and the test sets to create one data set.

# put columns together
train <- cbind(trainSubject, train_X, trainLabels)
test <- cbind(testSubject, test_X, testLabels)

# merge train and test
all <- rbind(test, train)

# clean columns
colnames(all) <- c("subject", features$V2, "activity")
allSubject <- as.factor(all$subject)
all <- all[, !duplicated(colnames(all))]

# - - -
# 2
# Extracts only the measurements on the mean and 
# standard deviation for each measurement. 

meancols = grepl("mean",colnames(all),fixed=TRUE)
sdcols = grepl("sd",colnames(all),fixed=TRUE)

# this file has only mean and sd columns
all_ex = all[meancols | sdcols]

# - - -
# 3
# Uses descriptive activity names to name the activities in the data set

# get names from activity file
all$activity = activity$V2[all$activity]

# - - -
# 4
# Appropriately labels the data set with descriptive variable names. 

# maybe these names are better
names(all) <- gsub("^t", "time", names(all))
names(all) <- gsub("^f", "frequency", names(all))
names(all) <- gsub("^Acc", "acceleration", names(all))
names(all) <- gsub("^Mag", "magnitude", names(all))
names(all) <- gsub("^Gyro", "gyroscope", names(all))


# - - -
# 5
# From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# we use the aggregate function
all_mean = copy(all); all_mean = as.data.table(all_mean)
all_mean = aggregate(all_mean[,!c("subject","activity")],
	list(all_mean[,subject],all_mean[,activity]),mean)

# finally we export the tidy data
write.table(all_mean, "tidydata.txt", row.names = FALSE, quote = FALSE)


