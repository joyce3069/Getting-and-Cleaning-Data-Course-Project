# Step 0: Downloading and Unzipping data set

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/assdata.zip")
unzip(zipfile="./data/assdata.zip",exdir="./data")

# Step 1:Merges the training and the test sets to create one data set.
#1.1 Reading Data and Assigning column names:

features <- read.table("./data/UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("./data/UCI HAR Dataset/activity_lables.txt", col.names = c("activityID","activity"))
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names = c("subject"))
x_test <- read.table("./data/UCI HAR Dataset/test/x_test.txt", col.names = c("features$function"))
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names = c("activityID"))
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", col.names = c("subject"))
x_train <- read.table("./data/UCI HAR Dataset/train/x_train.txt", col.names = c("features$function"))
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt", col.names = c("activityID"))

#1.2 Merging all data in one set:

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

# Step 2:Extracts only the measurements on the mean and standard deviation for each measurement. 

TidyData <- Merged_Data %>% select(subject, activityID, contains("mean"), contains("std"))

# Step 3:Uses descriptive activity names to name the activities in the data set

TidyData$activityID <- activities[TidyData$activityID, 2]

# Step 4: Appropriately labels the data set with descriptive variable names.

names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))

# Step 5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

SecData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(SecData, "SecData.txt", row.name=FALSE)