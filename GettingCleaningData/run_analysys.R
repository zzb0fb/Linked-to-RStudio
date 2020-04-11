#################### Getting & Cleaning Data - Project
# 
# As starting point I will create a directory ("data") where load the .zip file
if (!file.exists('data')){
  dir.create('data')
}

# And as next step I will download the information keeping record on when was downloaded
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL,destfile = 'data/data_project.zip')
unzip('data/data_project.zip',exdir = 'data/')
date_of_download <- date()

#
##### 1. Merges the training and the test sets to create one data set.
#

# Firstly I will set the working directory where I have my data set
#
# I start reading provided files files
# './features.txt': List of all features.
# './activity_labels.txt': Links the class labels with their activity name.
# 'test/X_test.txt': Test set.
# 'test/y_test.txt': Test labels.
# 'subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 
#  to 30.
# 'subject_test.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 
#  to 30.

features <- read.table('./features.txt',
                       col.names = c('Feature-Id','Feature Name'))
activity_labels <- read.table('./activity_labels.txt',
                              col.names = c('Activity-Id','Activity Name'))
# Reading test information
X_test<-read.table('./test/X_test.txt',
           col.names = features$Feature.Name,check.names = FALSE)
y_test<-read.table('./test/y_test.txt')
subject_test<-read.table('./test/subject_test.txt')

# Reading train information
X_train<-read.table('./train/X_train.txt',
                   col.names = features$Feature.Name,check.names = FALSE)
y_train<-read.table('./train/y_train.txt')
subject_train<-read.table('./train/subject_train.txt')

# Once test and train data area loaded it is time to merge both data sets
# Set understable columns names

X_data = rbind(X_test, X_train)
y_data = rbind(y_test, y_train)
y_data=dplyr::rename(y_data, Activity.Id=V1)
subject_data = rbind(subject_test,subject_train)
subject_data=dplyr::rename(subject_data, subject_id=V1)

#
##### 2.Extracts only the measurements on the mean and standard deviation for each measurement.
#
# Not sure exactly how to interpret this question, therefore I start extracting the names
# of the columns with mean and standard deviation
# And create a subset of X_data with means and std deviations

mean_measurements_X_data=grep("mean()", names(X_data))
std_measurements_X_data=grep("std()", names(X_data))
X_data_mean_std <- X_data[,c(mean_measurements_X_data,std_measurements_X_data)]

#
#### 3.Uses descriptive activity names to name the activities in the data set
#
# you need to get the activity numbers in the data and replace them with 
# descriptive terms which are words (taken from thoughtfulbloke aka David Hood)
# Therefore I take the recently created X_data_mean_std data set
# And I will merge it with y_data which is the activities data set
#
y_data_activties=cbind(subject_data,y_data,X_data_mean_std)
# And finally merge with activity_labels to get the proper activity name
activity_names=merge(activity_labels,y_data_activties)

#
#### 4. Appropriately labels the data set with descriptive variable names.
#
names(activity_names) <- gsub("class", "Activity", names(activity_names))
names(activity_names) <- gsub("^t", "Time", names(activity_names))
names(activity_names) <- gsub("^f", "Frequency", names(activity_names))
names(activity_names) <- gsub("Acc", "Accelerometer", names(activity_names))
names(activity_names) <- gsub("Gyro", "Gyroscope", names(activity_names))
names(activity_names) <- gsub("Mag", "Magnitude", names(activity_names))
names(activity_names) <- gsub("std", "StandardDeviation", names(activity_names))
names(activity_names) <- gsub("Freq", "Frequency", names(activity_names))
names(activity_names) <- gsub("mean", "Mean", names(activity_names))
names(activity_names) <- gsub("gravity", "Gravity", names(activity_names))


#
#### 5. From the data set in step 4, creates a second, independent tidy data set
#       with the average of each variable for each activity and each subject.
#
second_tidy_data<-aggregate(activity_names[, 4:82], list(activity_names$Activity.Name), mean)
write.table(second_tidy_data, file="./second.tidy.data.csv", row.name=FALSE)
