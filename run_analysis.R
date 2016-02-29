# You should create one R script called run_analysis.R that does the following.
# 
# 1 Merges the training and the test sets to create one data set.
# 2 Extracts only the measurements on the mean and standard deviation for each measurement.
# 3 Uses descriptive activity names to name the activities in the data set
# 4 Appropriately labels the data set with descriptive variable names.
# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# - 'features_info.txt': Shows information about the variables used on the feature vector.
# 
# - 'features.txt': List of all features.
# 
# - 'activity_labels.txt': Links the class labels with their activity name.
# 
# - 'train/X_train.txt': Training set.
# 
# - 'train/y_train.txt': Training labels.
# 
# - 'test/X_test.txt': Test set.
# 
# - 'test/y_test.txt': Test labels.
# 
# The following files are available for the train and test data. Their descriptions are equivalent. 
# 
# - 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
# 
# - 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
# 
# - 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
# 
# - 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

# Answer

# 1 Merges the training and the test sets to create one data set.
activity_labels = read.table("activity_labels.txt")
features = read.table("features.txt")

# Reading data
subject_test = read.table("subject_test.txt", header = F)
X_test = read.table("X_test.txt", header = F)
y_test =  read.table("y_test.txt", header = F)

subject_train = read.table("subject_train.txt", header = F)
X_train = read.table("X_train.txt", header = F)
y_train = read.table("y_train.txt", header = F)

head(subject_test)
tail(subject_test)
head(subject_train)
head(X_train)
names(X_train)
head(y_train)
table(subject_test)
table(subject_train)

dim(subject_train)
dim(X_train)
dim(y_train)

dim(subject_test)
dim(X_test)
dim(y_test)

# Merging the datasets
mergedSet = rbind(X_train, X_test)
mergedLabels = rbind(y_train, y_test)
mergedSubject = rbind(subject_train,subject_test)

dim(mergedSet)
dim(mergedLabels)
dim(mergedSubject)

dim(features)
# Renaming tests based on feature file
colnames(mergedSet) = features$V2
# renaming colnames of codename 1-6 and colnames of subject
colnames(mergedLabels) = "Code"
colnames(mergedSubject) = "Subject"

# merging everything
completeMerged = cbind(mergedSet, mergedLabels, mergedSubject) 
dim(completeMerged)
colnames(completeMerged)

# 2 Extracts only the measurements on the mean and standard deviation for each measurement.

grep("mean",colnames(completeMerged), value  = T, ignore.case  = T)
grep("mean",colnames(completeMerged), value  = F, ignore.case  = T)
grep("std",colnames(completeMerged), value  = T, ignore.case  = T)
grep("std",colnames(completeMerged), value  = F, ignore.case  = T)

# extracting only mean and sdev, and add columns subject and code
mergedMeanStd = completeMerged[,c(sort(c(562,563,grep("mean",colnames(completeMerged), value  = F,  ignore.case  = T),
                                         grep("std",colnames(completeMerged), value  = F , ignore.case  = T)
                                         )))]
colnames(mergedMeanStd)
dim(mergedMeanStd)
head(mergedMeanStd)

# 3 Uses descriptive activity names to name the activities in the data set
library(dplyr)

colnames(activity_labels) = c("Code", "Activity")
mergedMeanStd$Subject = as.factor(mergedMeanStd$Subject)
table(a$Activity)
table(a$Code)
head(a)
dim(mergedMeanStd)

tempVar = merge(mergedMeanStd,activity_labels, by = "Code") #create temp variable just for merging purpose, and then replace the "Code" column in the main table

mergedMeanStd[,grep("Code", colnames(mergedMeanStd), value = F) ] = tempVar$Activity #replacing the code with activity 

colnames(mergedMeanStd)[grep("Code", colnames(mergedMeanStd), value = F)] = "Activity" #renaming the column name

mergedMeanStd

# 4 Appropriately labels the data set with descriptive variable names.

colnames(mergedMeanStd) = gsub("^t", "Time", colnames(mergedMeanStd), ignore.case = T)
colnames(mergedMeanStd) = gsub("^f", "Frequency", colnames(mergedMeanStd),  ignore.case = T)
colnames(mergedMeanStd) = gsub("std", "StandardDeviation", colnames(mergedMeanStd), ignore.case = T)
colnames(mergedMeanStd) = gsub("mean", "Average", colnames(mergedMeanStd), ignore.case = T)
colnames(mergedMeanStd) = gsub("\\()", "", colnames(mergedMeanStd))
colnames(mergedMeanStd) = gsub("acc", "Acceleration", colnames(mergedMeanStd), ignore.case = T)
colnames(mergedMeanStd) = gsub("tbody", "TimeBody", colnames(mergedMeanStd), ignore.case = T)
colnames(mergedMeanStd) = gsub("bodybody", "Body", colnames(mergedMeanStd), ignore.case = T)

# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy = mergedMeanStd[order(mergedMeanStd$Subject,mergedMeanStd$Activity),]
tidy  = aggregate(.~ Subject + Activity, mergedMeanStd, mean)
tidy = tidy[order(tidy$Subject,tidy$Activity),]

write.table(tidy, file = "Tidy.txt",row.name=FALSE)

