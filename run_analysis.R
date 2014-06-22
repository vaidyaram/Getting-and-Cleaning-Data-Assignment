# This library is required for the lable function
library(Hmisc)

#set the working directory
setwd("C:/Backup/Jagat/Statistics/Getting and Cleaning Data/Quiz/Assignment")

# 1  Merges the training and the test sets to create one data set

# 1A - Download the zip file
fileUrl= "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,"./Data/getdata-projectfiles-UCI HAR Dataset.zip")

#1B - unzip the zip file
unzip("./Data/getdata-projectfiles-UCI HAR Dataset.zip",exdir="./Data")

# 1C - Exract the features.txt.
#features.txt files has the both the sequence number of the variable/VariableID and the variable name
#Hence, we need to extract the variableID and variable name separately
feature_Info_file<- ("./Data/UCI HAR Dataset/features.txt")
feature_Info <-readChar(feature_Info_file,file.info(feature_Info_file)$size)
feature_Info_table<-read.table ("./Data/UCI HAR Dataset/features.txt",comment.char = "",sep="\n",strip.white = TRUE)
feature_Info_table$ID = as.character(lapply(strsplit(as.character(feature_Info_table$V1), split=" "), "[", 1))
feature_Info_table$ActivityName = as.character(lapply(strsplit(as.character(feature_Info_table$V1), split=" "), "[", 2))


# 1D - Prepare the "train" data set
# X_Train.txt has the measurements for subjects who are in the "train" condition
# Label the column names to activity names extracted in Step 1C. The raw "X_Train.txt" file does not contains
# activity names. Hence, we need to label the column names
x_train <- read.table ("./Data/UCI HAR Dataset/train/X_train.txt",comment.char = "",sep="",colClasses="numeric");
names(x_train)<-c(feature_Info_table$ActivityName)

# 1E - "subject_train.txt" file has the IDs of the subjects corresponding to the measurements in "X_Train.txt"
# Hence, read the subject_train.txt file and label the column as subject ID
subject_train <- read.table ("./Data/UCI HAR Dataset/train/subject_train.txt",comment.char = "",sep="",colClasses="numeric");
names(subject_train)<-c("Subject ID")

# 1F - "y_train.txt" file has the activity code for each measurement
# read the "y_train.txt", which has the activity ID corresponding to each measurement in X_train.txt
# Replace the ACTIVITY ID with the activity description as mentioned in "activity_labels.txt"
# This step is hard coded. However, we can also read the "activity_labels.txt" and write code to 
# map each of the IDs to activity label

y_train <- read.table ("./Data/UCI HAR Dataset/train/y_train.txt",comment.char = "",sep="",colClasses="numeric");
names(y_train)<-c("Activity_Label")

y_train$Activity_Label = as.character(lapply(y_train$Activity_Label, function(x) if(x=="1") x<-"WALKING" 
                                                                                else if(x=="2") x<-"WALKING_UPSTAIRS" 
                                                                                else if(x=="3") x<-"WALKING_DOWNSTAIRS" 
                                                                                else if(x=="4") x<-"SITTING" 
                                                                                else if(x=="5") x<-"STANDING" 
                                                                                else if(x=="6") x<-"LAYING"))
# 1G - Merge the subjectID, actitivity ID and measurements for the training data set
# After this step, we have created comprehensive training data set
merge_activity_train<-cbind(y_train$Activity_Label,x_train)
merge_subject_activity_train<-cbind(subject_train$"Subject ID",merge_activity_train)

# 1H - Prepare the "test" data set
# X_Test.txt has the measurements for subjects who are in the "test" condition
# Label the column names to activity names extracted in Step 1C. The raw "X_Test.txt" file does not contains
# activity names. Hence, we need to label the column names
x_test <- read.table ("./Data/UCI HAR Dataset/test/X_test.txt",comment.char = "",sep="",colClasses="numeric");
names(x_test)<-c(feature_Info_table$ActivityName)

# 1I - "subject_test.txt" file has the IDs of the subjects corresponding to the measurements in "X_Test.txt"
# Hence, read the subject_test.txt file and label the column as subject ID
subject_test <- read.table ("./Data/UCI HAR Dataset/test/subject_test.txt",comment.char = "",sep="",colClasses="numeric");
names(subject_test)<-c("Subject ID")

#read the y_test, which has the activity data for each measurement and add descriptive label
y_test <- read.table ("./Data/UCI HAR Dataset/test/y_test.txt",comment.char = "",sep="",colClasses="numeric");
names(y_test)<-c("Activity_Label")

# 1J - "y_test.txt" file has the activity code for each measurement
# read the "y_test.txt", which has the activity ID corresponding to each measurement in X_test.txt
# Replace the ACTIVITY ID with the activity description as mentioned in "activity_labels.txt"
# This step is hard coded. However, we can also read the "activity_labels.txt" and write code to 
# map each of the IDs to activity label
y_test$Activity_Label = as.character(lapply(y_test$Activity_Label, function(x) if(x=="1") x<-"WALKING" 
                                             else if(x=="2") x<-"WALKING_UPSTAIRS" 
                                             else if(x=="3") x<-"WALKING_DOWNSTAIRS" 
                                             else if(x=="4") x<-"SITTING" 
                                             else if(x=="5") x<-"STANDING" 
                                             else if(x=="6") x<-"LAYING"))
# 1K - Merge the subjectID, actitivity ID and measurements for the test data set
# After this step, we have created comprehensive test data set
merge_activity_test<-cbind(y_test$Activity_Label,x_test)
merge_subject_activity_test<-cbind(subject_test$"Subject ID",merge_activity_test)



#1L - The merge in step 1G and 1K changes the column names. Hence, we need to change the names of columns 1 and 2
# of each data set to "Subject_ID" and "Activity_ID". This is to ensure that we can merge the 
# "test" and "train" data sets.
colnames(merge_subject_activity_train)[1]<-"Subject_ID"
colnames(merge_subject_activity_test)[1]<-"Subject_ID"
colnames(merge_subject_activity_train)[2]<-"Activity_ID"
colnames(merge_subject_activity_test)[2]<-"Activity_ID"

#1M - Now, we can merge the test and train data sets to create a tidy data set
tidyData<-rbind(merge_subject_activity_train,merge_subject_activity_test)


# To check, find the dimension of the tidy data. There are 561 variables, Subject_ID and Activity_ID. 
#Hence, 563 columns. Hehe overall tidy data set should have 10299 rows and 563 columns. 
print(paste("The dimension of train data set are -#rows ",nrow(merge_subject_activity_train), "#columns", ncol(merge_subject_activity_train)))
print(paste("The dimension of test data set are -#rows ",nrow(merge_subject_activity_test),"#columns", ncol(merge_subject_activity_test)))
print(paste("The dimension of tidy data set are -#rows ", nrow(tidyData),"#columns ", ncol(tidyData)))

#2 -"Extracts only the measurements on the mean and standard deviation for each measurement."

#2a - Get the column names from tidyData set. Get the column names that have mean or standard deviation in the 
# column name. Extract each of the column whose name has mean or standard deviation and add it to 
# targetData set.Now, targetData contains only the filtered data with mean or standard deviation of measurements

columnNames <-names(tidyData)
mean_std_ColumnNames<-vector()
targetData<-vector()

for (i in seq(columnNames))
{
   if((length(grep("mean",columnNames[i]))==1)| (length(grep("std",columnNames[i]))==1))
   {
     targetData<-cbind(targetData,tidyData[,i]) 
     mean_std_ColumnNames<-rbind(mean_std_ColumnNames,columnNames[i])
   } 
}

#2b - Set the column names to those having mean or standard deviation
targetDatadf<-as.data.frame(targetData)
names(targetDatadf)<-c(mean_std_ColumnNames)

#2c - Now, the previous data set contains only measurements of columns have mean or standard deviation in the 
# column names. However, "Subject_ID" and "Activity_ID" will not be extracted in Step 2a. 
# So, we need to add the Subject_ID and Activity_ID to the data set
tidyData_Activity<-cbind(tidyData$"Activity_ID",targetDatadf)
tidyData_Student_Activity<-cbind(tidyData$"Subject_ID",tidyData_Activity)
colnames(tidyData_Student_Activity)[1]<-"Subject_ID"
colnames(tidyData_Student_Activity)[2]<-"Activity_ID"

print(paste("The dimension of tidy data set with the mean and standard deviation measurements are - #rows ",nrow(tidyData_Student_Activity), "#columns ", ncol(tidyData_Student_Activity)))

#3 Uses descriptive activity names to name the activities in the data set
# The names of activity names have already been replaced in steps 1J and 1F


#4 - Appropriately labels the data set with descriptive variable names

# Check the variable names and label the variables accordingly
# The variable names contains the following component - tBody or fBody or tGravity. The next level of variable component is "Acc" or "Gyro"
# Hence, the descriptive labels are added based on the components of the column name/variable name.

tidyData_Student_Activity_ColumnNames<-names(tidyData_Student_Activity)
#add data labels for the variables
for (i in seq(tidyData_Student_Activity_ColumnNames))
{
  # Labels for tBodyAcc
  if(length(grep("tBody",tidyData_Student_Activity_ColumnNames[i]))==1)
  {
      if(length(grep("Acc",tidyData_Student_Activity_ColumnNames[i]))==1)
      {
        label(tidyData_Student_Activity[,i])<-"This is time domain signal for body acceletation measurement" 
      }
      else if(length(grep("Gyro",tidyData_Student_Activity_ColumnNames[i]))==1)
      {
        label(tidyData_Student_Activity[,i])<-"This is time domain signal for gyro measurement"
      }
  }
  if(length(grep("fBody",tidyData_Student_Activity_ColumnNames[i]))==1)
  {
    if(length(grep("Acc",tidyData_Student_Activity_ColumnNames[i]))==1)
    {
      label(tidyData_Student_Activity[,i])<-"This is frequency domain signal for body acceletation measurement (Fast Fourier Transform (FFT)" 
    }
    else if(length(grep("Gyro",tidyData_Student_Activity_ColumnNames[i]))==1)
    {
      label(tidyData_Student_Activity[,i])<-"This is frequency domain signal for gyro measurement (Fast Fourier Transform (FFT)" 
    }
  }
  if(length(grep("tGravity",tidyData_Student_Activity_ColumnNames[i]))==1)
  {
      label(tidyData_Student_Activity[,i])<-"This is time domain signal for gravity measurement" 
  }
}

#5 Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#5a - For this set, we need to subset the data for each combination of subject and varible name. 
# We need to get the average of the column in the subset and then create a new data set with the average
# values of measurements for each subject and activity

activity_set=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
newData=data.frame()
secondTidyDataSet=data.frame()

#iterate for all subject IDs
for(i in 1:30)
{
  #iterate for each activity type
  for (j in activity_set)
  {
    #extract the subject for each ID and activity combination
    newData<-tidyData[(tidyData$Subject_ID == i & tidyData$Activity_ID == j),] 
    c1<-cbind(j,t(colMeans(newData[3:563])))
    c2<-cbind(i,c1)
    #add a row for the subjectID, activityID and the average of column names
    secondTidyDataSet=rbind(secondTidyDataSet,c2)
  }
}

#5b - Change the column names,by adding -AVERAGE suffix to reflect that each column is now an average of all the measurements 
# for each subject and activity
oldColumnNames<-names(secondTidyDataSet)
newColumnNames<-vector()

for(i in oldColumnNames)
{
  newColumnNames<-paste(oldColumnNames,"-AVERAGE")
}

names(secondTidyDataSet)<-c(newColumnNames)
colnames(secondTidyDataSet)[1]<-"Subject_ID"
colnames(secondTidyDataSet)[2]<-"Activity_ID"

# 5c- Check the dimensions of the second tidy data set. There are 30 subjects and 6 activities.
#Hence, the seconTidyDataSet should have 180 rows and 563 columns 
print(paste("The dimensions of the second tidy set are - #rows ",nrow(secondTidyDataSet), "#columns ", ncol(secondTidyDataSet)))

# 5d- Now, write the seconTidyData into a text file
write.table(secondTidyDataSet,file="./Data/SecondTidyDataSet.txt", sep = " ", col.names = TRUE)
