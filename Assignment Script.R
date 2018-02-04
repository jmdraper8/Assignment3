# Assignment
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Set working directory
setwd("~/OneDrive/Data Science/datasciencecoursea/C3_GettingandCleaningData/Assignment")

#Read data in and store for data manipulation and cleansing
trainDataX <- read.table("./train/X_train.txt", sep = "", header = F, na.strings ="", stringsAsFactors= F)
trainDataY <- read.table("./train/Y_train.txt", sep = "", header = F, na.strings ="", stringsAsFactors= F)
subjectTrainData <- read.table("./train/subject_train.txt", sep = "", header = F, na.strings ="", stringsAsFactors= F)

testDataX <- read.table("./test/X_test.txt", sep = "", header = F, na.strings ="", stringsAsFactors= F)
testDataY <- read.table("./test/Y_test.txt", sep = "", header = F, na.strings ="", stringsAsFactors= F)
subjectTestData <- read.table("./test/subject_test.txt", sep = "", header = F, na.strings ="", stringsAsFactors= F)

activities<- read.table("./activity_labels.txt", sep = "", header = F, na.strings ="", stringsAsFactors= F)
features <- read.table("./features.txt", sep = "", header = F, na.strings ="", stringsAsFactors= F)

#Add Activity Lable Column to Y data for train and test data sets
testDataY[,2] = activities$V2[testDataY[,1]]
trainDataY[,2] = activities$V2[trainDataY[,1]]

#Set Column Header names
names(features) = c("Feature_ID", "Feature_Label")
names(subjectTrainData) = "Subject_ID"
names(trainDataY) = c("Activity_ID", "Activity_Label")
colnames(trainDataX) [1:561] <- features$Feature_Label

names(subjectTestData) = "Subject_ID"
names(testDataY) = c("Activity_ID", "Activity_Label")
colnames(testDataX) [1:561] <- features$Feature_Label

names(features) = c("Feature_ID", "Feature_Label")
names(activities) = c("Activity_ID", "Activity_Label")


#Extract features Mean and Std
extract_features <- grepl("mean|std", features$Feature_Label)

#Extract only the Standard Deviation and Mean measurements
testDataX = testDataX[,extract_features]
trainDataX = trainDataX[,extract_features]

#Merge Data
testData <- cbind(subjectTestData, testDataY, testDataX)
trainData <- cbind(subjectTrainData, trainDataY, trainDataX)
fullData = rbind(testData, trainData)

#Extract the mean and std data from the full data set 
idLabels   = c("Subject_ID", "Activity_ID", "Activity_Label")
dataLabels = setdiff(colnames(fullData), idLabels)
mergeData = melt(fullData, id = idLabels, measure.vars = dataLabels)

# Apply mean function to dataset
cleanData = dcast(mergeData, Subject_ID + Activity_Label ~ variable, mean)

#Export the data to a file
write.table(cleanData, file = "./tidy_data.txt")