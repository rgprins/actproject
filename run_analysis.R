#Rscript which
#Merges the training and the test sets to create one data set. (Described in step 1)
#Extracts only the measurements on the mean and standard deviation for each measurement. (described in step 2)
#Uses descriptive activity names to name the activities in the data set (described in step 4)
#Appropriately labels the data set with descriptive variable names. (described in step 3)
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#NOTE: PLYR is needed to run this Rscript

#STEP 1.1: load all the datasets
#load the test datasets
test_set <- read.table("./test/X_test.txt", header=FALSE)
test_subject <- read.table("./test/subject_test.txt", header=FALSE)
test_activitylevels <- read.table("./test/Y_test.txt", header=FALSE)
#load the train datasets
train_set <- read.table("./train/X_train.txt", header=FALSE)
train_subject <- read.table("./train/subject_train.txt", header=FALSE)
train_activitylevels <- read.table("./train/Y_train.txt", header=FALSE)

#STEP 1.2: merge all the data
#merge the three different datasets (subject identifier, observed activity levels 
#and  measurement data) for the testset and traningsets
test_totalset <- cbind(test_subject, test_activitylevels, test_set))
train_totalset <- cbind(train_subject, train_activitylevels, train_set)

#bind both the test and train datasets
totalset <- rbind(test_totalset, train_totalset)

#STEP 2: load description of data to select mean and std variables and subset based on this
#load the feature description data and create a vector (labels) from this
varlabs <- read.table("features.txt", header=FALSE)
labels <- varlabs[,2]

#create two logical vectors to indicate whether a variable is a mean or a
#standard deviation
meanindex<-grepl("mean", labels, ignore.case=TRUE)
stdindex<-grepl("std", labels, ignore.case=TRUE)
#create a sum of both logical vectors (overallindex) which indicates 
#whether the variable is a mean or a standard deviation (1) or not (0)
overallindex<-meanindex+stdindex

#create a vector which indicates which variables to keep (i.e mean or std)
#first create the empty vector keepindex
#already store the first (subjectindex) and second positions (activitylevel) in this vector
keepindex<-c(1, 2)
labelindex<-vector()
#loop which extracts the position of each variable 
for (i in 1:length(overallindex)) {
  
  if(overallindex[i]==1){
    #save i+2 value in index, because first index is subjectindex and second is activities
    keepindex <- c(keepindex, i+2)
    labelindex <- c(labelindex, i)
  } 
}

#subset based on the created keepindex vector
keep_set <- totalset[, keepindex]
#load the variablenames to keep
keeplabels <- labels[labelindex]
#extend this with subjectid and activities
keeplabels <- c("subjectid", "activities", as.character(keeplabels))
#create columnnames
colnames(keep_set) <- c(keeplabels)

#STEP 3 label the data
#create a factor with the activity labels
acti_labels <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

#attach the labels to the numerical activity categories, by creating a factor
keep_set$activities <- factor(keep_set$activities, levels = c(1,2,3,4,5,6), labels = acti_labels)


#STEP 4: taking means of all variables according to subjectid and activities, using plyr
#use the numcolwise function to create means on all variables
library(plyr)
tidy_set <- ddply(keep_set, ~subjectid ~activities, numcolwise(mean))

#STEP 5: write the resulting tidy dataset
write.table(tidy_set, file="tidydata.txt", row.names=FALSE) 