# This code assumes you are in the folder where the text files were extracted
  
#clear the workspace
rm(list=ls())

cols <- read.delim("./UCI HAR Dataset/features.txt",header = FALSE, sep = "")
cols <- cols$V2

library(dplyr)
library(tidyr)

#Pull in the label names for the activities
actlabels <- read.delim("./UCI HAR Dataset/activity_labels.txt",header = FALSE,sep = "")
#Rename the column names for the coming merge
colnames(actlabels) <- c("Nr","Activity")

#Get the test data for X and for Y
test_x <- read.delim("./UCI HAR Dataset/test/X_test.txt",header = FALSE, sep = "")
colnames(test_x) <- cols
test_y <- read.delim("./UCI HAR Dataset/test/y_test.txt",header = FALSE, sep = "")
colnames(test_y) <- "Nr"

#Merge the data and order
test_y$ID  <- 1:nrow(test_y)
test_y2  <- merge(test_y,actlabels, by = "Nr")

#does the actual order as per the ID. Reverts back to original order of y_test
#And adds this to the front of the testdata
test_x2 <- cbind(test_y2[order(test_y2$ID), ],test_x)
#Remove the ID column as it is not necessary anymore
test_x2$ID <- NULL

#Get the train data for X and for Y
train_x <- read.delim("./UCI HAR Dataset/train/X_train.txt",header = FALSE, sep = "")
colnames(train_x) <- cols
train_y <- read.delim("./UCI HAR Dataset/train/y_train.txt",header = FALSE, sep = "")
colnames(train_y) <- "Nr"

#Merge the data and order
train_y$ID  <- 1:nrow(train_y)
train_y2  <- merge(train_y,actlabels, by = "Nr")

#does the actual order as per the ID. Reverts back to original order of y_test
#And adds this to the front of the testdata
train_x2 <- cbind(train_y2[order(train_y2$ID), ],train_x)
#Remove the ID column as it is not necessary anymore
train_x2$ID <- NULL

#Combine the two data sets and view the result
#1. Merges the training and the test sets to create one data set.
comb_x <- rbind(test_x2,train_x2)

#Remove duplicate columns. Had to remove duplicates otherwise the select throws an error
duplicated.columns <- duplicated(t(comb_x))
comb_x2 <- comb_x[, !duplicated.columns]

#Only use the mean and std columns
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
comb_x3 <- select(comb_x2,contains("Nr"), contains("Activity"), contains("mean"), contains("std"))

#Rename and clean up the columns to be more descriptive
nucols <- colnames(comb_x3)
colnames(comb_x3) <- gsub("-std","(std)",
                     gsub("-mean","(mean)",
                     gsub("Gravity","_Gravity",
                     gsub("\\()","",
                     gsub("Freq","_Freq",
                     gsub("Mean","_Mean",
                     gsub("Mag","_Mag",
                     gsub("Jerk","_Jerk",
                     gsub("Body","_Body",
                     gsub("BodyBody","Body",
                     gsub("Acc","_Acc",
                     gsub("Gyro","_Gyro",nucols))))))))))))

#Separate the data set into different activity data sets
acts <- actlabels$Activity
combb <- arrange(comb_x3,Nr)

#This is the final combined dataframe. Export it.
final_combined <- combb
write.table(final_combined, "./final_combined.txt", sep="\t")

#Add all columns except the first two
final_df_mean <- select(final_combined,-(Nr:Activity))
#Empty the dataframe and keep the columns
final_df_mean <- final_df_mean[0,]

#Save the colnames of the dataframe
tmpcols <- names(final_df_mean)

#Assign the different activities to different data frames for averaging and exclude the Nr and Activity
#columns
for(i in 1:length(acts)){
        #Split my different activities into different dataframes
        #i.e. WALKING,WALKING_UPSTAIRS,WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING            
        assign(as.character(acts[i]),select(final_combined[final_combined$Activity == acts[i],],-(Nr:Activity)))
        #Apply the mean values to above dataframe and append to dataframe final_df_mean
        final_df_mean <- rbind(final_df_mean,lapply(select(final_combined[final_combined$Activity == acts[i],],-(Nr:Activity)),mean))
        #Reset my colnames. For some reason they change when I use rbind
        colnames(final_df_mean) <- tmpcols
}

#Add the different activities as rownames and view the final dataframe with just the means inserted
rownames(final_df_mean) <- acts
write.table(final_df_mean, "./final_df_mean.txt", sep="\t")

