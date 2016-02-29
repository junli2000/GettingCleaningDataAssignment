run_analysis <- function() {

    ## Read data files
    features <- read.table("UCI HAR Dataset\\features.txt", sep="")
    activity_labels <- read.table("UCI HAR Dataset\\activity_labels.txt", sep="")
    x_test <- read.table("UCI HAR Dataset\\test\\x_test.txt", sep="")
    y_test <- read.table("UCI HAR Dataset\\test\\y_test.txt", sep="")
    subject_test <- read.table("UCI HAR Dataset\\test\\subject_test.txt", sep="")
    x_train <- read.table("UCI HAR Dataset\\train\\x_train.txt", sep="")
    y_train <- read.table("UCI HAR Dataset\\train\\y_train.txt", sep="")
    subject_train <- read.table("UCI HAR Dataset\\train\\subject_train.txt", sep="")

    ## Merges the training and the test sets to create one data set
    data_set = rbind(x_test, x_train)
    activities = rbind(y_test, y_train)
    subjects = rbind(subject_test, subject_train)
    
    ## Extracts only the measurements on the mean and standard deviation for each measurement
    col1 <- grep("mean()", features$V2)
    col2 <- grep("std()", features$V2)
    cols <- sort(c(col1, col2))
    data <- data_set[, cols]
    
    ## Uses descriptive activity names to name the activities in the data set
    names(activities) <- c("ActivityID")
    names(subjects) <- c("SubjectID")
    data <- cbind(activities, subjects, data)
    names(activity_labels) <- c("ActivityID", "ActivityName")
    data <- merge(activity_labels, data, by.x="ActivityID", by.y="ActivityID", all=TRUE)
    data <- data[, 2:ncol(data)]

    ## Appropriately labels the data set with descriptive variable names
    v_names <- names(data)
    for (i in 3:length(v_names))
    {
        v_names[i] <- sub("V", "", v_names[i])
        v_names[i] <- as.character(features[features$V1==v_names[i], 2])
    }
    names(data) <- v_names
    
    ## Creates tidy data set with the average of each variable for each activity and each subject
    library(dplyr)
    library(reshape2)
    melted_data <- melt(data, id.vars=c("ActivityName", "SubjectID"))
    grouped_data <- group_by(melted_data, ActivityName, SubjectID, variable)
    tidy_data_set <- summarise(grouped_data, mean=mean(value))
    write.table(tidy_data_set, file="tidy_data_set.txt", row.names=FALSE)

    ## Return tidy data set
    tidy_data_set
}
