library(dplyr)
library(tidyr)

## Import and Merge Training Data
train_x <- read.table("UCI HAR Dataset//train/X_train.txt", nrows=7352, comment.char="")
train_sub <- read.table("UCI HAR Dataset//train/subject_train.txt", col.names=c("subject"))
train_y <- read.table("UCI HAR Dataset/train//y_train.txt", col.names=c("activity"))
train_data <- cbind(train_x, train_sub, train_y)

## Import and Merge Test Data
test_x <- read.table("UCI HAR Dataset//test/X_test.txt", nrows=2947, comment.char="")
test_sub <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject"))
test_y <- read.table("UCI HAR Dataset/test//y_test.txt", col.names=c("activity"))
test_data <- cbind(test_x, test_sub, test_y)

## Merge Test and Training Data
data <- rbind(train_data, test_data)

## Read Features
feature_list <- read.table("UCI HAR Dataset//features.txt", col.names = c("id", "name"))
features <- c(as.vector(feature_list[, "name"]), "subject", "activity")

## Filter for mean or std in the name
filtered_feature_ids <- grepl("mean|std|subject|activity", features) & !grepl("meanFreq", features)
filtered_data = data[, filtered_feature_ids]

## Read activity_labels.txt & apply to data
activities <- read.table("UCI HAR Dataset//activity_labels.txt", col.names=c("id", "name"))
for (i in 1:nrow(activities)) {
    filtered_data$activity[filtered_data$activity == activities[i, "id"]] <- as.character(activities[i, "name"])
}

## Make readable variable names
filtered_feature_names <- features[filtered_feature_ids]
filtered_feature_names <- gsub("\\(\\)", "", filtered_feature_names)
filtered_feature_names <- gsub("Acc", "-acceleration", filtered_feature_names)
filtered_feature_names <- gsub("Mag", "-Magnitude", filtered_feature_names)
filtered_feature_names <- gsub("^t(.*)$", "\\1-time", filtered_feature_names)
filtered_feature_names <- gsub("^f(.*)$", "\\1-frequency", filtered_feature_names)
filtered_feature_names <- gsub("(Jerk|Gyro)", "-\\1", filtered_feature_names)
filtered_feature_names <- gsub("BodyBody", "Body", filtered_feature_names)
filtered_feature_names <- tolower(filtered_feature_names)

names(filtered_data) <- filtered_feature_names

## Create a tidy data set with the average of each var for each activity and subject
tidy_data <- tbl_df(filtered_data) %>%
    group_by(subject, activity) %>%
    summarise_each(funs(mean)) %>%
    gather(measurement, mean, -activity, -subject)

write.table(tidy_data, file="tidy_data.txt", row.name=FALSE)
