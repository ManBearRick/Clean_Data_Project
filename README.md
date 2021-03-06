## This readme.md is the same as the included CodeBook.rmd file.
---
title: "Code Book"
author: "Rick Ortega"
date: "4/21/2017"
output: html_document

## Code Book for run_analyis.R

This code book is to further understand the output of the run_analysis.R script within this repository. 

The script creates a tidy version of the University of California Irvine's dataset for Human Activity Recognition using smartphones that can be used for further research and analysis. 

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 

The run_analysis script creates a subset of the original data by selecting the mean and standard deviation measurements for each of the 33 signals which leaves us with 66 total features. After adding the 'activity' and 'subject' columns, we are left with 68 columns in our tidy data subset. 

In order to arrive at the final tidy data, the subset is further reduced by calculating the mean of the observations by activity and subject pair to generate 180 observations (6 activities * 30 subjects) of the same 68 variables. This dataset is tidied to generate a narrow and lean dataset containing 11880 observations with 4 variables each and is saved as a text file in the current working directory with the name tidy_data.txt

## Cleaning up the variable names

Here is how I cleaned up the variable names 

filtered_feature_names <- gsub("\\(\\)", "", filtered_feature_names)
filtered_feature_names <- gsub("Acc", "-acceleration", filtered_feature_names)
filtered_feature_names <- gsub("Mag", "-Magnitude", filtered_feature_names)
filtered_feature_names <- gsub("^t(.*)$", "\\1-time", filtered_feature_names)
filtered_feature_names <- gsub("^f(.*)$", "\\1-frequency", filtered_feature_names)
filtered_feature_names <- gsub("(Jerk|Gyro)", "-\\1", filtered_feature_names)
filtered_feature_names <- gsub("BodyBody", "Body", filtered_feature_names)
filtered_feature_names <- tolower(filtered_feature_names)

## Description of the variables

The tidy data contains 11880 observations of 6 activities and 30 subjects. The following are the columns in the tidy data.

    - subject
    - activity
    - measurement
    - mean
    
# Subject

A numeric id representing which of the 30 volunteers represent the corresponding data.

# Activity

The name of the activity being performed which can be one of the following.

    - laying
    - sitting
    - standing
    - walking
    - walking_downstairs
    - walking_upstairs
    
# Measurement

The name of the measurement for which the mean is calculated. This variable can contain one of the following 66 variables. Please refer the codebook with the original dataset for the explanation of these different variables.

    - body-acceleration-jerk-magnitude-mean-frequency
    - body-acceleration-jerk-magnitude-mean-time
    - body-acceleration-jerk-magnitude-std-frequency
    - body-acceleration-jerk-magnitude-std-time
    - body-acceleration-jerk-mean-x-frequency
    - body-acceleration-jerk-mean-x-time
    - body-acceleration-jerk-mean-y-frequency
    - body-acceleration-jerk-mean-y-time
    - body-acceleration-jerk-mean-z-frequency
    - body-acceleration-jerk-mean-z-time
    - body-acceleration-jerk-std-x-frequency
    - body-acceleration-jerk-std-x-time
    - body-acceleration-jerk-std-y-frequency
    - body-acceleration-jerk-std-y-time
    - body-acceleration-jerk-std-z-frequency
    - body-acceleration-jerk-std-z-time
    - body-acceleration-magnitude-mean-frequency
    - body-acceleration-magnitude-mean-time
    - body-acceleration-magnitude-std-frequency
    - body-acceleration-magnitude-std-time
    - body-acceleration-mean-x-frequency
    - body-acceleration-mean-x-time
    - body-acceleration-mean-y-frequency
    - body-acceleration-mean-y-time
    - body-acceleration-mean-z-frequency
    - body-acceleration-mean-z-time
    - body-acceleration-std-x-frequency
    - body-acceleration-std-x-time
    - body-acceleration-std-y-frequency
    - body-acceleration-std-y-time
    - body-acceleration-std-z-frequency
    - body-acceleration-std-z-time
    - body-gyro-jerk-magnitude-mean-frequency
    - body-gyro-jerk-magnitude-mean-time
    - body-gyro-jerk-magnitude-std-frequency
    - body-gyro-jerk-magnitude-std-time
    - body-gyro-jerk-mean-x-time
    - body-gyro-jerk-mean-y-time
    - body-gyro-jerk-mean-z-time
    - body-gyro-jerk-std-x-time
    - body-gyro-jerk-std-y-time
    - body-gyro-jerk-std-z-time
    - body-gyro-magnitude-mean-frequency
    - body-gyro-magnitude-mean-time
    - body-gyro-magnitude-std-frequency
    - body-gyro-magnitude-std-time
    - body-gyro-mean-x-frequency
    - body-gyro-mean-x-time
    - body-gyro-mean-y-frequency
    - body-gyro-mean-y-time
    - body-gyro-mean-z-frequency
    - body-gyro-mean-z-time
    - body-gyro-std-x-frequency
    - body-gyro-std-x-time
    - body-gyro-std-y-frequency
    - body-gyro-std-y-time
    - body-gyro-std-z-frequency
    - body-gyro-std-z-time
    - gravity-acceleration-magnitude-mean-time
    - gravity-acceleration-magnitude-std-time
    - gravity-acceleration-mean-x-time
    - gravity-acceleration-mean-y-time
    - gravity-acceleration-mean-z-time
    - gravity-acceleration-std-x-time
    - gravity-acceleration-std-y-time
    - gravity-acceleration-std-z-time
    
# Mean

The mean of the measurement.
