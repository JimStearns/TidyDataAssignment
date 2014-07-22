# John Hopkins Data Science Coursera "Getting and Cleaning Data"
# Submitted by Jim Stearns in satisfaction of course assignment.
# File: run_analysis.R

# Overview

# Assumes that raw data files exists in current working directory.
# I.e. this script "can be run as long as the Samsung data is in your working directory"
#
# Unclear from instructions: have test and train data files been flattened 
# so they're all in PWD, or are they in the "test" and "train" subdirectories?
# This script will work with either. It looks first in PWD, then in subdirectory
# of PWD
# Labels for activities (walking et al) and features (body acceleration et al)
# must be in PWD.
activity.labels.file = "activity_labels.txt"
feature.labels.file = "features.txt"
stopifnot(file.exists(activity.labels.file), file.exists(feature.labels.file))

tryInPwdAndSubdir <- function(file, subdir) {
    if (file.exists(file)) {
        return(file)
    }
    try2 = file.path(subdir, file)
    if (file.exists(try2)) {
        return(try2)
    }
    stop(paste("Could not find file '", file, 
               "' in current directory or subdirectory '", subdir, "'.", sep=""))
}

test.subject.file = tryInPwdAndSubdir("subject_test.txt", "test")
test.X.file = tryInPwdAndSubdir("X_test.txt", "test")
test.y.file = tryInPwdAndSubdir("y_test.txt", "test")

train.subject.file = tryInPwdAndSubdir("subject_train.txt", "train")
train.X.file = tryInPwdAndSubdir("X_train.txt", "train")
train.y.file = tryInPwdAndSubdir("y_train.txt", "train")

# Column merge subject_test, X_test and y_test (column bind).
# Column merge subject_train, X_train and y_train (column bind).
# Combine test and train data into a single file (row bind)
# Convert activity column from integer to factor with labels from activity_labels.txt.
# Label each column with labels from features.txt.
# Eliminate all columns not having either "mean()" or "std()" in its label.
# Sort by subject, then by activity.
# Use this file as input for creating a second, independent tidy data set
#   with the average of each variable for each activity and each subject. 

