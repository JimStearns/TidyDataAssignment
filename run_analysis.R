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

## Testing 1,2,3: use function to check inputs and return list of input files.
infiles <- vector(mode='list', length=8)
names(infiles) <- c("activity.labels", "feature.labels", "test.subject", "test.X", "test.y", "train.subject", "train.X", "train.y")
# Put the file names/paths to use in this dictionary.
infiles$activity.labels = activity.labels.file
infiles$feature.labels = feature.labels.file
infiles$test.subject = test.subject.file
infiles$test.X = test.X.file
infiles$test.y = test.y.file
infiles$train.subject = train.subject.file
infiles$train.X = train.X.file
infiles$train.y = train.y.file

inframes <- vector(mode='list', length=8)
names(infiles) <- c("activity.labels", "feature.labels", "test.subject", "test.X", "test.y", "train.subject", "train.X", "train.y")
# Put the file names/paths to use in this dictionary.
inframes$activity.labels = read.table(infiles$activity.labels, header=FALSE, 
                                      sep=" ", col.names=c("num", "desc"))
inframes$feature.labels = read.table(infiles$feature.labels, header=FALSE,
                                     sep=" ", col.names=c("num", "desc"))
inframes$test.subject = read.table(infiles$test.subject, header=FALSE)
inframes$test.X = read.table(infiles$test.X, header=FALSE)
inframes$test.y = read.table(infiles$test.y, header=FALSE)
inframes$train.subject = read.table(infiles$train.subject, header=FALSE)
inframes$train.X = read.table(infiles$train.X, header=FALSE)
inframes$train.y = read.table(infiles$train.y, header=FALSE)

#### Summary ####
# 1. Merge subject number, activity, and measurements in two contexts:
#   training set and test set:
#   - Column merge subject_test, X_test and y_test (column bind).
#   - Column merge subject_train, X_train and y_train (column bind).
# 2. Combine test and train data into a single file (row bind)
# 3. Convert activity column from integer to factor with labels from activity_labels.txt.
# 4. Label each column with labels from features.txt.
# 5. Eliminate all columns not having either "mean()" or "std()" in its label.
# 6. Sort by subject, then by activity.
# 7. Save dataset as TidyData_detailed.csv.txt
# 8. Using this tidy but detailed file as input, create a second, independent 
#   tidy data set with the average of each variable for each activity 
#   and each subject. 
#   - Check that the second dataset has 180 rows. Calculation: 
#       - There are 30 subjects.
#       - There are 6 activities (walking etc.)
#       - Each subject participated in every activity.
#       - There should be 180 rows of data in this tidy summary file.
#   - Save the second dataset to current working directory as "TidyData_summary.csv.txt"
#       (.txt allows file to be uploaded to Coursera)

# 1. Merge subject number, activity, and measurements in two contexts:
#   training set and test set:
#   - Column merge subject_test, X_test and y_test (column bind).
#   - Column merge subject_train, X_train and y_train (column bind).
test.merge <- cbind(inframes$test.subject, inframes$test.y, inframes$test.X)
train.merge <- cbind(inframes$train.subject, inframes$train.y, inframes$train.X)

# 2. Combine test and train data into a single file (row bind)
test.and.train <- rbind(train.merge, test.merge)

# 3. Convert activity column from integer to factor with labels from activity_labels.txt.


# 4. Label each column with labels from features.txt.
feature.label.vector = as.vector(inframes$feature.labels$desc)
colnames(test.and.train) <- c("subject", "activity", feature.label.vector)

# 5. Eliminate all columns not having either "mean()" or "std()" in its label.

# 6. Sort by subject, then by activity.

# 7. Save dataset as TidyData_detailed.csv.txt

# 8. Using this tidy but detailed file as input, create a second, independent 
#   tidy data set with the average of each variable for each activity 
#   and each subject. 
#   - Check that the second dataset has 180 rows. Calculation: 
#       - There are 30 subjects.
#       - There are 6 activities (walking etc.)
#       - Each subject participated in every activity.
#       - There should be 180 rows of data in this tidy summary file.
#   - Save the second dataset to current working directory as "TidyData_summary.csv.txt"
#       (.txt allows file to be uploaded to Coursera)
