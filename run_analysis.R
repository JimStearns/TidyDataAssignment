# John Hopkins Data Science Coursera "Getting and Cleaning Data"
# Submitted by Jim Stearns in satisfaction of course assignment.
# File: run_analysis.R
# Due Date: 27-Jul-2014

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
# 3. Use descriptive activity names to name the activities in the data set
#   (one option: convert activity column from integer to factor with labels 
#   from activity_labels.txt). Or just add activityDescription column.
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

# 4. Label each column with "tidied" labels from features.txt.
#   By "tidied", column labels are:
#   - all lowercase.
#   - descriptive (avoid cryptic abbreviations). But there is a data dictionary.
#   - avoid underscores and white spaces. My rule: allow periods, within reason:
#       strip trailing periods, downsize a sequence of multiple periods to just one.
#   - not duplicated.
make.tidy.column.names <- function(raw.column.names) {
    # 1. apply make.names(), requiring uniqueness, disallowing underscores.
    #       This takes care of spaces as well.
    # 2. tolower.
    # 3. Allow periods, but trim excesses (multiple, trailing)
    # 4. Left "bodybody" as is.
    tidy.names.1 <- tolower(make.names(raw.column.names, unique=TRUE, allow_=FALSE))
    tidy.names.singledots = gsub("\\.$", "", gsub("(\\.){2,}", "\\.", tidy.names.1))

    if (length(tidy.names.singledots) > length(unique(tidy.names.singledots))) {
        stop("make.tidy.column.names: at least one column label is repeated.")
    }
    return(tidy.names.singledots)
}
feature.label.vector = as.vector(make.tidy.column.names(inframes$feature.labels$desc))
colnames(test.and.train) <- c("subject", "activity", feature.label.vector)

# 3. Use descriptive activity names to name the activities in the data set
#   (one option: convert activity column from integer to factor with labels 
#   from activity_labels.txt). Or just add activityDescription column.
#test.and.train$activity.desc <- 
#    inframes$activity.labels$desc[as.integer(test.and.train$activity)]
test.and.train2 = cbind(test.and.train[,1:2], 
                        inframes$activity.labels$desc[as.integer(test.and.train$activity)],
                        test.and.train[,3:ncol(test.and.train)])
names(test.and.train2)[3] <- "activity.desc"

# 5. Pare down the columns to those having either "mean()" or "std()" in its original label.
# After tidying up replaces "()" with ".", this mean including
# column labels with ".mean." or ".std." or ending in "mean" or "std".
# Intentionally omitted: columns with "gravitymean" or "meanfreq". 
regex.meanorstd <- "(\\.mean\\.|\\.mean$|\\.std\\.|\\.std$)"
mean.or.std.col.indices <- grep(regex.meanorstd, feature.label.vector)
mean.or.std.col.names <- feature.label.vector[mean.or.std.col.indices]

# Keep columns "subject", "activity", and "activity.desc" as well as the mean/std columns.
collist = c("subject", "activity", "activity.desc", mean.or.std.col.names)
tidy.data.detailed <- test.and.train2[,collist]

# Documentation task (for data dictionary): write out data columns included and excluded.
write.table(collist, file="tidydata_cols_included.csv", sep=",", col.names=FALSE)
write.table(feature.label.vector[!feature.label.vector %in% collist], 
          file="tidydata_cols_excluded.csv", sep=",", col.names=FALSE)

# 6. Sort by subject, then by activity.
tidy.data.detailed.sorted <- tidy.data.detailed[
    order(tidy.data.detailed$subject, tidy.data.detailed$activity),]

# 7. Save dataset as tidydata_detailed.csv.txt
write.csv(tidy.data.detailed.sorted, file="tidydata_detailed.csv.txt", row.names=FALSE)

# 8. Using this tidy but detailed file as input, create a second, independent 
#   tidy data set with the average of each variable for each activity 
#   and each subject. 
#   - Check that the second dataset has 180 rows. Calculation: 
#       - There are 30 subjects.
#       - There are 6 activities (walking etc.)
#       - Each subject participated in every activity.
#       - There should be 180 rows of data in this tidy summary file.
#   - Save the second dataset to current working directory as "tidydata_summary.csv.txt"
#       (.txt allows file to be uploaded to Coursera)
library(data.table)
detail.dt <- data.table(tidy.data.detailed.sorted)
# Use the helpful grouping functionality of data.tables, in particular
# "lapply" combined with the ".SD" variable and the "by" sorting specifier.
# Please see http://stackoverflow.com/questions/16513827/r-summarizing-multiple-columns-with-data-table
# for discussion.
# Side-note: including activity.desc in the sequence of keys by which to sort
#   is completely unnecessary as a sort key - the first two, subject and activity,
#   are completely adequate. Including activity.desc in by is an easy way to make sure
#   the column is included in the resulting data.table.
summary.dt <- detail.dt[, lapply(.SD, mean), 
                        by=list(subject, activity, activity.desc), 
                        .SDcols=mean.or.std.col.names]
# Write the csv file with a .txt suffix to allow upload to Coursera.
# row.names=FALSE: don't include a row number column.
write.csv(summary.dt, file="tidydata_summary.csv.txt", row.names=FALSE)
