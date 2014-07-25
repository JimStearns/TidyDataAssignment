TidyDataAssignment
==================

Assignment for Johns Hopkins Data Science Coursera "Getting and Cleaning Data"

## From the instructors:

> The purpose of this project is to demonstrate your ability to collect, 
> work with, and clean a data set.
> The goal is to prepare tidy data that can be used for later analysis. 

> ... 

> You will be required to submit:

> 1) a tidy data set as described below,

> 2) a link to a Github repository containing:

>>   a) your script "run_analysis.R" for performing the analysis,
>>   b) a code book called "CodeBook.md"" that describes the variables, the data,
and any transformations or work that you performed to clean up the data.
>>   c) a README.md (this file) that explains how all of the scripts work and how they are connected.  

## Required Input Files:

The following input data files should be in the working directory when run_analysis.R
is executed:

1.  train/X_train.txt:
2.  train/y_train.txt:
3.  train/subject_train.txt: a number 1:30 to specify the subject. Intentionally (to provide subject anonymity), no additional subject information is provided.
4.  test/X_test.txt:
5.  test/y_test.txt:
6.  test/subject_test.txt: as in subject_train, a number 1:30 to specify the subject. 
7.  activity_labels.txt: Text descriptions for the six activities (WALKING etc). Use this file to map from 1:6 used in y_train.txt and y_test.txt to the appropriate description.
8.  features.txt: Text descriptions of the 561 measurement columns in the X_test and X_train data sets.

Here is a summary of how the input files fit together:

* The test and train datasets have the same width (561). They will be row-bound (merged vertically)
* The X_train, y_train, and subject_train datasets have the same number of rows. They will be column-bound (merged horizontally).
* The X_test, y_test datasets have the same number of rows. They will be column-bound (merged horizontally) using R's cbind method.

Here's a summary of the output files:

1.  README.md: this MarkDown file providing overview of input, processing, and output.
2.  tidydata_summary.csv.txt: A summary of tidy data. Uploaded to Coursera but also included in the repo. CSV format even though ".txt" was appended as well in order to simplify upload to Coursera. Contains 180 rows of summary data, 30 subjects each performing each of six activities. Header row included. 
3.  run_analysis.R: R script that takes the 8 input files and produces the tidydata_summary output file.
4.  CodeBook.md: a data dictionary describing the rows, columns and cell values in tidydata_summary.
