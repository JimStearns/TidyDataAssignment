Code Book (aka Data Dictionary) for TidyDataAssignment
======================================================

Assignment for Johns Hopkins Data Science Coursera "Getting and Cleaning Data"
Author: Jim Stearns
Due Date: 27-Jul-2014

## Overview

A subset of the attributes of thousands of observations are grouped by subject, then by that subject's activity. The average of these attributes are calculated and stored in the summary file. This is a data dictionary of the values in that summary file.

There are 30 subjects and six activities. Each subject performed every activity.
File "tidydata_summary.csv.txt" therefore contains 180 rows of summary information,
plus the header describing the column names.

There are 561 attributes (columns) in the input test and training data.
The labels for each of these columns is contained in the file "features.txt".
66 of these attributes are determinated to be related to the mean and standard deviation.
Algorithm for inclusion: the attribute label in features.txt contains the string "mean()" or "std()". 495 attributes are excluded - not summarized in the "tidydata_summary.csv.txt".

## Map of detailed variables to summary variables (66)

| Idx | Name in features.txt | Name in tidydata_summary.csv.txt |
:---|:---------------------------------|:------------------------|
| 1 | tBodyAcc-mean()-X | tbodyacc.mean.x |
| 2 | tBodyAcc-mean()-Y | tbodyacc.mean.y |
| 3 | tBodyAcc-mean()-Z | tbodyacc.mean.z |
| 4 | tBodyAcc-std()-X | tbodyacc.std.x |
| 5 | tBodyAcc-std()-Y | tbodyacc.std.y |
| 6 | tBodyAcc-std()-Z | tbodyacc.std.z |
| 41 | tGravityAcc-mean()-X | tgravityacc.mean.x |
| 42 | tGravityAcc-mean()-Y | tgravityacc.mean.y |
| 43 | tGravityAcc-mean()-Z | tgravityacc.mean.z |
| 44 | tGravityAcc-std()-X | tgravityacc.std.x |
| 45 | tGravityAcc-std()-Y | tgravityacc.std.y |
| 46 | tGravityAcc-std()-Z | tgravityacc.std.z |
| 81 | tBodyAccJerk-mean()-X | tbodyaccjerk.mean.x |
| 82 | tBodyAccJerk-mean()-Y | tbodyaccjerk.mean.y |
| 83 | tBodyAccJerk-mean()-Z | tbodyaccjerk.mean.z |
| 84 | tBodyAccJerk-std()-X | tbodyaccjerk.std.x |
| 85 | tBodyAccJerk-std()-Y | tbodyaccjerk.std.y |
| 86 | tBodyAccJerk-std()-Z | tbodyaccjerk.std.z |
| 121 | tBodyGyro-mean()-X | tbodygyro.mean.x |
| 122 | tBodyGyro-mean()-Y | tbodygyro.mean.y |
| 123 | tBodyGyro-mean()-Z | tbodygyro.mean.z |
| 124 | tBodyGyro-std()-X | tbodygyro.std.x |
| 125 | tBodyGyro-std()-Y | tbodygyro.std.y |
| 126 | tBodyGyro-std()-Z | tbodygyro.std.z |
| 161 | tBodyGyroJerk-mean()-X | tbodygyrojerk.mean.x |
| 162 | tBodyGyroJerk-mean()-Y | tbodygyrojerk.mean.y |
| 163 | tBodyGyroJerk-mean()-Z | tbodygyrojerk.mean.z |
| 164 | tBodyGyroJerk-std()-X | tbodygyrojerk.std.x |
| 165 | tBodyGyroJerk-std()-Y | tbodygyrojerk.std.y |
| 166 | tBodyGyroJerk-std()-Z | tbodygyrojerk.std.z |
| 201 | tBodyAccMag-mean() | tbodyaccmag.mean |
| 202 | tBodyAccMag-std() | tbodyaccmag.std |
| 214 | tGravityAccMag-mean() | tgravityaccmag.mean |
| 215 | tGravityAccMag-std() | tgravityaccmag.std |
| 227 | tBodyAccJerkMag-mean() | tbodyaccjerkmag.mean |
| 228 | tBodyAccJerkMag-std() | tbodyaccjerkmag.std |
| 240 | tBodyGyroMag-mean() | tbodygyromag.mean |
| 241 | tBodyGyroMag-std() | tbodygyromag.std |
| 253 | tBodyGyroJerkMag-mean() | tbodygyrojerkmag.mean |
| 254 | tBodyGyroJerkMag-std() | tbodygyrojerkmag.std |
| 266 | fBodyAcc-mean()-X | fbodyacc.mean.x |
| 267 | fBodyAcc-mean()-Y | fbodyacc.mean.y |
| 268 | fBodyAcc-mean()-Z | fbodyacc.mean.z |
| 269 | fBodyAcc-std()-X | fbodyacc.std.x |
| 270 | fBodyAcc-std()-Y | fbodyacc.std.y |
| 271 | fBodyAcc-std()-Z | fbodyacc.std.z |
| 345 | fBodyAccJerk-mean()-X | fbodyaccjerk.mean.x |
| 346 | fBodyAccJerk-mean()-Y | fbodyaccjerk.mean.y |
| 347 | fBodyAccJerk-mean()-Z | fbodyaccjerk.mean.z |
| 348 | fBodyAccJerk-std()-X | fbodyaccjerk.std.x |
| 349 | fBodyAccJerk-std()-Y | fbodyaccjerk.std.y |
| 350 | fBodyAccJerk-std()-Z | fbodyaccjerk.std.z |
| 424 | fBodyGyro-mean()-X | fbodygyro.mean.x |
| 425 | fBodyGyro-mean()-Y | fbodygyro.mean.y |
| 426 | fBodyGyro-mean()-Z | fbodygyro.mean.z |
| 427 | fBodyGyro-std()-X | fbodygyro.std.x |
| 428 | fBodyGyro-std()-Y | fbodygyro.std.y |
| 429 | fBodyGyro-std()-Z | fbodygyro.std.z |
| 503 | fBodyAccMag-mean() | fbodyaccmag.mean |
| 504 | fBodyAccMag-std() | fbodyaccmag.std |
| 516 | fBodyBodyAccJerkMag-mean() | fbodybodyaccjerkmag.mean |
| 517 | fBodyBodyAccJerkMag-std() | fbodybodyaccjerkmag.std |
| 529 | fBodyBodyGyroMag-mean() | fbodybodygyromag.mean |
| 530 | fBodyBodyGyroMag-std() | fbodybodygyromag.std |
| 542 | fBodyBodyGyroJerkMag-mean() | fbodybodygyrojerkmag.mean |
| 543 | fBodyBodyGyroJerkMag-std() | fbodybodygyrojerkmag.std |

## Notes

* Variables chosen: "mean()" or "std()" are in features.txt label name. 
* This has the effect of not including gravityMean or 
tBody{Acc,AccJerk, Gyro, GyroJerk}Mean. 
    * Justification: any choice is somewhat arbitrary, but the attributes with mean() and std() appear to be a measure of the attribute itself while meanFreg and the angle means are measures of an attribute of the attribute.

# List of Excluded Attributes in features.txt (495)

| Idx | Name in features.txt |
:---|:---------------------------------|
| 7 | tBodyAcc-mad()-X |
| 8 | tBodyAcc-mad()-Y |
| 9 | tBodyAcc-mad()-Z |
| 10 | tBodyAcc-max()-X |
| 11 | tBodyAcc-max()-Y |
| 12 | tBodyAcc-max()-Z |
| 13 | tBodyAcc-min()-X |
| 14 | tBodyAcc-min()-Y |
| 15 | tBodyAcc-min()-Z |
| 16 | tBodyAcc-sma() |
| 17 | tBodyAcc-energy()-X |
| 18 | tBodyAcc-energy()-Y |
| 19 | tBodyAcc-energy()-Z |
| 20 | tBodyAcc-iqr()-X |
| 21 | tBodyAcc-iqr()-Y |
| 22 | tBodyAcc-iqr()-Z |
| 23 | tBodyAcc-entropy()-X |
| 24 | tBodyAcc-entropy()-Y |
| 25 | tBodyAcc-entropy()-Z |
| 26 | tBodyAcc-arCoeff()-X,1 |
| 27 | tBodyAcc-arCoeff()-X,2 |
| 28 | tBodyAcc-arCoeff()-X,3 |
| 29 | tBodyAcc-arCoeff()-X,4 |
| 30 | tBodyAcc-arCoeff()-Y,1 |
| 31 | tBodyAcc-arCoeff()-Y,2 |
| 32 | tBodyAcc-arCoeff()-Y,3 |
| 33 | tBodyAcc-arCoeff()-Y,4 |
| 34 | tBodyAcc-arCoeff()-Z,1 |
| 35 | tBodyAcc-arCoeff()-Z,2 |
| 36 | tBodyAcc-arCoeff()-Z,3 |
| 37 | tBodyAcc-arCoeff()-Z,4 |
| 38 | tBodyAcc-correlation()-X,Y |
| 39 | tBodyAcc-correlation()-X,Z |
| 40 | tBodyAcc-correlation()-Y,Z |
| 47 | tGravityAcc-mad()-X |
| 48 | tGravityAcc-mad()-Y |
| 49 | tGravityAcc-mad()-Z |
| 50 | tGravityAcc-max()-X |
| 51 | tGravityAcc-max()-Y |
| 52 | tGravityAcc-max()-Z |
| 53 | tGravityAcc-min()-X |
| 54 | tGravityAcc-min()-Y |
| 55 | tGravityAcc-min()-Z |
| 56 | tGravityAcc-sma() |
| 57 | tGravityAcc-energy()-X |
| 58 | tGravityAcc-energy()-Y |
| 59 | tGravityAcc-energy()-Z |
| 60 | tGravityAcc-iqr()-X |
| 61 | tGravityAcc-iqr()-Y |
| 62 | tGravityAcc-iqr()-Z |
| 63 | tGravityAcc-entropy()-X |
| 64 | tGravityAcc-entropy()-Y |
| 65 | tGravityAcc-entropy()-Z |
| 66 | tGravityAcc-arCoeff()-X,1 |
| 67 | tGravityAcc-arCoeff()-X,2 |
| 68 | tGravityAcc-arCoeff()-X,3 |
| 69 | tGravityAcc-arCoeff()-X,4 |
| 70 | tGravityAcc-arCoeff()-Y,1 |
| 71 | tGravityAcc-arCoeff()-Y,2 |
| 72 | tGravityAcc-arCoeff()-Y,3 |
| 73 | tGravityAcc-arCoeff()-Y,4 |
| 74 | tGravityAcc-arCoeff()-Z,1 |
| 75 | tGravityAcc-arCoeff()-Z,2 |
| 76 | tGravityAcc-arCoeff()-Z,3 |
| 77 | tGravityAcc-arCoeff()-Z,4 |
| 78 | tGravityAcc-correlation()-X,Y |
| 79 | tGravityAcc-correlation()-X,Z |
| 80 | tGravityAcc-correlation()-Y,Z |
| 87 | tBodyAccJerk-mad()-X |
| 88 | tBodyAccJerk-mad()-Y |
| 89 | tBodyAccJerk-mad()-Z |
| 90 | tBodyAccJerk-max()-X |
| 91 | tBodyAccJerk-max()-Y |
| 92 | tBodyAccJerk-max()-Z |
| 93 | tBodyAccJerk-min()-X |
| 94 | tBodyAccJerk-min()-Y |
| 95 | tBodyAccJerk-min()-Z |
| 96 | tBodyAccJerk-sma() |
| 97 | tBodyAccJerk-energy()-X |
| 98 | tBodyAccJerk-energy()-Y |
| 99 | tBodyAccJerk-energy()-Z |
| 100 | tBodyAccJerk-iqr()-X |
| 101 | tBodyAccJerk-iqr()-Y |
| 102 | tBodyAccJerk-iqr()-Z |
| 103 | tBodyAccJerk-entropy()-X |
| 104 | tBodyAccJerk-entropy()-Y |
| 105 | tBodyAccJerk-entropy()-Z |
| 106 | tBodyAccJerk-arCoeff()-X,1 |
| 107 | tBodyAccJerk-arCoeff()-X,2 |
| 108 | tBodyAccJerk-arCoeff()-X,3 |
| 109 | tBodyAccJerk-arCoeff()-X,4 |
| 110 | tBodyAccJerk-arCoeff()-Y,1 |
| 111 | tBodyAccJerk-arCoeff()-Y,2 |
| 112 | tBodyAccJerk-arCoeff()-Y,3 |
| 113 | tBodyAccJerk-arCoeff()-Y,4 |
| 114 | tBodyAccJerk-arCoeff()-Z,1 |
| 115 | tBodyAccJerk-arCoeff()-Z,2 |
| 116 | tBodyAccJerk-arCoeff()-Z,3 |
| 117 | tBodyAccJerk-arCoeff()-Z,4 |
| 118 | tBodyAccJerk-correlation()-X,Y |
| 119 | tBodyAccJerk-correlation()-X,Z |
| 120 | tBodyAccJerk-correlation()-Y,Z |
| 127 | tBodyGyro-mad()-X |
| 128 | tBodyGyro-mad()-Y |
| 129 | tBodyGyro-mad()-Z |
| 130 | tBodyGyro-max()-X |
| 131 | tBodyGyro-max()-Y |
| 132 | tBodyGyro-max()-Z |
| 133 | tBodyGyro-min()-X |
| 134 | tBodyGyro-min()-Y |
| 135 | tBodyGyro-min()-Z |
| 136 | tBodyGyro-sma() |
| 137 | tBodyGyro-energy()-X |
| 138 | tBodyGyro-energy()-Y |
| 139 | tBodyGyro-energy()-Z |
| 140 | tBodyGyro-iqr()-X |
| 141 | tBodyGyro-iqr()-Y |
| 142 | tBodyGyro-iqr()-Z |
| 143 | tBodyGyro-entropy()-X |
| 144 | tBodyGyro-entropy()-Y |
| 145 | tBodyGyro-entropy()-Z |
| 146 | tBodyGyro-arCoeff()-X,1 |
| 147 | tBodyGyro-arCoeff()-X,2 |
| 148 | tBodyGyro-arCoeff()-X,3 |
| 149 | tBodyGyro-arCoeff()-X,4 |
| 150 | tBodyGyro-arCoeff()-Y,1 |
| 151 | tBodyGyro-arCoeff()-Y,2 |
| 152 | tBodyGyro-arCoeff()-Y,3 |
| 153 | tBodyGyro-arCoeff()-Y,4 |
| 154 | tBodyGyro-arCoeff()-Z,1 |
| 155 | tBodyGyro-arCoeff()-Z,2 |
| 156 | tBodyGyro-arCoeff()-Z,3 |
| 157 | tBodyGyro-arCoeff()-Z,4 |
| 158 | tBodyGyro-correlation()-X,Y |
| 159 | tBodyGyro-correlation()-X,Z |
| 160 | tBodyGyro-correlation()-Y,Z |
| 167 | tBodyGyroJerk-mad()-X |
| 168 | tBodyGyroJerk-mad()-Y |
| 169 | tBodyGyroJerk-mad()-Z |
| 170 | tBodyGyroJerk-max()-X |
| 171 | tBodyGyroJerk-max()-Y |
| 172 | tBodyGyroJerk-max()-Z |
| 173 | tBodyGyroJerk-min()-X |
| 174 | tBodyGyroJerk-min()-Y |
| 175 | tBodyGyroJerk-min()-Z |
| 176 | tBodyGyroJerk-sma() |
| 177 | tBodyGyroJerk-energy()-X |
| 178 | tBodyGyroJerk-energy()-Y |
| 179 | tBodyGyroJerk-energy()-Z |
| 180 | tBodyGyroJerk-iqr()-X |
| 181 | tBodyGyroJerk-iqr()-Y |
| 182 | tBodyGyroJerk-iqr()-Z |
| 183 | tBodyGyroJerk-entropy()-X |
| 184 | tBodyGyroJerk-entropy()-Y |
| 185 | tBodyGyroJerk-entropy()-Z |
| 186 | tBodyGyroJerk-arCoeff()-X,1 |
| 187 | tBodyGyroJerk-arCoeff()-X,2 |
| 188 | tBodyGyroJerk-arCoeff()-X,3 |
| 189 | tBodyGyroJerk-arCoeff()-X,4 |
| 190 | tBodyGyroJerk-arCoeff()-Y,1 |
| 191 | tBodyGyroJerk-arCoeff()-Y,2 |
| 192 | tBodyGyroJerk-arCoeff()-Y,3 |
| 193 | tBodyGyroJerk-arCoeff()-Y,4 |
| 194 | tBodyGyroJerk-arCoeff()-Z,1 |
| 195 | tBodyGyroJerk-arCoeff()-Z,2 |
| 196 | tBodyGyroJerk-arCoeff()-Z,3 |
| 197 | tBodyGyroJerk-arCoeff()-Z,4 |
| 198 | tBodyGyroJerk-correlation()-X,Y |
| 199 | tBodyGyroJerk-correlation()-X,Z |
| 200 | tBodyGyroJerk-correlation()-Y,Z |
| 203 | tBodyAccMag-mad() |
| 204 | tBodyAccMag-max() |
| 205 | tBodyAccMag-min() |
| 206 | tBodyAccMag-sma() |
| 207 | tBodyAccMag-energy() |
| 208 | tBodyAccMag-iqr() |
| 209 | tBodyAccMag-entropy() |
| 210 | tBodyAccMag-arCoeff()1 |
| 211 | tBodyAccMag-arCoeff()2 |
| 212 | tBodyAccMag-arCoeff()3 |
| 213 | tBodyAccMag-arCoeff()4 |
| 216 | tGravityAccMag-mad() |
| 217 | tGravityAccMag-max() |
| 218 | tGravityAccMag-min() |
| 219 | tGravityAccMag-sma() |
| 220 | tGravityAccMag-energy() |
| 221 | tGravityAccMag-iqr() |
| 222 | tGravityAccMag-entropy() |
| 223 | tGravityAccMag-arCoeff()1 |
| 224 | tGravityAccMag-arCoeff()2 |
| 225 | tGravityAccMag-arCoeff()3 |
| 226 | tGravityAccMag-arCoeff()4 |
| 229 | tBodyAccJerkMag-mad() |
| 230 | tBodyAccJerkMag-max() |
| 231 | tBodyAccJerkMag-min() |
| 232 | tBodyAccJerkMag-sma() |
| 233 | tBodyAccJerkMag-energy() |
| 234 | tBodyAccJerkMag-iqr() |
| 235 | tBodyAccJerkMag-entropy() |
| 236 | tBodyAccJerkMag-arCoeff()1 |
| 237 | tBodyAccJerkMag-arCoeff()2 |
| 238 | tBodyAccJerkMag-arCoeff()3 |
| 239 | tBodyAccJerkMag-arCoeff()4 |
| 242 | tBodyGyroMag-mad() |
| 243 | tBodyGyroMag-max() |
| 244 | tBodyGyroMag-min() |
| 245 | tBodyGyroMag-sma() |
| 246 | tBodyGyroMag-energy() |
| 247 | tBodyGyroMag-iqr() |
| 248 | tBodyGyroMag-entropy() |
| 249 | tBodyGyroMag-arCoeff()1 |
| 250 | tBodyGyroMag-arCoeff()2 |
| 251 | tBodyGyroMag-arCoeff()3 |
| 252 | tBodyGyroMag-arCoeff()4 |
| 255 | tBodyGyroJerkMag-mad() |
| 256 | tBodyGyroJerkMag-max() |
| 257 | tBodyGyroJerkMag-min() |
| 258 | tBodyGyroJerkMag-sma() |
| 259 | tBodyGyroJerkMag-energy() |
| 260 | tBodyGyroJerkMag-iqr() |
| 261 | tBodyGyroJerkMag-entropy() |
| 262 | tBodyGyroJerkMag-arCoeff()1 |
| 263 | tBodyGyroJerkMag-arCoeff()2 |
| 264 | tBodyGyroJerkMag-arCoeff()3 |
| 265 | tBodyGyroJerkMag-arCoeff()4 |
| 272 | fBodyAcc-mad()-X |
| 273 | fBodyAcc-mad()-Y |
| 274 | fBodyAcc-mad()-Z |
| 275 | fBodyAcc-max()-X |
| 276 | fBodyAcc-max()-Y |
| 277 | fBodyAcc-max()-Z |
| 278 | fBodyAcc-min()-X |
| 279 | fBodyAcc-min()-Y |
| 280 | fBodyAcc-min()-Z |
| 281 | fBodyAcc-sma() |
| 282 | fBodyAcc-energy()-X |
| 283 | fBodyAcc-energy()-Y |
| 284 | fBodyAcc-energy()-Z |
| 285 | fBodyAcc-iqr()-X |
| 286 | fBodyAcc-iqr()-Y |
| 287 | fBodyAcc-iqr()-Z |
| 288 | fBodyAcc-entropy()-X |
| 289 | fBodyAcc-entropy()-Y |
| 290 | fBodyAcc-entropy()-Z |
| 291 | fBodyAcc-maxInds-X |
| 292 | fBodyAcc-maxInds-Y |
| 293 | fBodyAcc-maxInds-Z |
| 294 | fBodyAcc-meanFreq()-X |
| 295 | fBodyAcc-meanFreq()-Y |
| 296 | fBodyAcc-meanFreq()-Z |
| 297 | fBodyAcc-skewness()-X |
| 298 | fBodyAcc-kurtosis()-X |
| 299 | fBodyAcc-skewness()-Y |
| 300 | fBodyAcc-kurtosis()-Y |
| 301 | fBodyAcc-skewness()-Z |
| 302 | fBodyAcc-kurtosis()-Z |
| 303 | fBodyAcc-bandsEnergy()-1,8 |
| 304 | fBodyAcc-bandsEnergy()-9,16 |
| 305 | fBodyAcc-bandsEnergy()-17,24 |
| 306 | fBodyAcc-bandsEnergy()-25,32 |
| 307 | fBodyAcc-bandsEnergy()-33,40 |
| 308 | fBodyAcc-bandsEnergy()-41,48 |
| 309 | fBodyAcc-bandsEnergy()-49,56 |
| 310 | fBodyAcc-bandsEnergy()-57,64 |
| 311 | fBodyAcc-bandsEnergy()-1,16 |
| 312 | fBodyAcc-bandsEnergy()-17,32 |
| 313 | fBodyAcc-bandsEnergy()-33,48 |
| 314 | fBodyAcc-bandsEnergy()-49,64 |
| 315 | fBodyAcc-bandsEnergy()-1,24 |
| 316 | fBodyAcc-bandsEnergy()-25,48 |
| 317 | fBodyAcc-bandsEnergy()-1,8 |
| 318 | fBodyAcc-bandsEnergy()-9,16 |
| 319 | fBodyAcc-bandsEnergy()-17,24 |
| 320 | fBodyAcc-bandsEnergy()-25,32 |
| 321 | fBodyAcc-bandsEnergy()-33,40 |
| 322 | fBodyAcc-bandsEnergy()-41,48 |
| 323 | fBodyAcc-bandsEnergy()-49,56 |
| 324 | fBodyAcc-bandsEnergy()-57,64 |
| 325 | fBodyAcc-bandsEnergy()-1,16 |
| 326 | fBodyAcc-bandsEnergy()-17,32 |
| 327 | fBodyAcc-bandsEnergy()-33,48 |
| 328 | fBodyAcc-bandsEnergy()-49,64 |
| 329 | fBodyAcc-bandsEnergy()-1,24 |
| 330 | fBodyAcc-bandsEnergy()-25,48 |
| 331 | fBodyAcc-bandsEnergy()-1,8 |
| 332 | fBodyAcc-bandsEnergy()-9,16 |
| 333 | fBodyAcc-bandsEnergy()-17,24 |
| 334 | fBodyAcc-bandsEnergy()-25,32 |
| 335 | fBodyAcc-bandsEnergy()-33,40 |
| 336 | fBodyAcc-bandsEnergy()-41,48 |
| 337 | fBodyAcc-bandsEnergy()-49,56 |
| 338 | fBodyAcc-bandsEnergy()-57,64 |
| 339 | fBodyAcc-bandsEnergy()-1,16 |
| 340 | fBodyAcc-bandsEnergy()-17,32 |
| 341 | fBodyAcc-bandsEnergy()-33,48 |
| 342 | fBodyAcc-bandsEnergy()-49,64 |
| 343 | fBodyAcc-bandsEnergy()-1,24 |
| 344 | fBodyAcc-bandsEnergy()-25,48 |
| 351 | fBodyAccJerk-mad()-X |
| 352 | fBodyAccJerk-mad()-Y |
| 353 | fBodyAccJerk-mad()-Z |
| 354 | fBodyAccJerk-max()-X |
| 355 | fBodyAccJerk-max()-Y |
| 356 | fBodyAccJerk-max()-Z |
| 357 | fBodyAccJerk-min()-X |
| 358 | fBodyAccJerk-min()-Y |
| 359 | fBodyAccJerk-min()-Z |
| 360 | fBodyAccJerk-sma() |
| 361 | fBodyAccJerk-energy()-X |
| 362 | fBodyAccJerk-energy()-Y |
| 363 | fBodyAccJerk-energy()-Z |
| 364 | fBodyAccJerk-iqr()-X |
| 365 | fBodyAccJerk-iqr()-Y |
| 366 | fBodyAccJerk-iqr()-Z |
| 367 | fBodyAccJerk-entropy()-X |
| 368 | fBodyAccJerk-entropy()-Y |
| 369 | fBodyAccJerk-entropy()-Z |
| 370 | fBodyAccJerk-maxInds-X |
| 371 | fBodyAccJerk-maxInds-Y |
| 372 | fBodyAccJerk-maxInds-Z |
| 373 | fBodyAccJerk-meanFreq()-X |
| 374 | fBodyAccJerk-meanFreq()-Y |
| 375 | fBodyAccJerk-meanFreq()-Z |
| 376 | fBodyAccJerk-skewness()-X |
| 377 | fBodyAccJerk-kurtosis()-X |
| 378 | fBodyAccJerk-skewness()-Y |
| 379 | fBodyAccJerk-kurtosis()-Y |
| 380 | fBodyAccJerk-skewness()-Z |
| 381 | fBodyAccJerk-kurtosis()-Z |
| 382 | fBodyAccJerk-bandsEnergy()-1,8 |
| 383 | fBodyAccJerk-bandsEnergy()-9,16 |
| 384 | fBodyAccJerk-bandsEnergy()-17,24 |
| 385 | fBodyAccJerk-bandsEnergy()-25,32 |
| 386 | fBodyAccJerk-bandsEnergy()-33,40 |
| 387 | fBodyAccJerk-bandsEnergy()-41,48 |
| 388 | fBodyAccJerk-bandsEnergy()-49,56 |
| 389 | fBodyAccJerk-bandsEnergy()-57,64 |
| 390 | fBodyAccJerk-bandsEnergy()-1,16 |
| 391 | fBodyAccJerk-bandsEnergy()-17,32 |
| 392 | fBodyAccJerk-bandsEnergy()-33,48 |
| 393 | fBodyAccJerk-bandsEnergy()-49,64 |
| 394 | fBodyAccJerk-bandsEnergy()-1,24 |
| 395 | fBodyAccJerk-bandsEnergy()-25,48 |
| 396 | fBodyAccJerk-bandsEnergy()-1,8 |
| 397 | fBodyAccJerk-bandsEnergy()-9,16 |
| 398 | fBodyAccJerk-bandsEnergy()-17,24 |
| 399 | fBodyAccJerk-bandsEnergy()-25,32 |
| 400 | fBodyAccJerk-bandsEnergy()-33,40 |
| 401 | fBodyAccJerk-bandsEnergy()-41,48 |
| 402 | fBodyAccJerk-bandsEnergy()-49,56 |
| 403 | fBodyAccJerk-bandsEnergy()-57,64 |
| 404 | fBodyAccJerk-bandsEnergy()-1,16 |
| 405 | fBodyAccJerk-bandsEnergy()-17,32 |
| 406 | fBodyAccJerk-bandsEnergy()-33,48 |
| 407 | fBodyAccJerk-bandsEnergy()-49,64 |
| 408 | fBodyAccJerk-bandsEnergy()-1,24 |
| 409 | fBodyAccJerk-bandsEnergy()-25,48 |
| 410 | fBodyAccJerk-bandsEnergy()-1,8 |
| 411 | fBodyAccJerk-bandsEnergy()-9,16 |
| 412 | fBodyAccJerk-bandsEnergy()-17,24 |
| 413 | fBodyAccJerk-bandsEnergy()-25,32 |
| 414 | fBodyAccJerk-bandsEnergy()-33,40 |
| 415 | fBodyAccJerk-bandsEnergy()-41,48 |
| 416 | fBodyAccJerk-bandsEnergy()-49,56 |
| 417 | fBodyAccJerk-bandsEnergy()-57,64 |
| 418 | fBodyAccJerk-bandsEnergy()-1,16 |
| 419 | fBodyAccJerk-bandsEnergy()-17,32 |
| 420 | fBodyAccJerk-bandsEnergy()-33,48 |
| 421 | fBodyAccJerk-bandsEnergy()-49,64 |
| 422 | fBodyAccJerk-bandsEnergy()-1,24 |
| 423 | fBodyAccJerk-bandsEnergy()-25,48 |
| 430 | fBodyGyro-mad()-X |
| 431 | fBodyGyro-mad()-Y |
| 432 | fBodyGyro-mad()-Z |
| 433 | fBodyGyro-max()-X |
| 434 | fBodyGyro-max()-Y |
| 435 | fBodyGyro-max()-Z |
| 436 | fBodyGyro-min()-X |
| 437 | fBodyGyro-min()-Y |
| 438 | fBodyGyro-min()-Z |
| 439 | fBodyGyro-sma() |
| 440 | fBodyGyro-energy()-X |
| 441 | fBodyGyro-energy()-Y |
| 442 | fBodyGyro-energy()-Z |
| 443 | fBodyGyro-iqr()-X |
| 444 | fBodyGyro-iqr()-Y |
| 445 | fBodyGyro-iqr()-Z |
| 446 | fBodyGyro-entropy()-X |
| 447 | fBodyGyro-entropy()-Y |
| 448 | fBodyGyro-entropy()-Z |
| 449 | fBodyGyro-maxInds-X |
| 450 | fBodyGyro-maxInds-Y |
| 451 | fBodyGyro-maxInds-Z |
| 452 | fBodyGyro-meanFreq()-X |
| 453 | fBodyGyro-meanFreq()-Y |
| 454 | fBodyGyro-meanFreq()-Z |
| 455 | fBodyGyro-skewness()-X |
| 456 | fBodyGyro-kurtosis()-X |
| 457 | fBodyGyro-skewness()-Y |
| 458 | fBodyGyro-kurtosis()-Y |
| 459 | fBodyGyro-skewness()-Z |
| 460 | fBodyGyro-kurtosis()-Z |
| 461 | fBodyGyro-bandsEnergy()-1,8 |
| 462 | fBodyGyro-bandsEnergy()-9,16 |
| 463 | fBodyGyro-bandsEnergy()-17,24 |
| 464 | fBodyGyro-bandsEnergy()-25,32 |
| 465 | fBodyGyro-bandsEnergy()-33,40 |
| 466 | fBodyGyro-bandsEnergy()-41,48 |
| 467 | fBodyGyro-bandsEnergy()-49,56 |
| 468 | fBodyGyro-bandsEnergy()-57,64 |
| 469 | fBodyGyro-bandsEnergy()-1,16 |
| 470 | fBodyGyro-bandsEnergy()-17,32 |
| 471 | fBodyGyro-bandsEnergy()-33,48 |
| 472 | fBodyGyro-bandsEnergy()-49,64 |
| 473 | fBodyGyro-bandsEnergy()-1,24 |
| 474 | fBodyGyro-bandsEnergy()-25,48 |
| 475 | fBodyGyro-bandsEnergy()-1,8 |
| 476 | fBodyGyro-bandsEnergy()-9,16 |
| 477 | fBodyGyro-bandsEnergy()-17,24 |
| 478 | fBodyGyro-bandsEnergy()-25,32 |
| 479 | fBodyGyro-bandsEnergy()-33,40 |
| 480 | fBodyGyro-bandsEnergy()-41,48 |
| 481 | fBodyGyro-bandsEnergy()-49,56 |
| 482 | fBodyGyro-bandsEnergy()-57,64 |
| 483 | fBodyGyro-bandsEnergy()-1,16 |
| 484 | fBodyGyro-bandsEnergy()-17,32 |
| 485 | fBodyGyro-bandsEnergy()-33,48 |
| 486 | fBodyGyro-bandsEnergy()-49,64 |
| 487 | fBodyGyro-bandsEnergy()-1,24 |
| 488 | fBodyGyro-bandsEnergy()-25,48 |
| 489 | fBodyGyro-bandsEnergy()-1,8 |
| 490 | fBodyGyro-bandsEnergy()-9,16 |
| 491 | fBodyGyro-bandsEnergy()-17,24 |
| 492 | fBodyGyro-bandsEnergy()-25,32 |
| 493 | fBodyGyro-bandsEnergy()-33,40 |
| 494 | fBodyGyro-bandsEnergy()-41,48 |
| 495 | fBodyGyro-bandsEnergy()-49,56 |
| 496 | fBodyGyro-bandsEnergy()-57,64 |
| 497 | fBodyGyro-bandsEnergy()-1,16 |
| 498 | fBodyGyro-bandsEnergy()-17,32 |
| 499 | fBodyGyro-bandsEnergy()-33,48 |
| 500 | fBodyGyro-bandsEnergy()-49,64 |
| 501 | fBodyGyro-bandsEnergy()-1,24 |
| 502 | fBodyGyro-bandsEnergy()-25,48 |
| 505 | fBodyAccMag-mad() |
| 506 | fBodyAccMag-max() |
| 507 | fBodyAccMag-min() |
| 508 | fBodyAccMag-sma() |
| 509 | fBodyAccMag-energy() |
| 510 | fBodyAccMag-iqr() |
| 511 | fBodyAccMag-entropy() |
| 512 | fBodyAccMag-maxInds |
| 513 | fBodyAccMag-meanFreq() |
| 514 | fBodyAccMag-skewness() |
| 515 | fBodyAccMag-kurtosis() |
| 518 | fBodyBodyAccJerkMag-mad() |
| 519 | fBodyBodyAccJerkMag-max() |
| 520 | fBodyBodyAccJerkMag-min() |
| 521 | fBodyBodyAccJerkMag-sma() |
| 522 | fBodyBodyAccJerkMag-energy() |
| 523 | fBodyBodyAccJerkMag-iqr() |
| 524 | fBodyBodyAccJerkMag-entropy() |
| 525 | fBodyBodyAccJerkMag-maxInds |
| 526 | fBodyBodyAccJerkMag-meanFreq() |
| 527 | fBodyBodyAccJerkMag-skewness() |
| 528 | fBodyBodyAccJerkMag-kurtosis() |
| 531 | fBodyBodyGyroMag-mad() |
| 532 | fBodyBodyGyroMag-max() |
| 533 | fBodyBodyGyroMag-min() |
| 534 | fBodyBodyGyroMag-sma() |
| 535 | fBodyBodyGyroMag-energy() |
| 536 | fBodyBodyGyroMag-iqr() |
| 537 | fBodyBodyGyroMag-entropy() |
| 538 | fBodyBodyGyroMag-maxInds |
| 539 | fBodyBodyGyroMag-meanFreq() |
| 540 | fBodyBodyGyroMag-skewness() |
| 541 | fBodyBodyGyroMag-kurtosis() |
| 544 | fBodyBodyGyroJerkMag-mad() |
| 545 | fBodyBodyGyroJerkMag-max() |
| 546 | fBodyBodyGyroJerkMag-min() |
| 547 | fBodyBodyGyroJerkMag-sma() |
| 548 | fBodyBodyGyroJerkMag-energy() |
| 549 | fBodyBodyGyroJerkMag-iqr() |
| 550 | fBodyBodyGyroJerkMag-entropy() |
| 551 | fBodyBodyGyroJerkMag-maxInds |
| 552 | fBodyBodyGyroJerkMag-meanFreq() |
| 553 | fBodyBodyGyroJerkMag-skewness() |
| 554 | fBodyBodyGyroJerkMag-kurtosis() |
| 555 | angle(tBodyAccMean,gravity) |
| 556 | angle(tBodyAccJerkMean),gravityMean) |
| 557 | angle(tBodyGyroMean,gravityMean) |
| 558 | angle(tBodyGyroJerkMean,gravityMean) |
| 559 | angle(X,gravityMean) |
| 560 | angle(Y,gravityMean) |
| 561 | angle(Z,gravityMean) |







