Codebook

Codebook was generated on r as.character(Sys.time()) during the same process that generated the dataset. See run_analysis.md or run_analysis.html for details on dataset creation.
Variable list and descriptions
Variable name 	Description
subject 	ID the subject who performed the activity for each window sample. Its range is from 1 to 30.
activity 	Activity name
featDomain 	Feature: Time domain signal or frequency domain signal (Time or Freq)
featInstrument 	Feature: Measuring instrument (Accelerometer or Gyroscope)
featAcceleration 	Feature: Acceleration signal (Body or Gravity)
featVariable 	Feature: Variable (Mean or SD)
featJerk 	Feature: Jerk signal
featMagnitude 	Feature: Magnitude of the signals calculated using the Euclidean norm
featAxis 	Feature: 3-axial signals in the X, Y and Z directions (X, Y, or Z)
featCount 	Feature: Count of data points used to compute average
featAverage 	Feature: Average of each variable for each activity and each subject
Dataset structure

str(dtTidy)

List the key variables in the data table

key(dtTidy)

Show a few rows of the dataset

dtTidy

Summary of variables

summary(dtTidy)

List all possible combinations of features

dtTidy[, .N, by=c(names(dtTidy)[grep("^feat", names(dtTidy))])]

Save to file

Save data table objects to a tab-delimited text file "DatasetHARUS.txt""

write.table(dtTidy, file="DatasetHARUS.txt", quote=FALSE, sep="\t", row.name=FALSE)
