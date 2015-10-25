#-----------------------------
## Load packages and Set path.
#-----------------------------

require(knitr)
require(markdown)
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
path <- getwd()
print(path)

#--------------
## Get the data
#--------------

# Download the file and put it in the Data folder
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path, f))

# Unzip the file
executable <- file.path("C:", "Program Files (x86)", "7-Zip", "7z.exe")
parameters <- "x"
cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path, f), "\""))
system(cmd)

# The archive put the files in a folder named UCI HAR Dataset. Set this folder as the input path.
pathInput <- file.path(path, "UCI HAR Dataset")
list.files(pathInput, recursive=TRUE)

#----------------
## Read the files
#----------------

# Read the subject files
dtSubjectTrain <- fread(file.path(pathInput, "train", "subject_train.txt"))
dtSubjectTest  <- fread(file.path(pathInput, "test" , "subject_test.txt" ))

# Read the activity_labels files 
dtActivityTrain <- fread(file.path(pathInput, "train", "Y_train.txt"))
dtActivityTest  <- fread(file.path(pathInput, "test" , "Y_test.txt" ))

# Read the data files

fileToDataTable <- function (f) {
        df <- read.table(f)
        dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(pathInput, "train", "X_train.txt"))
dtTest  <- fileToDataTable(file.path(pathInput, "test" , "X_test.txt" ))

#--------------------------------------------------------------------------
## Project-Q1: Merges the training and the test sets to create one data set
#--------------------------------------------------------------------------

# Mutate the data tables (Merges the training and the test sets)
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

# Merge the columns in the data tables and set the key
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)
setkey(dt, subject, activityNum)

#--------------------------------------------------------------------------------------------------
## Project-Q2: Extracts only the measurements on the mean & standard deviation for each measurement
#--------------------------------------------------------------------------------------------------

# From features.tx, one gets which variables in dt are measurements for the mean & standard deviation
dtFeatures <- fread(file.path(pathInput, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

# Subset only measurements for the mean and standard deviation
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

# Convert the column numbers to a vector of variable names matching columns in dt
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

# Subset the variables using variable names
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]

#------------------------------------------------------------------------------------
## Project-Q3: Uses descriptive activity names to name the activities in the data set
#------------------------------------------------------------------------------------

# From activity_labels.txt file, one can add descriptive names to the activities
dtActivityNames <- fread(file.path(pathInput, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

#-------------------------------------------------------------------------------
## Project-Q4: Appropriately labels the data set with descriptive variable names 
#-------------------------------------------------------------------------------

# Merge activity labels
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)

# Add activityName as a key
setkey(dt, subject, activityNum, activityName)

# Melt the data table to reshape it from a short and wide format to a tall and narrow format
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

# Merge activity name
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

# Create a new variable, activity that is equivalent to activityName as a factor class
# Create a new variable, feature that is equivalent to featureName as a factor class
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

# Seperate features from featureName using the helper function grepthis
grepthis <- function (regex) {
        grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

# Check: to make sure all possible combinations of feature are accounted for
# by all possible combinations of the factor class variables.

r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

#--------------------------------------------------------------------------------------
## Project-Q5: From the data set in step 4, creates a second, independent tidy data set
##             with the average of each variable for each activity and each subject
#--------------------------------------------------------------------------------------

# Create a data set with the average of each variable for each activity and each subject.
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]
print(dtTidy)
write.table(dtTidy, file="DatasetHARUS.txt", quote=FALSE, sep="\t", row.name=FALSE)


# Make codebook.
library(knitr)
knit("makeCodebook.Rmd", output = "codebook.md", encoding = "ISO8859-1", quiet = TRUE)
markdownToHTML("codebook.md", "codebook.html")



