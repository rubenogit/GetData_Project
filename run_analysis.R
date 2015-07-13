
#Download and unzip the data file
address <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
address <- sub("^https", "http", address)
zipname <- "UCI HAR Dataset.zip"
download.file(address,zipname)
unzip(zipname)

#housekeeping - remove the zip as it is no longer needed
file.remove("UCI HAR Dataset.zip")
#housekeeping
rm(address, zipname)

ucidir <- "UCI HAR Dataset"

#read the data file with features
feature_file <- normalizePath(paste(getwd(),ucidir,"features.txt",sep="/"))
feature_data <- read.table(feature_file, sep=" ", header=FALSE, stringsAsFactors=FALSE)
features <- feature_data$V2

#housekeeping
rm(feature_file, feature_data)

#vector of the features that contain mean or std
#I don't include the angles as these are not estimate variables of the feature vector
selected_features <- grep("mean\\(\\)|std\\(\\)", features)

#function to clean the names of the selected features
cleannames <- function(f) {

    f <- gsub("\\-mean\\(\\)\\-X", "X 9", f)
    f <- gsub("\\-std\\(\\)\\-X",  "X 8", f)
    f <- gsub("\\-mean\\(\\)\\-Y", "Y 9", f)
    f <- gsub("\\-std\\(\\)\\-Y",  "Y 8", f)
    f <- gsub("\\-mean\\(\\)\\-Z", "Z 9", f)
    f <- gsub("\\-std\\(\\)\\-Z",  "Z 8", f)
    f <- gsub("\\-mean\\(\\)", "9", f)
    f <- gsub("\\-std\\(\\)",  "8", f)
    f <- gsub("\\-", " ", f)

    f <- gsub("tBody",    "06", f)
    f <- gsub("tGravity", "07", f)
    f <- gsub("fBody",    "16", f)
    f <- gsub("fGravity", "17", f)

    f <- gsub("Acc",  "2", f)
    f <- gsub("Gyro", "3", f)
    f <- gsub("Jerk", "4", f)
    f <- gsub("Mag",  "5", f)

    f <- gsub("0", "Time domain signal: ", f)
    f <- gsub("1", "Frequency domain signal: ", f)

    f <- gsub("66", "Body vs Body", f)
    f <- gsub("6", "Body", f)
    f <- gsub("7", "Gravity", f)

    f <- sub("2", " Linear Acceleration ", f)
    f <- sub("3", " Angular Velocity ", f)
    f <- sub("4", "Jerk ", f)
    f <- sub("5", "Magnitude ", f)

    f <- sub("X", "X-direction", f)
    f <- sub("Y", "Y-direction", f)
    f <- sub("Z", "Z-direction", f)


    f <- gsub("9", "(mean)", f)
    f <- gsub("8", "(standard deviation)", f)
}


#these are now the clean names
feature_names <- cleannames(features[selected_features])

#housekeeping
rm(cleannames)

#columns are a fixed width of 16
#construct a vector with columns to select in two steps:
# 1) select non of the columns by default (use -16 to skip)
# 2) change the default for selected features (use 16 to include)
selected_columns <- rep(-16, length(features))
selected_columns[selected_features] <- 16

#create the file urls for the data sets
test_x_url  <- normalizePath(paste(ucidir, "test", "X_test.txt", sep="/"))
train_x_url <- normalizePath(paste(ucidir, "train", "X_train.txt", sep="/"))

#read test and train data from test and train directories
#read only the selected columns
test_x  <- read.fwf(test_x_url,  selected_columns)
train_x <- read.fwf(train_x_url, selected_columns)

#housekeeping
rm(features, selected_features, selected_columns, test_x_url, train_x_url)

#change the column names to something more readable
colnames(test_x) <- feature_names
colnames(train_x) <- feature_names

#add column to show what dataset the data is from
test_x$Set <- "test"
train_x$Set <- "train"


#create the file urls for subjects
subject_test_url  <- normalizePath(paste(ucidir, "test",  "subject_test.txt", sep="/"))
subject_train_url <- normalizePath(paste(ucidir, "train", "subject_train.txt", sep="/"))

#read the subjects
test_subject  <- read.table(subject_test_url,  header=FALSE)$V1
train_subject <- read.table(subject_train_url, header=FALSE)$V1

#add columns to the test and train datasets
test_x$Subject  <- test_subject
train_x$Subject <- train_subject

#housekeeping
rm(subject_test_url, subject_train_url, test_subject, train_subject)


#read activity labels
activity_labels_url <- normalizePath(paste(ucidir, "activity_labels.txt", sep="/"))
activity_labels_data <- read.table(activity_labels_url , sep=" ", header=FALSE, stringsAsFactors=FALSE)
activity_labels <- activity_labels_data$V2

test_act_url  <- normalizePath(paste(ucidir, "test", "y_test.txt", sep="/"))
train_act_url <- normalizePath(paste(ucidir, "train", "y_train.txt", sep="/"))
test_activity  <- read.table(test_act_url,  header=FALSE)$V1
train_activity <- read.table(train_act_url, header=FALSE)$V1

test_x$Activity  <- activity_labels[test_activity]
train_x$Activity <- activity_labels[train_activity]

#housekeeping
rm(ucidir, activity_labels_url, activity_labels_data, activity_labels, test_act_url, train_act_url, test_activity, train_activity)


col_order <- c("Activity","Subject",feature_names,"Set")

x <- rbind(test_x[col_order],train_x[col_order])

#housekeeping
rm(test_x, train_x, col_order)

#mean per Activity and Subject
mean_x <- aggregate(x[,feature_names], list(x[,"Activity"],x[,"Subject"]),mean)
colnames(mean_x)[1:2] <- c("Activity","Subject")
write.table(mean_x, "ucimean.txt", row.name=FALSE) 