## Collect and tidy Human Activity Recognition Using Smartphones Data Set 
cleanHAR <- function()
{
    rawData <- loadHAR()
    
    rawData <- nameVariables(rawData)

    mergedData <- mergeTestAndTrain(rawData)
    
    measurements <- extractMeanAndStd(mergedData)
    
    measurementAverages <- computeAverages(mergedData)
    
    harTidy <- list(mergedData, measurementAverages)
    names(harTidy) <- c("meanAndStdDevEachMeasurement", "averageByActivityAndSubject")
    
    return(harTidy)
}

## Load the data from the UCI files into a data frame. Strip some of the extraneous columns from the text files.  
## Ensure we're saving as the correct data class (e.g., strings vs factors)
loadHAR <- function()
{
    # Confirm the raw text file directory exists, push curent directory, and move to the source data directory.
    stopifnot(file.exists("UCI HAR Dataset"))
    wd <- getwd()
    setwd("./UCI HAR Dataset")
    
    # Create variable names for labels
    activityLabels <- read.table("./activity_labels.txt")
    activityLabels <- as.character(activityLabels[,2])
    
    # Test data
    XTest <- read.table("./test/X_test.txt")
    
    yTest <- read.table("./test/y_test.txt")
    yTest[,1] <- factor(yTest[,1], labels = activityLabels, ordered = TRUE)
    
    # Training data
    XTrain <- read.table("./train/X_train.txt")
    
    yTrain <- read.table("./train/y_train.txt")
    yTrain[,1] <- factor(yTrain[,1], labels = activityLabels, ordered = TRUE)
    
    # Variable names for unlabelled data
    features <- read.table("./features.txt", stringsAsFactors=FALSE)
    features <- as.vector(features[,2])  # Retain only the names and use position the vector as index.
    
    # Subject codes
    subjectTest <- read.table("./test/subject_test.txt")
    subjectTest[,1] <- as.factor(subjectTest[,1])
    
    subjectTrain <- read.table("./train/subject_train.txt")
    subjectTrain[,1] <- as.factor(subjectTrain[,1])
    
    # Consolodate
    rawData <- list(XTest, yTest, subjectTest, XTrain, yTrain, subjectTrain, features, activityLabels)
    names(rawData) <- c("XTest", "yTest", "subjectTest", "XTrain", "yTrain", "subjectTrain", "features", "activityLabels")
    
    # Pop the directory stack
    setwd(wd)
    
    return(rawData)
}

nameVariables <- function(rawData)
{
    # Scrub the feature names of parentheses, etc.
    rawData$features <- gsub("[(,)]", "", rawData$features)
    rawData$features <- gsub("-", "", rawData$features)

    # Add the variable names to the data frames
    names(rawData$XTest) <- rawData$features
    names(rawData$XTrain) <- rawData$features
    
    names(rawData$yTest) <- "label"
    names(rawData$yTrain) <- "label"
    
    names(rawData$subjectTest) <- "subject"
    names(rawData$subjectTrain) <- "subject"
    
    return(rawData)
}

mergeTestAndTrain <- function(rawData)
{
    # Append the label columns to the data
    labeledTest <- cbind(rawData$XTest, rawData$yTest, rawData$subjectTest)
    labeledTrain <- cbind(rawData$XTrain, rawData$yTrain, rawData$subjectTrain)
    
    # Stack the test and trainin data
    testAndTrain <- rbind(labeledTest, labeledTrain)
    
    return(testAndTrain)
}

extractMeanAndStd <- function(mergedData)
{
    meanstd <- names(mergedData)[grep("mean|std", names(mergedData))]
    
    return(mergedData[, meanstd])
}

computeAverages <- function(mergedData)
{
    meanstd <- names(mergedData)[grep("mean|std", names(mergedData))]
    
    # Split-apply-combine
    measurementAverages <- ddply(mergedData[, c(meanstd, "label", "subject")], .(label, subject), colwise(mean))
    
    return(measurementAverages)
}

