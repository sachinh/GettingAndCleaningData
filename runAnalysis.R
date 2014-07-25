## Declare String Literals for messages and such
## Can help with Localization needs, if required

##
## Function Name: runAnalysis
## Parameters   : None
## Purpose      : TBD
## Developer    : Sachin Holla
##

runAnalysis <- function() {

        # first read the common data files
        features <- read.csv("features.txt",header=FALSE, sep="")
        actLabels <- read.csv("activity_labels.txt",header=FALSE, sep="")
        
        ## start reading in all the test data
        testX <- read.csv("test/X_test.txt",header=FALSE, sep="")
        testY <- read.csv("test//y_test.txt",header=FALSE)
        subjTest <- read.csv("test//subject_test.txt",header=FALSE)

        # assign column names and retain only the 'mean' and 'std' relevant columns
        names(testX) <- features[,2]        
        df <- testX[,grep("*-mean\\(*|*-std\\(*", features[,2])]
        df <- df[,grep("*-meanFreq\\(*", names(df),invert=TRUE)]
        df <- df[,grep("*BodyBody*", names(df),ignore.case=FALSE, invert=TRUE)]

        # now add in the subjectID and also the activity labels
        df <- cbind(df,subjTest)
        df <- cbind(df,c("activity"))
        names(df)[61:62] <- c("subjectID", "activity")
        df[,62] <- actLabels[testY$V1, "V2"]
        
        ## start reading in all the training data
        trainX <- read.csv("train/X_train.txt",header=FALSE, sep="")
        trainY <- read.csv("train//y_train.txt",header=FALSE)
        subjTrain <- read.csv("train//subject_train.txt",header=FALSE)

        # assign column names and retain only the 'mean' and 'std' relevant columns
        names(trainX) <- features[,2]
        df2 <- trainX[,grep("*-mean\\(*|*-std\\(*", features[,2])]
        df2 <- df2[,grep("*-meanFreq\\(*", names(df2),invert=TRUE)]
        df2 <- df2[,grep("*BodyBody*", names(df2),ignore.case=FALSE, invert=TRUE)]
        
        # now add in the subjectID and also the activity labels
        df2 <- cbind(df2,subjTrain)
        df2 <- cbind(df2,c("activity"))
        names(df2)[61:62] <- c("subjectID", "activity")
        df2[,62] <- actLabels[trainY$V1, "V2"]

        # now add the test and training data together
        allData <- rbind(df, df2)
        
        # now make the columns more descriptive
        # by removing the () and the - and capitalizing "mean" and std -> StdDev
        names(allData) <- gsub("\\(\\)","",names(allData))
        names(allData) <- gsub("\\-","",names(allData))
        names(allData) <- gsub("mean","Mean",names(allData))
        names(allData) <- gsub("std","StdDev",names(allData))
        
        # now create the tidy data set
        # first move the subjectID and activity to the front of the data.frame
        allData <- cbind(allData[,61:62], allData[,1:60])
        # then create the averages by leveraging the aggregate
        tidyData <- aggregate(allData, by=list(allData$subjectID,allData$activity),FUN=mean, na.rm=TRUE)

        # do some cleanup due to the way that the aggregate fn works
        names(tidyData)[1:2] <- names(tidyData)[3:4]
        tidyData <- cbind(tidyData[,1:2], tidyData[,5:ncol(tidyData)])

        # now write out this dataset into a file
        write.table(tidyData, "TidyDataSet.txt", sep=" ", row.names=FALSE)
}
