
#1. Merges the training and the test sets to create one data set

xtestdata    <- read.table("./test/x_test.txt") 
ytestdata    <- read.table("./test/y_test.txt")         
testsubjects <- read.table("./test/subject_test.txt")   
# Train data
xtraindata    <- read.table("./train/x_train.txt")       
ytraindata    <- read.table("./train/y_train.txt")       
trainsubjects <- read.table("./train/subject_train.txt") 
allActivityData <- rbind(xtestdata,xtraindata)
allLabelSets    <- rbind(ytestdata,ytraindata)
allSubjectCodes <- rbind(testsubjects,trainsubjects)


#2. Extracts only the measurements on the mean and standard deviation for each measurement.

featurenames   <- read.table("./features.txt")
meanandstddevfeatures  <- grepl("(-std\\(\\)|-mean\\(\\))",featurenames$V2)
filteredActivityData <- allActivityData[, which(meanandstddevfeatures == TRUE)]

#3. Uses descriptive activity names to name the activities in the data set.

activityLabels  <- read.table("./activity_labels.txt")
activity <- as.factor(allLabelSets$V1)
levels(activity) <- activityLabels$V2
subject <- as.factor(allSubjectCodes$V1)
filteredActivityData <- cbind(subject,activity,filteredActivityData)

#4. Appropriately label the data set with descriptive variable names.

filteredfeatures <- (cbind(featurenames,meanandstddevfeatures)[meanandstddevfeatures==TRUE,])$V2
cleaner <- function(featurename) {

    tolower(gsub("(\\(|\\)|\\-)","",featurename))

}

filteredfeatures <- sapply(filteredfeatures,cleaner)
names(filteredActivityData)[3:ncol(filteredActivityData)] <- filteredfeatures
write.csv(filteredActivityData,file="dataset1.csv")

write.table(filteredActivityData, "dataset1.txt", sep="\t")

#5. 
library(reshape2)
molten <- melt(filteredActivityData,id.vars=c("subject","activity"))
tidy <- dcast(molten,subject + activity ~ variable,mean)
write.table(tidy, "dataset2.txt", sep="\t")
