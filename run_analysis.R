# Getting and Cleaning Data
# run_analysis.R
# Ying-Yu Hsieh

# Move this file to <directory_of_

#   1. Merges the training and the test sets to create one data set.
##  1.1 read and setup all datasets
### 1.1.1 read feature list from features.txt
    df_feature <- read.table("features.txt")
### 1.1.2 read activity labels from a activity_labels.txt
    df_activity <- read.table("activity_labels.txt")
### 1.1.3 read test datasets from ./test/X_test.txt and ./test/y_test.txt
    df_test_subject <- read.table("./test/subject_test.txt")
    df_test_x <- read.table("./test/X_test.txt")
    df_test_y <- read.table("./test/y_test.txt")
### 1.1.4 read train datasets
    df_train_subject <- read.table("./train/subject_train.txt")
    df_train_x <- read.table("./train/X_train.txt")
    df_train_y <- read.table("./train/y_train.txt")
##  1.2. setup column names of datasets
### 1.2.1 setup feature columns
    names(df_feature) <- c("fea.id","fea.name")
### 1.2.2 setup activity columns
    names(df_activity) <- c("act.id","act.name")
### 1.2.3 setup columns of test (each of the dataset contains 2947 rows)
    names(df_test_subject) <- c("sub.id")
    names(df_test_x) <- df_feature[,2]
    names(df_test_y) <- c("act.id")
### 1.2.4 setup columns of train (each of the dataset contains 7352 rows)
    names(df_train_subject) <- c("sub.id")
    names(df_train_x) <- df_feature[,2]
    names(df_train_y) <- c("act.id")
##  1.3 add three columns - subject id (sub.id), activity id (act.id) and data
##      source (tag)
    df_test_x <- cbind(df_test_subject,df_test_y,tag=rep("test",
                nrow(df_test_x)), df_test_x)
    df_train_x <- cbind(df_train_subject,df_train_y,tag=rep("train",
                nrow(df_train_x)),df_train_x)
##  1.4 merge test (tag="test") & train dataset (tag="train")
    df_merge <- rbind(df_test_x,df_train_x)
    
##############################################
# ANS 1: The merged dataset is df_merge
##############################################
    
#   2. Extracts only the measurements on the mean and standard deviation for 
#      each measurement
##  2.1 create vector with column 1~3 (sub.id,act.id,tag) and columns with 
##      name containing mean and std
    idx <- c(1:3,grep("mean|std", names(df_merge)))
##  2.2 create dataset with the columns - sub.id, act.id, tag and columns of 
##      mean and std of measurements
    df_extract <- df_merge[,idx]
    
##############################################
# ANS 2: The extract dataset is df_extract
##############################################
    
    
#   3. Uses descriptive activity names to name the activities in the data set
    library(plyr)
##  3.1 get act.id and act.name for each row
    desc <- join(data.frame(act.id=df_extract[,2]),df_activity)
##  3.2 creat dataset and replace value of act.id (2nd column) by act.name
    df_act_desc <- df_extract
    names(df_act_desc)[2] <- "act.name"
    df_act_desc$act.name <- desc[,2]
    
##############################################
# ANS 3: The dataset with descriptive 
#        activity names is df_act_desc
##############################################
    
    
#   4. Appropriately labels the data set with descriptive variable names

##############################################
# ANS 4: Replace column names with descriptive 
#        names is done at step 1.2.3 & 1.2.4
##############################################

#   5. Creates a second, independent tidy data set with the average of each
#      variable for each activity and each subject
#      Note:
#          1) use the extracted dataset
#          2) ignore data source (since the instruction does not mention)
##  5.1 remove tag (data source)
    tmp <- df_extract
    tmp$tag <- NULL
##  5.2 use ddply to group (sub.id, act.id) and apply colMeans
    df_average <- ddply(tmp, .(sub.id, act.id), .fun=function(x){ colMeans(x[,-c(1:2)]) })
    rm(tmp)
##  5.3 add "AVG." in front of the name of each column
    names(df_average)[3:length(names(df_average))] <- paste("AVG",names(df_average)[3:length(names(df_average))],sep=".")
##  5.4 write file for project upload
    write.table(df_average, 'project-tidydata.txt',row.names=TRUE,sep='\t');
    ## write.csv(df_average, 'project-tidydata.csv', row.names=T);
    
##############################################
# ANS 5: The tidy dataset with the average of
#        each variable for each activity and 
#        subject is df_average
#        And save result as:
#            project-tidydata.txt
##############################################
    
    
