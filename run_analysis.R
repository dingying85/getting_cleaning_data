###getting and cleaning data
#setwd("/home/dingying85/Desktop/coursera/getting_and_cleaning_data/UCI HAR Dataset")
training_data=read.table("train/X_train.txt")
training_subject=read.delim("train/subject_train.txt",header=FALSE)
training_activity=read.delim("train/y_train.txt",header=FALSE)
testing_data=read.table("test/X_test.txt")
testing_subject=read.delim("test/subject_test.txt",header=FALSE)
testing_activity=read.delim("test/y_test.txt",header=FALSE)
#Merges the training and the test sets to create one data set.
full_data=rbind(training_data,testing_data)
#Extracts only the measurements on the mean and standard deviation for each measurement.

activity_names=read.table("features.txt")




index_select1=grep("mean()",activity_names[,2])
index_select2=grep("std()",activity_names[,2])
index_select=c(index_select1,index_select2)
#Uses descriptive activity names to name the activities in the data set
full_data1=full_data[,index_select]
colnames(full_data1)=activity_names[index_select,2]
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

sample_label=c(training_subject[[1]],testing_subject[[1]])
activity_label=c(training_activity[[1]],testing_activity[[1]])

combine_label=table(sample_label,activity_label)

sample_num=dim(combine_label)[1]
activity_num=dim(combine_label)[2]

x1=rep(1:sample_num,each=activity_num)
x2=rep(1:activity_num,sample_num)

combine_label=cbind(x1,x2)

mean_activity=t(sapply(1:(sample_num*activity_num), function(x) colMeans(full_data1[which(sample_label==combine_label[x,1]&activity_label==combine_label[x,2]),])))


summary=cbind(combine_label,mean_activity)
colnames(summary)[1:2]=c("samplename","activity")

activity_name=read.table("activity_labels.txt")[,2]
x_name=rep(activity_name,each=activity_num)

summary[,2]=x_name

colname_parse=gsub("[()]","",tolower(colnames(summary)))
colnames(summary)=colname_parse


write.table(summary,file="tidy.txt",row.names=FALSE,quote=FALSE,sep="\t")

#write.table(colnames(summary),file="README.md",row.names=FALSE,quote=FALSE,sep="\t")
