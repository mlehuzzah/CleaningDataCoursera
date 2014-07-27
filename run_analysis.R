

path1<-"~/R/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset"
features<-read.table(paste(path1,"/features.txt",sep=""), sep=' ')

meanVars<-grep("mean\\(\\)", features[,2])
#pull out all entries of "features" which contain mean()

stdVars<-grep("std\\(\\)", features[,2])
#pull out all entries of "features" which contain mean()

vars2Keep<-c(meanVars,stdVars)
#vector corresponding to the entries of features which have mean or std

featuresKeep<-features[vars2Keep,2]
#names of the desired features

activities<-read.table(paste(path1,"/activity_labels.txt",sep=""), sep=" ")


testActivity<-read.table(paste(path1,"/test/y_test.txt",sep=""), sep=' ')
testSubject<-read.table(paste(path1,"/test/subject_test.txt",sep=""), sep=' ')
testData<-read.table(paste(path1,"/test/X_test.txt",sep=""))

testDataKeep<-testData[,vars2Keep]
#subset of testData corresponding to the variables we want

colnames(testDataKeep)<-featuresKeep
# rename the column names to the features

testAct<-merge(testActivity,activities,"V1",all=TRUE,sort=FALSE)
#shitty way to turn the activity numbers into words... should have figures out how to do this with factors

testTidy<-cbind(testSubject,testAct[,2],testDataKeep)
colnames(testTidy)[2]<-"activity"
colnames(testTidy)[1]<-"subject"


trainActivity<-read.table(paste(path1,"/train/y_train.txt",sep=""), sep=' ')
trainSubject<-read.table(paste(path1,"/train/subject_train.txt",sep=""), sep=' ')
trainData<-read.table(paste(path1,"/train/X_train.txt",sep=""))
trainDataKeep<-trainData[,vars2Keep]
colnames(trainDataKeep)<-featuresKeep
trainAct<-merge(trainActivity,activities,"V1",all=TRUE,sort=FALSE)
trainTidy<-cbind(trainSubject,trainAct[,2],trainDataKeep)
colnames(trainTidy)[2]<-"activity"
colnames(trainTidy)[1]<-"subject"


tidyData<-rbind(testTidy,trainTidy)

write.table(tidyData, "~/R/Assign3_dataset1.txt", sep=" ") 




# The following builds an data frame of NAs whose columns match that of tidyData, and with every subject/activity combination
tidyData3 <- as.data.frame(matrix(NA, ncol = 2, nrow = 180))
colnames(tidyData3)<-c("subject","activity")
#populates the first two cols of the data frame with all combos of subject/activity
for (j in  1:30 ) {
  for (k in  1:6 ) {
    tidyData3[6*(j-1)+k,1]<-j
    tidyData3[6*(j-1)+k,2]<-as.character(activities[k,2])
  }
}


for(k in 3:68){
  a<-aggregate(tidyData[,k],list(subject=tidyData[,1],activity=tidyData[,2]),mean)
  tidyData3<-merge(tidyData3,a,by=c("subject","activity"),all=TRUE)
  colnames(tidyData3)[k]<-colnames(tidyData)[k]
  
}

write.table(tidyData3, "~/R/Assign3_dataset2.txt", sep=" ") 
