### load data###
train <- read.csv("/Users/yimizhao/Desktop/Study/Data_science/Projects/DataScienceProjects/titanic/data/train.csv")
test <- read.csv("/Users/yimizhao/Desktop/Study/Data_science/Projects/DataScienceProjects/titanic/data/test.csv")

###Data cleaning ###
##(1)train data cleaning##
#1) data preparing
# remove"Passengerid",Ticket","Fare", "Cabin" and "Embarked"
mydata<-train[,-c(1,9:12)]
str(mydata)
mydata$Survived<-factor(mydata$Survived)
mydata$Pclass<-factor(mydata$Pclass)
mydata$Name<-as.character(mydata$Name)

#replacing "Sex" variable with a Dummy Variable (male=0,female=1)
mydata$Sex=gsub("female",1,mydata$Sex)
mydata$Sex=gsub("male",0,mydata$Sex)
mydata$Sex<-factor(mydata$Sex)
head(mydata)

#2) Dealing with missing "Age" values
master_vector<-grep("Master.", mydata$Name, fixed=T)
miss_vector<-grep("Miss.", mydata$Name, fixed=T)
mrs_vector<-grep("Mrs.", mydata$Name, fixed=T)
mr_vector<-grep("Mr.", mydata$Name, fixed=T)
dr_vector<-grep("Dr.", mydata$Name, fixed=T)

#simplify "Name" variable
for(i in master_vector) {
    mydata$Name[i] = "Master"
}
for(i in miss_vector) {
    mydata$Name[i] = "Miss"
}
for(i in mrs_vector) {
    mydata$Name[i] = "Mrs"
}
for(i in mr_vector) {
    mydata$Name[i] = "Mr"
}
for(i in dr_vector) {
    mydata$Name[i] = "Dr"
}
head(mydata)
# making inference on missing age values: inputting title-group averages
# fisrt, calculate average age for each group
master_age = round(mean(mydata$Age[mydata$Name == "Master"], na.rm = T), digits = 2)
miss_age = round(mean(mydata$Age[mydata$Name == "Miss"], na.rm = T), digits =2)
mrs_age = round(mean(mydata$Age[mydata$Name == "Mrs"], na.rm = T), digits = 2)
mr_age = round(mean(mydata$Age[mydata$Name == "Mr"], na.rm = T), digits = 2)
dr_age = round(mean(mydata$Age[mydata$Name == "Dr"], na.rm = T), digits = 2)
# second, make inference on missing age values
for (i in 1:nrow(mydata)) {
    if (is.na(mydata[i,5])) {
        if (mydata$Name[i] == "Master") {
            mydata$Age[i] = master_age
        } else if (mydata$Name[i] == "Miss") {
            mydata$Age[i] = miss_age
        } else if (mydata$Name[i] == "Mrs") {
            mydata$Age[i] = mrs_age
        } else if (mydata$Name[i] == "Mr") {
            mydata$Age[i] = mr_age
        } else if (mydata$Name[i] == "Dr") {
            mydata$Age[i] = dr_age
        } else {
            print("Uncaught Title")
        }
    }
}
#check data
summary(mydata$Age)

#3)Feature engineering: creating new variables to strengthen our model
# define "Child" variable where age<12
mydata['Child']=NA
for (i in 1:nrow(mydata)) {
    if (mydata$Age[i] <= 12) {
        mydata$Child[i] = 1
    } else {
        mydata$Child[i] = 0
    }
}
head(mydata)
# define "Family" variable where 
# family size=# of Siblings/Spouses + # of Parents/Children
# add 1 so minimum family size becomes 1
mydata['Family']=NA
for(i in 1:nrow(mydata)) {
    x = mydata$SibSp[i]
    y = mydata$Parch[i]
    mydata$Family[i] = x + y + 1
}

# define variable "Mother"
# with title "Mrs" and value of Parch greater than 0
mydata['Mother']=NA
for(i in 1:nrow(mydata)) {
    if(mydata$Name[i] == "Mrs" & mydata$Parch[i] > 0) {
        mydata$Mother[i] = 1
    } else {
        mydata$Mother[i] = 0
    }
}
head(mydata)
##(2) test data cleaning##
# similar process as train data
PassengerId = test[1]
testData = test[-c(1, 8:11)]
testData$Pclass<-factor(testData$Pclass)

testData$Sex = gsub("female", 1, testData$Sex)
testData$Sex = gsub("male", 0, testData$Sex)
testData$Name=as.character(testData$Name)

test_master_vector = grep("Master.",testData$Name)
test_miss_vector = grep("Miss.", testData$Name)
test_mrs_vector = grep("Mrs.", testData$Name)
test_mr_vector = grep("Mr.", testData$Name)
test_dr_vector = grep("Dr.", testData$Name)

for(i in test_master_vector) {
    testData$Name[i] = "Master"
}
for(i in test_miss_vector) {
    testData$Name[i] = "Miss"
}
for(i in test_mrs_vector) {
    testData$Name[i] = "Mrs"
}
for(i in test_mr_vector) {
    testData$Name[i] = "Mr"
}
for(i in test_dr_vector) {
    testData$Name[i] = "Dr"
}

test_master_age = round(mean(testData$Age[testData$Name == "Master"], na.rm = TRUE), digits = 2)
test_miss_age = round(mean(testData$Age[testData$Name == "Miss"], na.rm = TRUE), digits =2)
test_mrs_age = round(mean(testData$Age[testData$Name == "Mrs"], na.rm = TRUE), digits = 2)
test_mr_age = round(mean(testData$Age[testData$Name == "Mr"], na.rm = TRUE), digits = 2)
test_dr_age = round(mean(testData$Age[testData$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(testData)) {
    if (is.na(testData[i,4])) {
        if (testData$Name[i] == "Master") {
            testData$Age[i] <- test_master_age
        } else if (testData$Name[i] == "Miss") {
            testData$Age[i] <- test_miss_age
        } else if (testData$Name[i] == "Mrs") {
            testData$Age[i] <- test_mrs_age
        } else if (testData$Name[i]== "Mr") {
            testData$Age[i] <- test_mr_age
        } else if (testData$Name[i] == "Dr") {
            testData$Age[i] <- test_dr_age
        } else {
            print(paste("Uncaught Title at: ", i, sep=""))
            print(paste("The Title unrecognized was: ", testData[i,2], sep=""))
        }
    }
}

#We do a manual replacement here, because we weren't able to programmatically figure out the title.
#We figured out it was 89 because the above print statement should have warned us.
testData[89, 4] = test_miss_age

testData["Child"] = NA

for (i in 1:nrow(testData)) {
    if (testData$Age[i] <= 12) {
        testData$Child[i]= 1
    } else {
        testData$Child[i] = 0
    }
}

testData["Family"] = NA

for(i in 1:nrow(testData)) {
    testData$Family[i] = testData$SibSp[i] + testData$Parch[i] + 1
}

testData["Mother"] = NA

for(i in 1:nrow(testData)) {
    if(testData$Name[i] == "Mrs" & testData$Parch[i] > 0) {
        testData$Mother[i] = 1
    } else {
        testData$Mother[i] = 0
    }
}

### Model Fitting(Logistic Regression)###
# model 1
md_logit1 <- glm(Survived ~ Pclass+Sex+Age+Child+Family+Mother,data=mydata,family="binomial")
summary(md_logit1)
# prediction
p.hats1<-predict.glm(md_logit1,testData,type="response")
survival<-vector()
for(i in 1:length(p.hats1)){
    if(p.hats1[i]>.5){
        survival[i]=1
    } else{
        survival[i]=0
    }
}
# CSV for kaggle submission
kaggle.sub1<-cbind(PassengerId,survival)
colnames(kaggle.sub1)<-c("PassengerId","Survived")
write.csv(kaggle.sub1,file="kaggle01.csv", row.names=F)

# model 2(add interaction Sex*Pclass )
md_logit2<- glm(Survived ~ Pclass+Sex+Age+Child+Family+Mother+Sex*Pclass,data=mydata,family="binomial")
summary(md_logit2)
# prediction
p.hats2<-predict.glm(md_logit2,testData,type="response")
survival<-vector()
for(i in 1:length(p.hats2)){
    if(p.hats1[i]>.5){
        survival[i]=1
    } else{
        survival[i]=0
    }
}
# CSV for kaggle submission
kaggle.sub2<-cbind(PassengerId,survival)
colnames(kaggle.sub2)<-c("PassengerId","Survived")
write.csv(kaggle.sub2,file="kaggle02.csv", row.names=F)


