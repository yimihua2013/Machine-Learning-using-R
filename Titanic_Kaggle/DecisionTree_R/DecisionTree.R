# load data
train <- read.csv("~/Desktop/Study/Data_science/Projects/DataScienceProjects/Titanic(Kaggle)/data/train.csv")
test <- read.csv("~/Desktop/Study/Data_science/Projects/DataScienceProjects/Titanic(Kaggle)/data/test.csv")# check the data

### (1)Decision Tree###
# rpart for ‘Recursive Partitioning and Regression Trees’ 
library(rpart)
fit01 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
text(fit)
# fancy plot
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
#prediction
Prediction <- predict(fit, test, type = "class")
kaggle.sub4<-data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(kaggle.sub4,"kaggle04.csv",row.names=F)

###(2)data cleaning & feature engineering###
# combine train and test data
test$Survived<-NA
combi<-rbind(train,test)
combi$Name<-as.character(combi$Name)

## Title variable: get title from Name## 
combi$Title<-sapply(combi$Name, FUN=function(x){strsplit(x,"[,.]")[[1]][[2]]})
#strip off those spaces from the beginning of the titles
combi$Title<-sub(" ","",combi$Title)
# reduce the title redundancy
combi$Title[combi$Title %in% c('Mme','Mlle')]<-'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

combi$Title <- as.factor(combi$Title)
table(combi$Title)

## FamilySize variable: define family size##
combi$FamilySize<-combi$SibSp+combi$Parch+1
summary(combi$FamilySize)

##(3)Decision tree based on feature engineering##
trainData<-combi[1:891,]
testData<-combi[892:1309,]
fit02<-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
    Embarked + Title + FamilySize,data=trainData,method="class")

fancyRpartPlot(fit02)
#prediction
Pred02 <- predict(fit02, testData, type = "class")
kaggle.sub5<-data.frame(PassengerId = test$PassengerId, Survived = Pred02)
write.csv(kaggle.sub5,"kaggle05.csv",row.names=F)

###(4)decison tree predictor defactor###
# remove "SibSp", "Parch", "Embarked"
fit03<-rpart(Survived ~ Pclass + Sex + Age + Fare + Title +
                 FamilySize,data=trainData,method="class")
fancyRpartPlot(fit03)
#prediction
Pred03 <- predict(fit03, testData, type = "class")
kaggle.sub6<-data.frame(PassengerId = test$PassengerId, Survived = Pred03)
write.csv(kaggle.sub6,"kaggle06.csv",row.names=F)