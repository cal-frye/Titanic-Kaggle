#Load raw data & packages
train <- read.csv("train.csv", header = T)
test <- read.csv("test.csv", header = T)

library(ggplot2)
library(stringr)
library(randomForest)


#=================================================================
#Data & feature engineering

test.survived <- data.frame(PassengerId = test[,1], Survived = rep("None", nrow(test)), test[,-1])
data.combined <- rbind(train, test.survived)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$SibSp <- as.factor(data.combined$SibSp)
data.combined$Parch <- as.factor(data.combined$Parch)

extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

Titles <- NULL
for (i in 1:nrow(data.combined)) {
  Titles <- c(Titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$Title <- as.factor(Titles)

temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$Family.size <- as.factor(temp.sibsp + temp.parch + 1)


#=================================================================
#Exploratory modeling

#Train a random forest with default parameters, using pclass & title
rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = T, ntree = 1000)
rf.1
varImpPlot(rf.1)


#Train a random forest using pclass, title, & sibsp
rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = T, ntree = 1000)
rf.2
varImpPlot(rf.2)

#Train a random forest using pclass, title, & parch
rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = T, ntree = 1000)
rf.3
varImpPlot(rf.3)

#Train a random forest using pclass, title, sibsp, & parch
rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = T, ntree = 1000)
rf.4
varImpPlot(rf.4)

#Try using family size variable instead of sibsp and parch
rf.train.5 <- data.combined[1:891, c("Pclass", "Title", "Family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = T, ntree = 1000)
rf.5
varImpPlot(rf.5)
