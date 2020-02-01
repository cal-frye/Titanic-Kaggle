#Load raw data & packages
train <- read.csv("train.csv", header = T)
test <- read.csv("test.csv", header = T)

library(ggplot2)
library(stringr)


#Prep data for analysis
##Add a "Survived" variable to test data
test.survived <- data.frame(PassengerId = test[,1], Survived = rep("None", nrow(test)), test[,-1])

##Combine test and train data sets
data.combined <- rbind(train, test.survived)

##Understand and update data types
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)


#Get to know the data
##Gross survival rates
table(data.combined$Survived)

##Class
table(data.combined$Pclass)

###Hypothesis: Upper class passengers survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived)))+
  geom_bar(width = 0.5)+
  xlab("Passenger Class")+
  ylab("Total Count")+
  labs(fill = "Survived")+
  ggtitle("Passenger Survival by Class")

##Name
###Examine the names in the training data set
head(as.character(train$Name))

length(unique(as.character(data.combined$Name)))

###Inspect duplicate names
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
data.combined[which(data.combined$Name %in% dup.names),]

##Explore the title within names
misses <- data.combined[which(str_detect(data.combined$Name, "Miss")),]
misses[1:5,]

###Hypothesis: Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

###Investigate males
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]


#Probe relationship between 'Survived' and 'Pclass'
##Create new 'Title' variable
###Create a utility function to assist title extraction
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

##Explore relationship between title and survival
##Since we only have survived labels for the train set, only use the first 891 rows
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived))+
  geom_bar(width = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Passenger Survival by Title & Class")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill = "Survived")


#Explore relationship between sex and survival/other variables
##Distribution of sex across data set
table(data.combined$Sex)

##Visualize 3-way relationship of sex, pclass, and survival
ggplot(data.combined[1:891,], aes(x= Sex, fill = Survived))+
  geom_bar(width = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Passenger Survival by Sex & Class")+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill = "Survived")


#Explore AGE across data set
summary(data.combined$Age)
summary(data.combined[1:891, "Age"])

##Survival by sex, pclass, and age
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived))+
  facet_wrap(~Sex + Pclass)+
  geom_histogram(binwidth = 10)+
  xlab("Age")+
  ylab("Total Count")+
  ggtitle("Passenger Survival by Sex, Class, & Age")


#Validate "Master" as a proxy for male children
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)


#"Miss." is more complicated, so let's examine further
misses <- data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x= Age, fill = Survived))+
  facet_wrap(~Pclass)+
  geom_histogram(binwidth = 5)+
  ggtitle("Age for Miss. by Class")+
  xlab("Age")+
  ylab("Total Count")


#Female children may have different survival rates
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))


#Sibsp variable
summary(data.combined$SibSp)

##Treat sibsp as a factor
length(unique(data.combined$SibSp))
data.combined$SibSp <- as.factor(data.combined$SibSp)

##Title could be predictive.
ggplot(data.combined[1:891,], aes(x=SibSp, fill = Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass + Title)+
  ggtitle("Passengers by SibSp, Class, Title, and Survival")+
  xlab("Sibsp")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill = "Survived")


#Parch variable
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x=Parch, fill = Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass + Title)+
  ggtitle("Passengers by Parch, Class, Title, and Survival")+
  xlab("Parch")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill = "Survived")


#Create a family size feature
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$Family.size <- as.factor(temp.sibsp + temp.parch + 1)

##Visualize Family.size
ggplot(data.combined[1:891,], aes(x=Family.size, fill = Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass + Title)+
  ggtitle("Passengers by Family Size, Class, Title, and Survival")+
  xlab("Family Size")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill = "Survived")


#Ticket variable
str(data.combined$Ticket)

##Convert to character data type
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

##Investigate ticketing for patterns
###First character
Ticket.first.char <- ifelse(data.combined$Ticket == ""," ", substr(data.combined$Ticket, 1, 1))
unique(Ticket.first.char)

data.combined$Ticket.first.char <- as.factor(Ticket.first.char)

ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill = Survived))+
  geom_bar()+
  ggtitle("Suvivability by Ticketing")+
  xlab("Ticket first character")+
  ylab("Total Count")+
  ylim(0,350)+
  labs(fill = "Survived")

###Combine ticketing with other variables
ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Suvivability by Ticketing & Class")+
  xlab("Ticket first character")+
  ylab("Total Count")+
  ylim(0,150)+
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Title)+
  ggtitle("Suvivability by Ticketing, Class, & Title")+
  xlab("Ticket first character")+
  ylab("Total Count")+
  ylim(0,200)+
  labs(fill = "Survived")


#Ticketing does not seem to be that predictive, especially compared to other variables


#Fares variable
summary(data.combined$Fare)
length(unique(data.combined$Fare))

ggplot(data.combined, aes(x= Fare))+
  geom_histogram(binwidth = 5)+
  ggtitle("Combined Fare Distribution")+
  xlab("Fare")+
  ylab("Total Count")+
  ylim(0,200)

##Combined fares with other variables
ggplot(data.combined[1:891,], aes(x= Fare, fill = Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass + Title)+
  ggtitle("Fare, Survival, Class, and Title")+
  xlab("Fare")+
  ylab("Total Count")+
  labs(fill = "Survived")+
  ylim(0,50)


#Cabin variable
str(data.combined$Cabin)

data.combined$Cabin <- as.character(data.combined$Cabin)

##Replace empty cabins with a "U" for "Unknown"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

##First character as a factor
Cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(Cabin.first.char)
levels(Cabin.first.char)

data.combined$Cabin.first.char <- Cabin.first.char

ggplot(data.combined[1:891,], aes(x= Cabin.first.char, fill = Survived))+
  geom_bar()+
  ggtitle("Cabin & Survival")+
  xlab("First Character of Cabin")+
  ylab("Total Count")+
  labs(fill = "Survived")+
  ylim(0,750)

ggplot(data.combined[1:891,], aes(x= Cabin.first.char, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Class, Cabin, & Survival")+
  xlab("First Character of Cabin")+
  ylab("Total Count")+
  labs(fill = "Survived")+
  ylim(0,750)

##Does this feature improve upon class and title?
ggplot(data.combined[1:891,], aes(x= Cabin.first.char, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Title)+
  ggtitle("Class, Title, Cabin, & Survival")+
  xlab("First Character of Cabin")+
  ylab("Total Count")+
  labs(fill = "Survived")+
  ylim(0,400)

##Multiple cabins
data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x= Cabin.multiple, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Title)+
  ggtitle("Multiple Cabins & Survival")+
  xlab("Multiple Cabins?")+
  ylab("Total Count")+
  labs(fill = "Survived")+
  ylim(0,350)

###Multiple Cabins is not predictive


#Point of embarkment
str(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x= Embarked, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Title)+
  ggtitle("Point of Departure, Class, Title, & Survival")+
  xlab("Point of Departure")+
  ylab("Total Count")+
  labs(fill = "Survived")+
  ylim(0,750)

##Embarkation does not appear to be predictive








