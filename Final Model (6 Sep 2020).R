################################################
#         1. Load necessary libraries          #
################################################

requiredPackages <- c("dplyr","Boruta","randomForest","caret","psych","ggmosaic")
if (length(setdiff(requiredPackages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(requiredPackages, rownames(installed.packages())))  
}

library(dplyr)
library(Boruta)
library(randomForest)
library(caret)
library(psych)
library(ggmosaic)

################################################
#        2.Data Import and Exploration         #
################################################

titanic_train <- read.csv("C:/Users/gkahw/Desktop/School/2. Analytics Software/Group Project/train.csv")
titanic_test <- read.csv("C:/Users/gkahw/Desktop/School/2. Analytics Software/Group Project/test.csv")

# Merge the datasets for preprocessing
titanic_data <- bind_rows(titanic_train,titanic_test)

# Observation
str(titanic_data)
summary(titanic_data)

# Check data integrity
sapply(titanic_data, function(x) {sum(is.na(x))})

# Convert to factor type
titanic_data$Survived <- as.factor(titanic_data$Survived)
titanic_data$Pclass <- as.factor(titanic_data$Pclass)
titanic_data$Embarked <- as.factor(titanic_data$Embarked)

##Survived count
ggplot(titanic_train, aes(x = Survived)) +
  geom_bar(width=0.5, fill = "blue") +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) + labs(x = 'How many people died & survived on Titanic?')
theme_classic()

##Survived count by Sex
ggplot(titanic_train, aes(x = Survived, fill=Sex)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.15)+
  theme_grey()

##Survival by Pclass

ggplot(titanic_train, aes(x = Survived, fill=Pclass)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), 
            vjust=-0.5)+
  theme_grey()

### Data Exploration inserted as Appendices###

#A bar & mosaic plot using proportions rather than counts

ggplot(titanic_train) +
  geom_bar(aes(Sex, fill = Survived), position = "fill")

ggplot(titanic_train) + 
  geom_mosaic(aes(x = product(Sex), fill = Survived)) + 
  labs(x = "Sex", y = "Proportion surviving")


## Visualizing Age to draw relations to survival

ggplot(titanic_data) +
  geom_histogram(aes(x = Age), bins = 35)

qplot(Age, Fare, data=titanic_train, colour=as.factor(Pclass), facets=Sex~Survived)


qplot(Age, Fare, data=titanic_train)
qplot(Age, Fare, data=titanic_train, colour=Pclass)
qplot(Age, Fare, data=titanic_train, colour=as.factor(Pclass))
qplot(Age, Fare, data=titanic_train, colour=as.factor(Pclass), facets=~Sex+Embarked)
qplot(Age, Fare, data=titanic_train, colour=as.factor(Pclass), facets=Sex~Embarked)

barchart <- ggplot(titanic_train, aes(as.factor(Pclass), fill=as.factor(Survived)))+geom_bar()

barchart+xlab("Passenger Class")+ylab("Number of Passengers")+ggtitle("Survival by Passenger Class and Gender")+scale_fill_discrete(name = "", labels = c("Died", "Survived"))

ggplot(titanic_train)  +   geom_bar(aes(Sex, fill = Survived))

################################################
#          3.Feature Engineering               #
################################################

##############################
# 3.1 Create title from name #
##############################

titanic_data$Title <- gsub('(.*, )|(\\..*)','',titanic_data$Name)

# Combine Rare Titles. 
officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')
titanic_data$Title[titanic_data$Title %in% royalty]  <- 'Royalty'
titanic_data$Title[titanic_data$Title %in% officer]  <- 'Officer'


#We count titles like Mlle, Ms all as Miss.
titanic_data$Title[titanic_data$Title=='Mlle'] <- 'Miss'
titanic_data$Title[titanic_data$Title=='Ms'] <- 'Miss'
titanic_data$Title[titanic_data$Title=='Mme'] <- 'Mrs'

table(as.character(titanic_data$Sex), titanic_data$Title)

titanic_data <- titanic_data %>% mutate(Title = factor(Title))


g1 <- ggplot(titanic_data[!is.na(titanic_data$Survived),], aes(x = Title, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Title VS Survived') + theme_grey()
g1 #Passenger with different identities has different Survival status.


##############################
#  3.2 Create Family size    #
##############################

titanic_data$FamilySize <- titanic_data$SibSp + titanic_data$Parch + 1

titanic_data$FamilySize <- titanic_data$SibSp + titanic_data$Parch + 1

g2 <- ggplot(titanic_data[!is.na(titanic_data$Survived),], aes(x = FamilySize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'FamilySize VS Survived') + theme_grey()
g2

###########################################
#  3.3 Create Group Size (From ticket)    #
###########################################

TicketGroup <- titanic_data %>%
  group_by(Ticket) %>%
  count()
colnames(TicketGroup) <- c('Ticket','GroupSize')

titanic_data <- titanic_data %>%
  left_join(TicketGroup,by="Ticket")

g3 <- ggplot(titanic_data[!is.na(titanic_data$Survived),], aes(x = GroupSize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'GroupSize VS Survived') + theme_grey()
g3

#######################################################
# 4. Split back into original test and training data  #
#######################################################

titanic_train <- titanic_data[is.na(titanic_data$Survived)==FALSE,]
titanic_test <- titanic_data[is.na(titanic_data$Survived)==TRUE,]

############################################
#           5.Fix missing values           #
############################################

##########################################
#  5.1 Missing values on training data   #
##########################################

# Identify missing values
str(titanic_train)
summary(titanic_train)

# We notice that variable Age has 177 missing values. Embarked has 2 missing values. Cabin also has a lot of missing values.

pairs.panels(titanic_train[c("Age","Title","Sex","Pclass")])
#Age is related to these features

# Treatment: Age - Impute via median age of Title,Sex,Pclass. Embarked - Impute via mode. Cabin - drop column.

# Imputing Embarked and drop Cabin
titanic_train <- titanic_train %>% 
  mutate(Embarked=ifelse(Embarked=='','S',as.character(Embarked))) %>%
  mutate(Embarked=factor(Embarked)) %>%
  select(-Cabin)

########## Imputing Age Variable ##########

# Finding median age based on Title,Sex,Pclass

median_age <- titanic_train %>%
  group_by(Title,Sex,Pclass) %>%
  summarise(MedianAge = median(Age,na.rm=TRUE))

# Fill in missing age
titanic_train <- titanic_train %>%
  left_join(median_age,by=c("Title","Sex","Pclass")) %>% 
  mutate(Age=ifelse(is.na(Age)==FALSE,Age,MedianAge)) %>%
  select(-MedianAge)

######################################
#  5.2 Missing values on test data   #
######################################

# Identify missing values
str(titanic_test)
summary(titanic_test)
titanic_test$Embarked <- factor(titanic_test$Embarked)

# Noticed that Fare has 1 missing, Age has 86 missing and Cabin has a lot missing.
# Treatment: Age - Impute via median age of Title,Sex,Pclass (from training set). Fare - Impute via median of Pclass (from training set). 
# Cabin - similarly, drop column.

# Compute median of Pclass from training set 
PclassFare <- titanic_train %>%
  group_by(Pclass) %>%
  summarise(MedianFare = median(Fare))

# Performing imputation of Age and Fare. Then, drop Cabin.
titanic_test <- titanic_test %>% 
  left_join(median_age,by=c("Title","Sex","Pclass")) %>% 
  mutate(Age=ifelse(is.na(Age)==FALSE,Age,MedianAge)) %>%
  select(-MedianAge) %>%
  left_join(PclassFare,by="Pclass") %>% 
  mutate(Fare=ifelse(is.na(Fare)==FALSE,Fare,MedianFare)) %>%
  select(-MedianFare,-Cabin)

################################################
#  6.Predicting Survival (Feature Selection)   #
################################################

# Final feature observation
str(titanic_train)

# The variables "Name" and "Ticket" have too many factors to be appropriate for use in a decision tree model.
# Also, information from these variables were already extracted during feature engineering.
# Hence, we will drop these 2 features at the onset.

titanic_train <- titanic_train %>%
  select(-Name,-Ticket)

# Using Boruta algorithm

set.seed(111)
boruta <- Boruta(Survived ~ ., data = titanic_train, doTrace = 2, maxRuns = 500)

print(boruta)             # 10 attributes important, 1 unimportant, 0 unconfirmed

plot(boruta, las = 2, cex.axis = 0.7, xlab="")

# Boruta implies that all features except PassengerID is important. Hence, we look at the relative importance.

# From the graph, the last 3 attributes (Parch,Embarked,SibSp) could potentially be removed.
# FamilySize is a linear function of Parch and SibSp
# Intuitivelly, port of embarkation (Embarked) also should not have an effect on the prediction.

################################################
#      7.Model Prediction (Random Forest)      #
################################################

# Split titanic training data further into sub training and sub test data.
set.seed(222)
ind <- sample(2, nrow(titanic_train), replace = T, prob = c(0.7, 0.3))
train <- titanic_train[ind==1,]
test <- titanic_train[ind==2,]

##############################################
#  7.1 Model 1 (All 10 important attributes) #
##############################################

set.seed(333)
rf1 <- randomForest(Survived~., data = train[,-1]) # Remove PassengerId
rf1

pretest1 <- predict(rf1, test)
confusionMatrix(pretest1, test$Survived)    # Accuracy 0.8046

###############################################
#  7.2 Model 2 (Removed Parch,Embarked,SibSp) #
###############################################

set.seed(333)
rf2 <- randomForest(Survived ~ Sex + Pclass + Title + FamilySize + Age + GroupSize + Fare,  
                    data = train)
rf2

pretest2 <- predict(rf2, test)
confusionMatrix(pretest2, test$Survived)    # Accuracy 0.8199 (improved)

###################################
#   8 Hyperparameter tuning       #
###################################

# Before the prediction, try hyperparameter tuning to see if accuracy can increase further.
?randomForest       # To see the parameters

# Optimising mtry
attributes <- c("Sex","Pclass","Title","FamilySize","GroupSize","Fare","Age")
set.seed(444)
bestmtry <- tuneRF(train[,attributes], train$Survived)    # default mtry is 2, so already optimised

# Given mtry=2, find best ntree and nodesize by building custom RF model for tuning

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)     
customRF$parameters <- data.frame(parameter = c("mtry", "ntree","nodesize"), class = rep("numeric", 3), label = c("mtry", "ntree","nodesize"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, nodesize=param$nodesize)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes
control <- trainControl(method="repeatedcv", number=5, repeats=3)
tunegrid <- expand.grid(.mtry=2, .ntree=c(500, 1000, 1500, 2000, 2500, 3000),.nodesize=c(1:8))

# Train the models

attributes <- c("Survived","Sex","Pclass","Title","FamilySize","GroupSize","Fare","Age")
set.seed(666)
# Note: Next step will take a few minutes to run
custom <- train(Survived~., data=train[,attributes], method=customRF, metric="Accuracy", tuneGrid=tunegrid, trControl=control) 
plot(custom)
custom          # Optimal parameters are mtry=2, ntree=1000 and nodesize=5

###################################################
#  Model 3 (Model 2 adjusting ntree and nodesize) #
###################################################

set.seed(333)
rf3 <- randomForest(Survived ~ Sex + Pclass + Title + FamilySize + Age + GroupSize + Fare,  
                    data = train,
                    ntree=1000,
                    nodesize=5)     
rf3

pretest3 <- predict(rf3, test)
confusionMatrix(pretest3, test$Survived)  # Accuracy 0.8276 (improved)

############################################################################
#  9. Using model 3 to predict actual test dataset (for Kaggle Submission) #
############################################################################

rf_prediction <- predict(rf3, titanic_test)

submission <- data.frame(PassengerId = titanic_test$PassengerId, Survived = rf_prediction)
write.csv(submission,file="C:/Users/gkahw/Desktop/School/2. Analytics Software/Group Project/submission.csv",row.names=FALSE)

# Kaggle score is 0.79425

######################################
#  10. How good is the Kaggle score? #
######################################

Kaggle <- read.csv("C:/Users/gkahw/Desktop/School/2. Analytics Software/Group Project/titanic-publicleaderboard.csv")

# First use a histogram to see the distribution
hist(Kaggle$Score,breaks=50,xaxt='n',xlab="")
axis(side=1, at=seq(0,1,0.05), labels=seq(0,1,0.05))

# Intial Observation: Should drop the suspiciously good scores as well as the suspicious low ones.
# Restrict range from 0.622 (simplest model: guess all did not survive) to 0.85

Kaggle <- Kaggle[Kaggle$Score>=0.622 & Kaggle$Score<=0.85,]

# Look at summary statistics
sum(Kaggle$Score<=0.79425)/length(Kaggle$Score)  # Our submission is in the top 5%!

