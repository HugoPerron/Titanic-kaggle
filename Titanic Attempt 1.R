setwd("~/Desktop/Kaggle/titanic")
library(tidyverse)

gender_submission <- read.csv("gender_submission.csv", header = TRUE)
head(gender_submission)
train <- read.csv("train.csv", header = TRUE)
head(train)
test <- read.csv("test.csv", header = TRUE)
head(test)

## Combine train and test to clean them efficiently
train$IsTrainSet <- TRUE
test$IsTrainSet <- FALSE
test$Survived <- NA

full_data <- rbind(train, test)
# Clean missing 'Embarked' values
full_data[full_data$Embarked == '', "Embarked"] <- 'C'

#Clean missing 'Age' values
age.median <- median(full_data$Age, na.rm = TRUE)
full_data[is.na(full_data$Age), "Age"] <- age.median

# Clean missing 'Fare' values
fare.median <- median(full_data$Fare, na.rm = TRUE)
full_data[is.na(full_data$Fare), "Fare"] <- fare.median

# Categorical casting
full_data$PClass <- as.factor(full_data$Pclass)
full_data$Sex <- as.factor(full_data$Sex)
full_data$Embarked <- as.factor(full_data$Embarked)

# Split data back into train and test
train <- full_data[full_data$IsTrainSet == TRUE,]
test <- full_data[full_data$IsTrainSet == FALSE,]

train$Survived <- as.factor(train$Survived)
library(randomForest)
survived.formula <- as.formula("Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked")
titanic.model <- randomForest(formula = survived.formula, data = train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(test))

Survived <- predict(titanic.model, newdata = test)

PassengerId <- test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file = "kaggle_submission.csv", row.names = FALSE)

