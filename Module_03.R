#Name: Shruti Kamble
#Course: Intermediate Analytics
#Instructor: Prof. Roy Wada
#Date: 05/01/2021

#1.Import the dataset and perform Exploratory Data Analysis by using descriptive statistics and plots to describe the dataset.
install.packages("ISLR")
library(ISLR)
View(College)
library(pastecs)
library(psych)

#1.Import the dataset and perform Exploratory Data Analysis by using descriptive statistics and plots to describe the dataset.
my_packages <- c("dplyr", "ggplot2", "psych", "magrittr", "ISLR", "pastecs", "OptimalCutpoints",
                 "InformationValue", "caret")
lapply(my_packages, require, character.only = TRUE)
headTail(College, top = 5, bottom = 5)
str(College)
col_data <- psych::describe(College)
stat_table <- col_data %>% select(n, mean, median, sd, min, max)
write.csv(stat_table, "stat_table.csv")
Hmisc::describe(College)

boxplot(College$Apps, main = paste("Number of applications received"))
boxplot(College$S.F.Ratio, main=paste("Student/faculty ratio"))
boxplot(College$Top10perc, main=paste("Top 10% of H.S. class"))
boxplot(College$Top25perc, main=paste("Top 25% of H.S. class"))
Hmisc::describe(College)


#2. Split the data into a train and test set – refer to the Feature_Selection.R  downloadscript for information on how to split a dataset.
  data("College")
dim(College)
x <- set.seed(123)
train_size <- floor(0.70 * nrow(College))
train_size
train_data <- sample(seq_len(nrow(College)), size = train_size)
train_set <- College[train_data, ]
test_set <- College[-train_data, ]
nrow(train_set) + nrow(test_set)

#3. Use the glm() function in the ‘stats’ package to fit a logistic regression model to the training set using at least two predictors.

  mod_01 <- glm(Private ~ ., data = train_set, family = binomial(link = "logit"))
summary(mod_01)

LogModel <- glm(Private ~ Outstate + F.Undergrad + PhD + perc.alumni + Apps , data=train_set, family=binomial(link = "logit"))
summary(LogModel)

#4.Create a confusion matrix and report the results of your model for the train set. Interpret and discuss the confusion matrix. Which misclassifications are more damaging for the analysis, False Positives or False Negatives?
  prob.train_set = predict(mod_01, newdata = train_set, type = "response")
predicted_clas_min = as.factor(ifelse(prob.train_set >= 0.5, "Yes", "No"))
install.packages("e1071")
library(e1071)

# Creating a confusion matrix
confusionMatrix(predicted_clas_min, train_set$Private, positive = "Yes")
misClassError(train_set$Private, predicted_clas_min)


#5. Report and interpret metrics for Accuracy, Precision, Recall, and Specificity.
  TP = 111
TN = 12
FP = 7
FN = 25

Accuracy = (TN + TP)/(TN+FP+FN+TP)
precision = TP/(FP+TP)
Recall = TP/(TP+FN)
Specificity = TN/(TN+FP)

#6. Create a confusion matrix and report the results of your model for the test set.

prob.test_set = predict(mod_01, newdata = test_set, type = "response")
prob.test_set
predicted_clas_min = as.factor(ifelse(prob.test_set >= 0.5, "Yes", "No"))
predicted_clas_min
head(predicted_clas_min)
confusionMatrix(predicted_clas_min , test_set$Private, positive = "Yes")

#7. Plot and interpret the ROC curve.
library(pROC)
ROC = roc(test_set$Private, prob.test_set)
X <- plot(ROC, col = "blue", ylab = "Sensitivity = TP rate", xlab = 'specificity = FP rate')

#8.Calculate and interpret the AUC.
AUC = auc(ROC)
cat("ROC area under the curve = ", AUC)

