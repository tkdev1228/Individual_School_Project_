### Hult International Business School
### MBAN DD
### Takumi Someya
### Student ID : 25804893
### A3: Binary Classification with a Bank Churn Dataset

### Environment Building

setwd("C:/Users/takum/Downloads/HultMBAN/Visualizing & Analyzing Data with R Methods & Tools-Shrivastava/A3")
getwd()

BankChurnDataset<-read.csv("BankChurnDataset.csv", na.strings = c("", "N/A", "NULL"))
NewCustomerDataset<-read.csv("NewCustomerDataset.csv", na.strings = c("", "N/A", "NULL"))

### Get an overview of the dataset

head(BankChurnDataset)
head(NewCustomerDataset)


str(BankChurnDataset)
str(NewCustomerDataset)


names(BankChurnDataset)
names(NewCustomerDataset)


#Fundamental statistic
summary(BankChurnDataset)
summary(NewCustomerDataset)

#################################################################################################

### 1. Handle missing values in the datasets

#Checking for missing values
colSums(is.na(BankChurnDataset))
colSums(is.na(NewCustomerDataset))

#Capture statistical trends in the data set to determine appropriate filling data

# Histogram of Age
hist(BankChurnDataset$Age, main="Histogram of Age", xlab="Age")

# Histogram of EstimatedSalary
hist(BankChurnDataset$EstimatedSalary, main="Histogram of EstimatedSalary", xlab="EstimatedSalary")


# Install zoo package (run only the first time)
install.packages("zoo")

# Load zoo package
library(zoo)

# Fill in missing values for Age and EstimatedSalary with the median
BankChurnDataset$Age <- zoo::na.aggregate(BankChurnDataset$Age, FUN = median)
BankChurnDataset$EstimatedSalary <- zoo::na.aggregate(BankChurnDataset$EstimatedSalary, FUN = median)


# Check that the missing value processing has been completed correctly
colSums(is.na(BankChurnDataset))
colSums(is.na(NewCustomerDataset))


#################################################################################################

# 2. Divide the "BankChurnDataset" into the training and testing dataset. Train the model and Comment on the Model Accuracy, specificity, and sensitivity of the dataset.

#Standardize Variables. As KNN uses distance. Any varaible on larger scale will have much larger
#on the distance than the variables on the smaller scale
var(BankChurnDataset[,1]) # Check the output of above two lines, Variaances looks so different
var(BankChurnDataset[,2]) # Check the output of above two lines, Variaances looks so different

# Review column list again to determine target columns
names(BankChurnDataset)

# Specify column name
target_column <- "Exited"  # Specify column name to be retrieved here

# Get column number with matching column name
column_index <- which(names(BankChurnDataset) == target_column)

# Show Results
print(column_index)

#Exited variables is in the 14th column. Let's save it 
# as a separate variable because KNN function needs it as a separate argument
Exited <- BankChurnDataset[,14]




### Selecting a machine learning model (KNN vs.logistic regression)

##Assumptions Setting
# I have made assumptions here about the characteristics of the KNN and logistic regression models,
# and the work as a data scientist who address this issue in a real business. 
# Then, based on this, I consider and implement the appropriate machine learning model.

# a)This churn rate projection shall be performed on a regular monthly basis. 
#   It is not a high-frequency forecast like the learning models of automated driving or stock trading algorithms.

# b)The logistic regression model has a smaller computational load than KNN and can solve binary classification problems. 
#   On the other hand, there is concern about worsening prediction accuracy due to multicollinearity during model training.

# c)The KNN model has a larger computational load than KNN, but it too can solve the binary classification problem. 
# In addition, the mathematical concepts on which the model is based are intuitive and easy to understand.

##Comparison Result
# I choose KNN. The reasons for this are as follows. 
# First, I will only run this training model once a month (I will have the opportunity to run it to improve the model itself, but not super-frequently). 
# Second, I found that the computational load, which is a disadvantage of this KNN model, can be reduced by combining parallel processing. (I implemented this later in this code). 
# Finally, and most importantly, the theory of KNN is very intuitive and easy to understand. 
# As a data scientist, I need to explain the analysis to engineers without ML/DL knowledge and to partners (bankers) on the business side. 
# My most important mission is to use the results of the analysis to impact business decisions. 
# So to convince them to use the results of this analysis, the explanatory nature and clarity of the model will be the first priority.





# Pre-processing to input the dataset into the KNN model

# Check the data type for each column
str(BankChurnDataset)

# Load dplyr
library(dplyr)

# Label encoding of Surname chr -> num
BankChurnDataset <- BankChurnDataset %>%
  mutate(Surname = as.numeric(factor(Surname)))

# Label encoding of Geography chr -> num
BankChurnDataset <- BankChurnDataset %>%
  mutate(Geography = as.numeric(factor(Geography)))

# Label encoding of Gender chr -> num
BankChurnDataset <- BankChurnDataset %>%
  mutate(Gender = as.numeric(factor(Gender)))

# Make sure that data types are correctly substituted.
str(BankChurnDataset)

# Standarize the dataset using "scale()" R function
standardized.BankChurnDataset <- scale(BankChurnDataset[,-14])

#Make sure it is standardized as intended
var(standardized.BankChurnDataset[,1])
var(standardized.BankChurnDataset[,2])



# Prepare train and test data for KNN models

# Check the total number of rows to to calculate the split ratio
nrow(BankChurnDataset)

# First 33000 rows for test *set 20% of total data
test.index <- 1:33000
test.data <- standardized.BankChurnDataset[test.index,]
test.Exited <- Exited[test.index]

# Rest of data for training **set 80% of total data
train.data <- standardized.BankChurnDataset[-test.index,]
train.Exited <- Exited[-test.index]

# doParallel
install.packages("doParallel")
library(doParallel)

# The computation of multiple knn models is a heavy load, so I will apply parallel processing to reduce it.
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

predicted.Exited <- NULL
error.rate <- NULL

# Install foreach package (run only the first time)
install.packages("foreach")
library(foreach)

# Run KNN models in parallel processing * k = 1~15
results <- foreach(i = 1:15, .combine = "c") %dopar% {
  set.seed(101)
  library(class)  # Load the class package here
  predicted.Exited <- knn(train.data, test.data, train.Exited, k = i)
  mean(test.Exited != predicted.Exited)
}

# Close the cluster
stopCluster(cl)

# Assign the result to error.rate
error.rate <- results

# Quick check that all errors.rates are calculated
head(error.rate)

# Elbow Mothod
# Visualize the learning results for each knn (k=1~15) and determine the appropriate k
library(ggplot2)
k.values<-1:15
error.df <- data.frame(error.rate,k.values)
error.df
ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()+ geom_line(lty="dotted",color='red') + ylim(0.1, 0.3)

#Looking at the graph we can easily see, increasing K beyond 9 will not help our missclassification error rate
# Set k = 9
predicted.Exited <- knn(train.data,test.data,train.Exited,k=9)
mean(test.Exited != predicted.Exited)

# Build KNN model with selected k (k=9)
knn_model <- knn(train.data, test.data, cl = train.Exited, k = 9)

# Create a confusion matrix
confusion_matrix <- table(knn_model, test.Exited)

# Evaluate the model
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])

# Show Results
print(paste("Accuracy:", accuracy))
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))
print("Confusion Matrix:")
print(confusion_matrix)

##### THE ANALYTICS INSIGHTS ABOUT ACCURACY, SENSITIVITY, SPECIFICITY ####

##Model Accuracy (Accuracy)
# Accuracy is approximately 84.88%, which represents the percentage of the total sample that the model was able to predict accurately. 
# For this assignment, the higher the model accuracy is the better.

## Sensitivity
# Sensitivity is approximately 69.21% and indicates the percentage of customers who actually defected that the model correctly predicted would deflect. 
# Sensitivity is of interest when it is important to keep the percentage of false negatives low. In this case, about 69% of the defected customers were detected correctly.

## Specificity
# Specificity is about 87.67% and represents the percentage of customers who did not actually defection that the model correctly predicted would not defection. 
# Specificity is of interest when it is important to keep the percentage of false positives low. In this case, about 87.67% of non-defection customers were correctly detected.

## Trade off between Sensitivity vs. Specificity
# Sensitivity and specificity usually have a trade-off relationship. 
# For example, increasing sensitivity to capture more positive instances (in this case, customers who have dropped out) may increase False Positives and decrease Specificity. 
# Conversely, reducing False Positives in favor of Specificity may reduce Sensitivity.

## To evaluate the impact of that trade-off, draw an ROC curve.
# Install necessary packages (if not already installed)
install.packages("pROC")

# Load packages
library(pROC)

# Create ROC curve
roc_curve <- roc(test.Exited, as.numeric(predicted.Exited))

# Calculate AUC
auc_value <- auc(roc_curve)

# Print AUC value
print(paste("AUC:", auc_value))

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
lines(c(0, 1), c(0, 1), col = "red", lty = 2, lwd = 2)  # Diagonal line for reference
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)

## RESULTS AND INSIGHTS OF ROC CURVE

# The ROC curve is in the low Specificity region, with a large rise in Sensitivity to about 0.7. 
# Additionally, the AUC is 0.721 (>0.5).

# With ROC curve where Specificity is relatively low, and Sensitivity rises significantly, especially up to around 0.7, 
# it indicates that the model is performing well in terms of identifying positive instances while accepting a certain level of false positives.

# The AUC value being greater than 0.5 is a good sign, as an AUC of 0.5 represents random classification. 
# The higher the AUC, the better the model's ability to discriminate between positive and negative instances.

##############################################################################################################

# 3. Use the model to predict whether a particular customer would churn or not using the "NewCustomerDataset".

# Pre-processing to input the dataset into the KNN model

# Check the data type for each column
str(NewCustomerDataset)

# Pre-processing to input the dataset into the KNN model
# Label encoding of categorical variables in NewCustomerDataset
NewCustomerDataset <- NewCustomerDataset %>%
  mutate(Surname = as.numeric(factor(Surname)),
         Geography = as.numeric(factor(Geography)),
         Gender = as.numeric(factor(Gender)))

# Make sure that data types have been converted correctly
str(NewCustomerDataset)

# Standardize NewCustomerDataset
standardized.NewCustomerDataset <- scale(NewCustomerDataset)

# Train KNN model (k=9)
knn_model_NewCustomer <- knn(train.data, standardized.NewCustomerDataset, cl = train.Exited, k = 9)

# Show predictions of "Churn" or "Not Churn"
predicted_Exited_NewCustomer <- ifelse(knn_model_NewCustomer == 1, "Churn", "Not Churn")
predicted_Exited_NewCustomer

# Visualize prediction results
# Create a data frame of the prediction results
result_df <- data.frame(CustomerID = NewCustomerDataset$id, Predicted_Exited = predicted_Exited_NewCustomer)

# Install package ggplot2 (run only the first time)
install.packages("ggplot2")
library(ggplot2)

# Calculate the ratio of each category
ratio_df <- result_df %>%
  group_by(Predicted_Exited) %>%
  summarise(ratio_df = n() / nrow(result_df))

# Create bar graphs (displayed separately)
ggplot(ratio_df, aes(x = Predicted_Exited, y = ratio_df, fill = Predicted_Exited)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Predicted Churn Status for New Customers",
       x = "Churn Status",
       y = "Ratio")
  theme_minimal()

# Get the number of Churn / Not Churn
print(ratio_df)
  
### CODE END ###
  
### Reference ###
# OpenAI. (2024). ChatGPT (3.5) [Large language model]. https://chat.openai.com, https://chat.openai.com/share/68670f72-4f55-4d5b-8e24-cad574688350
# OpenAI. (2024). ChatGPT (3.5) [Large language model]. https://chat.openai.com, https://chat.openai.com/share/3d7335db-9845-4000-bee4-fb8f1ba7e83d
# OpenAI. (2024). ChatGPT (3.5) [Large language model]. https://chat.openai.com, https://chat.openai.com/share/e4b5d2ed-11fd-4d15-97bf-9bd2a20ea689