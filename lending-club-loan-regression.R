#####################################################################################
# Data Science
# Providing a regression model for the interest rate - int_rate
# Jasmin Fluri, Roman Fischer, Sebastian Geiger, Thomas Probst
#####################################################################################


########## COACHING QUESTIONS ##########
# How can results or modelling attempts be saved to be able to compare them later on and 
# have an overview of the different attempts that have been made. #292
# Do we have to replicate all changes from the training data set to the test data set? e.g. when filling in NAs, fill them in by the means from training DS? #265?
# In the example of predicting house prices (line 79) we used the scale() function to normalize the features. For this assignment we should use values between 0 and 1. why?


######################################################################################
# DATA INFORMATION 
######################################################################################

# Understand the data:
# https://www.kaggle.com/pragyanbo/a-hitchhiker-s-guide-to-lending-club-loan-data

######################################################################################
# Before starting with the data preparation, check for missing libraries
######################################################################################


# Disable GPU to avoid errors
Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)

libraries_used<-
  c("MASS","varImp","plyr", "dplyr", "glmnet", "caret", "tidyverse", "funModeling", 
    "leaps", "corrplot", "car","randomForest", "mlbench","tidyr","keras")

libraries_missing<-
  libraries_used[!(libraries_used %in% installed.packages()[,"Package"])]

if(length(libraries_missing)) install.packages(libraries_missing)

library(plyr)
library(tidyverse)
library(caret)
library(leaps)
library(glmnet)
library(corrplot)
library(car)
library(randomForest)
library(varImp)
library(mlbench)
library(MASS)
library(tidyr)
library(stringi)
library(dplyr)
library(keras)



######################################################################################
# Set the working directory to the folder with the data and load data
######################################################################################

setwd("C:/Users/jasmi/Dropbox/MSc-FHNW/Modules/_DataScience/Assignment") #Jasmin
setwd("C:/Users/Thoems/OneDrive/Studium/Master/Data Science/R scripts/Assignment for regression") #Thomas PC
setwd("E:/OneDrive/Studium/Master/Data Science/R scripts/Assignment for regression") #Thomas Laptop
setwd("C:/Users/roman/Desktop/FHNW/Data Science/Assignment") #Roman
setwd("C:/Users/Sebastian/OneDrive/School_Master/DataScience/Assignment") #Sebi

# To make the regression reproducible, the seed is set to 1
set.seed (1)

# Load the csv file of the regression train data into the data object allData
allData<- read.csv("regression_train_loan.csv",header= TRUE,sep = ",",quote="\"",
                   dec=".",fill= TRUE,na.strings= "NA",blank.lines.skip= TRUE)
# Load the csv file of the test data in the data object allData_test
allData_test<- read.csv("loan_eval.csv",header= TRUE,sep = ",",quote="\"",dec=".",
                        fill= TRUE,na.strings= "NA",blank.lines.skip= TRUE)

# Print the head of the data object allData
head(allData)

######################################################################################
# FEATURE ENGINEERING / DATA ANALYSIS 
######################################################################################

# Calculate the number of unique values per variable in our dataset
meta_loans <- funModeling::df_status(allData, print_results = FALSE)

# Print the number of unique values per variable
knitr::kable(meta_loans)

# Calculates how many different values are in an attribute
meta_loans <-
  meta_loans %>%
  mutate(uniq_rat = unique / nrow(allData))
meta_loans
# Get unique values in percentage  
meta_loans %>%
  dplyr::select(variable, unique, uniq_rat) %>%
  mutate(unique = unique, uniq_rat = scales::percent(uniq_rat)) %>%
  knitr::kable()

#Remove the following values from the dataset:
# 1: X
# 2: id
# 3: member_id
# 11: sub_grade
# 12: emp_title
# 19: pymnt_plan
# 20: url
# 21: desc
# 23: title
# 24: zip_code
# 37: initial_list_status
# 53: policy_code
# 57: verification_status_joint
loan <- allData[,-c(1,2,3,11,12,19,20,21,23,24,37,53,57)]
loan_test <- allData_test[,-c(1,2,3,11,12,19,20,21,23,24,37,53,57)]
# Print the summary of the new smaller dataset
summary(loan)

######################################################################################
# DATA CLEANING
######################################################################################

# In order to count NA, we first have to get rid of all blank or "n/a" (e.g. in emp_length) values.
loan[loan==""] <- NA
loan_test[loan_test==""] <- NA
loan[loan=="n/a"] <- NA
loan_test[loan_test=="n/a"] <- NA

# Data frame is created of our dataset
loan.column <- as.data.frame(colnames(loan))

# The count of NA of all columns is stored in the data frame
loan.column$NAs <- as.data.frame(sapply(loan, function(x) sum(is.na(x))))[,1] 

# NAs are stored in %
loan.column$NA_percent <- loan.column$NAs / nrow(loan) 

#TODO : Print overview

#Remove the following columns which have >70% NAs
# 44: dti_joint	99.9%
# 43: annual_inc_joint 99.9%
# 54: il_util	97.9%
# 52: mths_since_rcnt_il 97.7%
# 48: open_acc_6m	97.6%
# 49: open_il_6m 97.6%
# 50: open_il_12m	97.6%
# 51: open_il_24m	97.6%
# 53: total_bal_il 97.6%
# 55: open_rv_12m	97.6%
# 56: open_rv_24m	97.6%
# 57: max_bal_bc 97.6%
# 58: all_util 97.6%
# 60: inq_fi 97.6%
# 61: total_cu_tl 97.6%
# 62: inq_last_12m 97.6%
# 21: mths_since_last_record 84.5%
# 41: mths_since_last_major_derog 75.0%

# Remove columes with more than 70% NA Values
loan <- loan[,-c(21,41,43,44,48,49,50,51,52,53,54,55,56,57,58,60,61,62)]
loan_test <- loan_test[,-c(21,41,43,44,48,49,50,51,52,53,54,55,56,57,58,60,61,62)]

# Remove rows with more than 70% NA Values --> There are none in our dataset

# 44 Rows leads to 31 NA Values to reach the 70% threshold
loan$na_count <- apply(loan, 1, function(x) sum(is.na(x)))

# TODO: Show overview

# Highest Values --> 14 NA Values, therefore no general Action required --> na_count column is removed
loan <- loan[,-c(45)]

#list occurrences of values in specific fields to spot anomalies, blank values, etc.
#as.data.frame(table(loan$grade)) 


######################################################################################
# MODIFICATION OF DATE COLUMNS
######################################################################################

# convert dates to year without days and month -> YYYY
loan$issue_d <- substr(loan$issue_d,5,8)
loan$earliest_cr_line <- substr(loan$earliest_cr_line,5,8)
loan$last_pymnt_d <- substr(loan$last_pymnt_d,5,8)
loan$next_pymnt_d <- substr(loan$next_pymnt_d,5,8)
loan$last_credit_pull_d <- substr(loan$last_credit_pull_d,5,8)

loan_test$issue_d <- substr(loan_test$issue_d,5,8)
loan_test$earliest_cr_line <- substr(loan_test$earliest_cr_line,5,8)
loan_test$last_pymnt_d <- substr(loan_test$last_pymnt_d,5,8)
loan_test$next_pymnt_d <- substr(loan_test$next_pymnt_d,5,8)
loan_test$last_credit_pull_d <- substr(loan_test$last_credit_pull_d,5,8)

# Print summary of last_pymnt_d
summary(loan$last_pymnt_d)
str(loan$last_pymnt_d) #year data is chr. but should be numeric

# Years are treated as numeric values and not as categorical values
loan$issue_d <- as.numeric(loan$issue_d)
loan$earliest_cr_line <- as.numeric(loan$earliest_cr_line)
loan$last_pymnt_d <- as.numeric(loan$last_pymnt_d)
loan$next_pymnt_d <- as.numeric(loan$next_pymnt_d)
loan$last_credit_pull_d <- as.numeric(loan$last_credit_pull_d)

loan_test$issue_d <- as.numeric(loan_test$issue_d)
loan_test$earliest_cr_line <- as.numeric(loan_test$earliest_cr_line)
loan_test$last_pymnt_d <- as.numeric(loan_test$last_pymnt_d)
loan_test$next_pymnt_d <- as.numeric(loan_test$next_pymnt_d)
loan_test$last_credit_pull_d <- as.numeric(loan_test$last_credit_pull_d)

# summary(loan$last_pymnt_d) --> Based on this output we changed the values to ordinal

# Make year values ordinal (string to ordinal) of the date fields
# The order = TRUE puts the years in the correct order
# loan$issue_d <- factor(loan$issue_d, order = TRUE)
# loan$earliest_cr_line <- factor(loan$earliest_cr_line, order = TRUE)
# loan$last_pymnt_d <- factor(loan$last_pymnt_d, order = TRUE)
# loan$next_pymnt_d <- factor(loan$next_pymnt_d, order = TRUE)
# loan$last_credit_pull_d <- factor(loan$last_credit_pull_d, order = TRUE)

# loan_test$issue_d <- factor(loan_test$issue_d, order = TRUE)
# loan_test$earliest_cr_line <- factor(loan_test$earliest_cr_line, order = TRUE)
# loan_test$last_pymnt_d <- factor(loan_test$last_pymnt_d, order = TRUE)
# loan_test$next_pymnt_d <- factor(loan_test$next_pymnt_d, order = TRUE)
# loan_test$last_credit_pull_d <- factor(loan_test$last_credit_pull_d, order = TRUE)

# check the distribution of the grade variable
as.data.frame(table(loan$grade)) 

# Convert loan$grade to ordinal and put it into the correct order
loan$grade <- factor(loan$grade, order = TRUE)
loan_test$grade <- factor(loan_test$grade, order = TRUE)

# check emp_length - the categories are strings and have to be converted to numeric
as.data.frame(table(loan$emp_length)) 

# Convert emp_length to numeric values - Only the numbers are extracted
loan$emp_length <- as.numeric(gsub("[^0-9.]", "",  loan$emp_length))
loan_test$emp_length <- as.numeric(gsub("[^0-9.]", "",  loan_test$emp_length))

# check the distribution of home_ownership
as.data.frame(table(loan$home_ownership)) 
# "ANY"  only occurs 2 times - ut as long as we don't have problems we let the model decide if it is of any interest
# same thing with  NONE (45) and OTHER (155) (there is no downsampling planned at the moment)

# check the distribution of verification_status: this looks good: only 3 categories
as.data.frame(table(loan$verification_status)) 

#check the distribution of loan_status
as.data.frame(table(loan$loan_status)) 
# TODO: maybe later downsampling according to:
# https://triamus.github.io/project/lending-club-loan-data-in-r/#defining-default
# punkt 6. careful, we have correlations to other columns.

#check the distribution of purpose. We will leave it as it is
as.data.frame(table(loan$purpose)) 

#check the distribution of addr_state
as.data.frame(table(loan$addr_state))  
# We let the model decide if it is of any use. If we run into computation limits:  we'll try without addr_state
# maybe needed later: remove addr_state from dataset
#loan <- subset(loan, select = -c(addr_state))

#Convert variable application type as factor variable
loan$application_type <- factor(loan$application_type)
loan_test$application_type <- factor(loan_test$application_type)


######################################################################################
# MISSING VALUE TREATMENT
######################################################################################

# For the emp_length value: we assume that missing values are values smaller than 1 year
# Therefore we fill missing values with 0 and set NA to 0 
loan$emp_length[is.na(loan$emp_length)] <- 0 
loan_test$emp_length[is.na(loan_test$emp_length)] <- 0 

# Replace NA Values with mean (for numerical values) or with the mode (for categorical values) 
# in columns that have less than 70% NA

# for numerical values the mean is set instead of the missing value for the following columns
# tot_cur_bal - 43
loan[is.na(loan[,43]), 43] <- mean(loan[,43], na.rm = TRUE) 
loan_test[is.na(loan_test[,43]), 43] <- mean(loan[,43], na.rm = TRUE) #we must fill in the means of our training data, not test data!

# total_rev_hi_lim - 44
loan[is.na(loan[,44]), 44] <- mean(loan[,44], na.rm = TRUE) 
loan_test[is.na(loan_test[,44]), 44] <- mean(loan[,44], na.rm = TRUE) 

# tot_coll_amt - 42
loan[is.na(loan[,42]), 42] <- mean(loan[,42], na.rm = TRUE)
loan_test[is.na(loan_test[,42]), 42] <- mean(loan[,42], na.rm = TRUE)

# mths_since_last_deling - 20
loan[is.na(loan[,20]), 20] <- mean(loan[,20], na.rm = TRUE) 
loan_test[is.na(loan_test[,20]), 20] <- mean(loan[,20], na.rm = TRUE) 

# revol_util - 24
loan[is.na(loan[,24]), 24] <- mean(loan[,24], na.rm = TRUE) 
loan_test[is.na(loan_test[,24]), 24] <- mean(loan[,24], na.rm = TRUE) 

# total_acc - 25
loan[is.na(loan[,25]), 25] <- mean(loan[,25], na.rm = TRUE) 
loan_test[is.na(loan_test[,25]), 25] <- mean(loan[,25], na.rm = TRUE) 

# annual_inc - 10
loan[is.na(loan[,10]), 10] <- mean(loan[,10], na.rm = TRUE) 
loan_test[is.na(loan_test[,10]), 10] <- mean(loan[,10], na.rm = TRUE) 

# delinq_2yrs - 17
loan[is.na(loan[,17]), 17] <- mean(loan[,17], na.rm = TRUE) 
loan_test[is.na(loan_test[,17]), 17] <- mean(loan[,17], na.rm = TRUE) 

# inq_last_6mths - 19
loan[is.na(loan[,19]), 19] <- mean(loan[,19], na.rm = TRUE) 
loan_test[is.na(loan_test[,19]), 19] <- mean(loan[,19], na.rm = TRUE) 

# open_acc - 21
loan[is.na(loan[,21]), 21] <- mean(loan[,21], na.rm = TRUE) 
loan_test[is.na(loan_test[,21]), 21] <- mean(loan[,21], na.rm = TRUE) 

# pub_rec - 22
loan[is.na(loan[,22]), 22] <- mean(loan[,22], na.rm = TRUE) 
loan_test[is.na(loan_test[,22]), 22] <- mean(loan[,22], na.rm = TRUE) 


# for categorical values the mode is set instead of the missing value for the following columns


# set the mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# earliest_cr_line - 18
loan[is.na(loan[,18]), 18] <- Mode(loan[,18]) 
loan_test[is.na(loan_test[,18]), 18] <- Mode(loan[,18]) #we must fill in the modes of our training data, not test data!

# next_pymnt_d - 37
loan[is.na(loan[,37]), 37] <- Mode(loan[,37]) 
loan_test[is.na(loan_test[,37]), 37] <- Mode(loan[,37]) 

# last_pymnt_d - 35
loan[is.na(loan[,35]), 35] <- Mode(loan[,35]) 
loan_test[is.na(loan_test[,35]), 35] <- Mode(loan[,35]) 

# last_credit_pull_d - 38
loan[is.na(loan[,38]), 38] <- Mode(loan[,38]) 
loan_test[is.na(loan_test[,38]), 38] <- Mode(loan[,38]) 

# collections_12_mths_ex_med - 39
loan[is.na(loan[,39]), 39] <- Mode(loan[,39]) 
loan_test[is.na(loan_test[,39]), 39] <- Mode(loan[,39]) 

# acc_now_delinq - 41
loan[is.na(loan[,41]), 41] <- Mode(loan[,41]) 
loan_test[is.na(loan_test[,41]), 41] <- Mode(loan[,41]) 


######################################################################################
# ATTRIBUTE IMPORTANCE / FEATURE SELECTION
######################################################################################

#function to get all numerical variables
num_vars <- 
  loan %>%
  sapply(is.numeric) %>%
  which() %>%
  names()

#print an overview of the variables that we have
meta_train <- funModeling::df_status(loan, print_results = FALSE)

#Show numerical variables with correlation as pie chart
corrplot::corrplot(cor(loan[, num_vars], use = "complete.obs"),
                   method = "pie", type = "upper")
loan.cor <- cor(loan[, num_vars]) #plot is printed as a table


#find variables with high correlation
caret::findCorrelation(cor(loan[, num_vars], use = "complete.obs"),
                       names = TRUE, cutoff = .6) #prints correlation



#removes values that have a correlation
loan <- subset(loan, select=-c(loan_amnt, funded_amnt, funded_amnt_inv, total_pymnt, 
                               total_pymnt_inv, out_prncp, total_rec_prncp, revol_bal, 
                               total_acc, recoveries, issue_d, last_pymnt_d )) 

loan_test <- subset(loan_test, select=-c(loan_amnt, funded_amnt, funded_amnt_inv, 
                                         total_pymnt, total_pymnt_inv, out_prncp,
                                         total_rec_prncp, revol_bal, total_acc, 
                                         recoveries, issue_d, last_pymnt_d )) 

#removes values that have a high percentage of zeros
loan <- subset(loan, select=-c(tot_coll_amt, acc_now_delinq, collections_12_mths_ex_med, 
                               collection_recovery_fee, total_rec_late_fee, pub_rec, 
                               delinq_2yrs))

loan_test <- subset(loan_test, select=-c(tot_coll_amt, acc_now_delinq, 
                                         collections_12_mths_ex_med, 
                                         collection_recovery_fee, 
                                         total_rec_late_fee, pub_rec, delinq_2yrs))


# Getting the attribute importance of variables in our dataset

lm.testmodel = lm(int_rate~., data = loan)

summary(lm.testmodel)



######################################################################################
# DATA SET ORGANIZATION (training & test) for regression models RM and neural networks NN
# The data for neural networks is further processed for the second assignment. Do not use for regression models.
# 
# data frame (RM)
# - loan.train
# - loan.test
# 
# numeric matrix (RM, NN)
# - num.loan.train
# - num.loan.test
# - nn.num.loan.train
# - nn.num.loan.test
#
# scaled numeric matrix around 0 (RM, NN)
# - sc0.loan.train
# - sc0.loan.test
# - nn.sc0.loan.train
# - nn.sc0.loan.test
#
# scaled numeric matrix between 0 and 1 (RM, NN)
# - sc01.loan.train
# - sc01.loan.test
# - nn.sc01.loan.train
# - nn.sc01.loan.test
#
######################################################################################

# data frame (RM)
loan.train <- loan # training data (already split by file)
loan.test <- loan_test # test data (already split by file)


# numeric matrix (RM)
num.loan.train <- data.matrix(loan.train)
num.loan.test <- data.matrix(loan.test)


# scaled numeric matrix around 0 (RM)
mean <- apply(num.loan.train, 2, mean) #only use training data for scaling
std <- apply(num.loan.train, 2, sd)
sc0.loan.train <- scale(num.loan.train, center = mean, scale = std)
sc0.loan.test <- scale(num.loan.test, center = mean, scale = std)


# scaled numeric matrix between 0 and 1 (RM)
min <- apply(num.loan.train, 2, min)
range <- apply(num.loan.train, 2, range)

sc01.loan.train <- apply(num.loan.train, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))) # convert matrix to floating numbers between 0 and 1.
#sc01.loan.test <- apply(num.loan.test, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))) # convert matrix to floating numbers between 0 and 1. <- not allowed to scale by test data values
sc01.loan.test <- num.loan.test

for(i in 1:length(num.loan.test[1,])){ #scale Test data by Training data values
  sc01.loan.test[,i] <- (num.loan.test[,i] - min[i]) / diff(range[,i])
}
#summary(sc01.loan.test)



# numeric matrix (NN)
nn.num.loan.train <- loan.train[loan.train$loan_status != "Current", ] # omit "Current"
nn.num.loan.test <- loan.test[loan.test$loan_status != "Current", ]

nn.num.loan.train$loan_status[nn.num.loan.train$loan_status!="Fully Paid"] <- "DEFAULTED" # set everything but "Fully Paid" to "DEFAULTED"
nn.num.loan.test$loan_status[nn.num.loan.test$loan_status!="Fully Paid"] <- "DEFAULTED"

nn.num.loan.train <- nn.num.loan.train %>%
  mutate(loan_status = ifelse(loan_status == "Fully Paid",1,0)) # make loan_status binary
nn.num.loan.test <- nn.num.loan.test %>%
  mutate(loan_status = ifelse(loan_status == "Fully Paid",1,0))

nn.loan.train_y <- nn.num.loan.train[,10] # labels for training & testing data (loan_status)
nn.loan.test_y <- nn.num.loan.test[,10]

nn.num.loan.train <- data.matrix(nn.num.loan.train[,-10]) # Create matrix of floating numbers (exclude loan_status)
nn.num.loan.test <- data.matrix(nn.num.loan.test[,-10])

nn.num.loan.train <- array_reshape(nn.num.loan.train, c(nrow(nn.num.loan.train), length(nn.num.loan.train[1,])))
nn.num.loan.test <- array_reshape(nn.num.loan.test, c(nrow(nn.num.loan.test), length(nn.num.loan.test[1,])))


# scaled numeric matrix around 0 (NN)
nn.mean <- apply(nn.num.loan.train, 2, mean) #only use training data for scaling
nn.std <- apply(nn.num.loan.train, 2, sd)
nn.sc0.loan.train <- scale(nn.num.loan.train, center = nn.mean, scale = nn.std)
nn.sc0.loan.test <- scale(nn.num.loan.test, center = nn.mean, scale = nn.std)


# scaled numeric matrix between 0 and 1 (NN)
nn.min <- apply(nn.num.loan.train, 2, min)
nn.range <- apply(nn.num.loan.train, 2, range)

nn.sc01.loan.train <- apply(nn.num.loan.train, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))) # convert matrix to floating numbers between 0 and 1.
#nn.sc01.loan.test <- apply(nn.num.loan.test, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))) # convert matrix to floating numbers between 0 and 1. <- not allowed to scale by test data values
nn.sc01.loan.test <- nn.num.loan.test

for(i in 1:length(nn.num.loan.test[1,])){ #scale Test data by Training data values
  nn.sc01.loan.test[,i] <- (nn.num.loan.test[,i] - nn.min[i]) / diff(nn.range[,i])
}
#summary(nn.sc01.loan.test)





########## SUBSET SELECTION / FEATURE ANALYSIS & ENGINEERING ##########

# TODO:check last semester approach: which attributes are most important for prediction?

# Best Subset Selection

# We are telling R that we have a really big dataset 
#sets <- regsubsets(int_rate ~ ., loan, nvmax = 12, really.big=T)
# TODO : Test again if it works after removing stuff


# Forward Stepwise Selection
#sets_FWS <- regsubsets(int_rate ~ ., loan, nvmax = 12, method = "forward")
# => ??


# Backward Stepwise Selection
#sets_BWS <- regsubsets(int_rate ~ ., loan, nvmax = 5, method = "backward")
# => ??

# Hybrid Stepwise Methods(?)


# do a feature selection of (5-10) attributes to get a meaningful result 
# TODO : Experiment
# TODO : make data frame (some sort of storage) to save the results of the different feature sets. 


########## LINEAR REGRESSION MODELS / ANALYSIS ##########

#get linear model for all variables
mymodel <- lm(int_rate~.,data=loan.train)
ourmodels <- list(mymodel)
summary(ourmodels[[1]])
BIC(mymodel) #2475357

#Remove variables with less or no significance due to linear model
#loan.Train <- subset(loan.Train, select=-c(next_pymnt_d, earliest_cr_line, addr_state, home_ownership))
mymodel_fixed1 <- lm(int_rate~. -addr_state,data=loan.train)
summary(mymodel_fixed1)
BIC(mymodel_fixed1) #2475131

mymodel_fixed2 <- lm(int_rate~. -home_ownership,data=loan.train)
summary(mymodel_fixed2)
BIC(mymodel_fixed2) #2475575

mymodel_fixed3 <- lm(int_rate~. -home_ownership -addr_state,data=loan.train)
summary(mymodel_fixed3)
BIC(mymodel_fixed3) #2475335

#remove(mymodel_fixed7)



#vif(ourmodels[[1]])
#plot(ourmodels[[1]])
# Notice the points fall along a line in the middle of the graph, but curve off in the extremities.
# Normal Q-Q plots that exhibit this behavior usually mean your data have more extreme values than
# would be expected if they truly came from a Normal distribution.

mymodel <- lm(int_rate~grade,data=loan.Train)
ourmodels[[2]] <- mymodel
summary(ourmodels[[2]])
par(mfrow=c(2,2))
plot(ourmodels[[2]])

#predict against test data
pred <- predict(mymodel, newdata = loan.Test)

par(mfrow=c(1,1))
plot(loan.Test$int_rate, pred)

prediction_model_perf <- data.frame(RMSE=RMSE(pred, loan.Test$int_rate),
                                    RSquared=R2(pred, loan.Test$int_rate))



#This stepAIC approach runs quite long and does not provide a good model
#fit <- lm(int_rate~., data=loan[complete.cases(loan),])
#step <- stepAIC(fit, direction = "both")
#step$anova


##########  Ridge & Lasso Regression ########## 

# Tuning parameters
# alpha - mixing percentage (alpha = 0 (ridge regression) / alpha = 1 (lasso regression))
# lambda - regularization tuning parameter

# 10-fold cross validation
validationspec <- trainControl(method = "cv" , number = 10 , savePredictions = "all")

# set random lambdas between 5 and -5
lambdas <- 10^seq(5, -5, length=100) # create possible lambda values

##########  
### Ridge Regression Model
##########  


# set seed again
set.seed(1)

# creat the model with lasso regression
# The model tries to minimise the RMSE --> which lambda has the lowest RMSE (can be visualy plotted)
model_ridge <- train(int_rate ~ .,
                     data=loan.train,
                     preProcess=c("center","scale"),
                     method="glmnet",
                     tuneGrid=expand.grid(alpha=0, lambda=lambdas),
                     trControl=validationspec,
                     na.action=na.omit)

# Wheightening of the model
coef(model_ridge$finalModel, model_ridge$bestTune$lambda)

### Prediction against the testdata
prediction_ridge <- predict(model_ridge, newdata=loan.test)

prediction_perf <- data.frame(Model="Ridge",
                              RMSE=RMSE(prediction_ridge, loan.test$int_rate),
                              RSquared=R2(prediction_ridge, loan.test$int_rate))


##########  
### Lasso Regression Model
##########  

# set seed again
set.seed(1)

# creat the model with lasso regression
# The model tries to minimise the RMSE --> which lambda has the lowest RMSE (can be visualy plotted)
model_lasso <- train(int_rate ~ .,
                     data=loan.train,
                     preProcess=c("center","scale"),
                     method="glmnet",
                     tuneGrid=expand.grid(alpha=1, lambda=lambdas),
                     trControl=validationspec,
                     na.action=na.omit)

# analyze which attributes are removed from the model
coef(model_lasso$finalModel, model_lasso$bestTune$lambda) #Attributes with the value 0 are removed from the model


### Prediction against the testdata
prediction_lasso <- predict(model_lasso, newdata=loan.test)

prediction_perf <- prediction_perf %>% add_row(Model="Lasso",
                           RMSE=RMSE(prediction_lasso, loan.test$int_rate),
                           RSquared=R2(prediction_lasso, loan.test$int_rate))


######################################################################################
# Assignment 2: Classification by Neural Networks
######################################################################################

# store complete training data set for later use with k-fold cross validation
nn.num.loan.k <- nn.num.loan.train
nn.sc0.loan.k <- nn.sc0.loan.train
nn.sc01.loan.k <- nn.sc01.loan.train

nn.loan.k_y <- nn.loan.train_y


# setting apart the validation set
nn.val_indices <- sample(nrow(nn.num.loan.train),nrow(nn.num.loan.train)*0.3) # indices of a validation data (30% of training data)

nn.num.loan.val <- nn.num.loan.train[nn.val_indices,] # validation data
nn.sc0.loan.val <- nn.sc0.loan.train[nn.val_indices,]
nn.sc01.loan.val <- nn.sc01.loan.train[nn.val_indices,]

nn.loan.val_y <- nn.loan.train_y[nn.val_indices] # validation label (loan_status)


nn.num.loan.train <- nn.num.loan.train[-nn.val_indices,] # training data
nn.sc0.loan.train <- nn.sc0.loan.train[-nn.val_indices,]
nn.sc01.loan.train <- nn.sc01.loan.train[-nn.val_indices,]

nn.loan.train_y <- nn.loan.train_y[-nn.val_indices] # training label (loan_status)



set.seed(1)
# defining the network
network <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(26)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
  #layer_dense(units = 2, activation = "softmax")

# defining the optimizer and loss function
network %>% compile(
  optimizer = optimizer_adam(lr = 1e-2),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# let it run
history <- network %>% fit(
  nn.sc01.loan.train,
  #nn.sc0.loan.train,
  #nn.num.loan.train,
  nn.loan.train_y,
  epochs = 25,
  batch_size = 512,
  validation_data = list(nn.sc01.loan.val, nn.loan.val_y)
  #validation_data = list(nn.sc0.loan.val, nn.loan.val_y)
  #validation_data = list(nn.num.loan.val, nn.loan.val_y)
)

network %>% evaluate(nn.sc01.loan.test, nn.loan.test_y) # accuracy 0.9738
#network %>% evaluate(nn.sc0.loan.test, nn.loan.test_y) # accuracy 0.9703
#network %>% evaluate(nn.num.loan.test, nn.loan.test_y) # accuracy 0.8904



# define loop variables
lv_unit <- c(2,4,8,16,32,64,128,256) #8
lv_optimizer <- c(optimizer_rmsprop(lr = 1e-1),
                  optimizer_rmsprop(lr = 1e-2),
                  optimizer_rmsprop(lr = 1e-3),
                  optimizer_rmsprop(lr = 1e-4),
                  optimizer_adam(lr = 1e-1),
                  optimizer_adam(lr = 1e-2),
                  optimizer_adam(lr = 1e-3),
                  optimizer_adam(lr = 1e-4),
                  optimizer_nadam(lr = 1e-1),
                  optimizer_nadam(lr = 1e-2),
                  optimizer_nadam(lr = 1e-3),
                  optimizer_nadam(lr = 1e-4) #12
                  )
lv_activation <- c("relu","selu","elu") #3
# 8*12*3 = 288


results <- data.frame()
i <- 1

for(i_unit in lv_unit) {
  for(i_optimizer in 1:12){
    for(i_activation in 1:3){
      
      set.seed(1)
      # defining the network
      network <- keras_model_sequential() %>%
        layer_dense(units = i_unit, activation = lv_activation[i_activation], input_shape = c(26)) %>%
      
        layer_dense(units = i_unit, activation = lv_activation[i_activation]) %>% #2
        layer_dense(units = i_unit, activation = lv_activation[i_activation]) %>% #3
        layer_dense(units = i_unit, activation = lv_activation[i_activation]) %>% #4
        layer_dense(units = i_unit, activation = lv_activation[i_activation]) %>% #5
        layer_dense(units = i_unit, activation = lv_activation[i_activation]) %>% #6
        layer_dense(units = i_unit, activation = lv_activation[i_activation]) %>% #7
        layer_dense(units = i_unit, activation = lv_activation[i_activation]) %>% #8
        
        layer_dense(units = 1, activation = "sigmoid")
      
      # defining the optimizer and loss function
      network %>% compile(
        optimizer = lv_optimizer[[i_optimizer]],
        loss = "binary_crossentropy",
        metrics = c("accuracy")
      )
      
      # let it run
      history <- network %>% fit(
        nn.sc01.loan.train,
        nn.loan.train_y,
        epochs = 25,
        batch_size = 512,
        validation_data = list(nn.sc01.loan.val, nn.loan.val_y)
      )
      
      tst <- network %>% evaluate(nn.sc01.loan.test, nn.loan.test_y)
      
      result <- data.frame(
        row.names = i,
        layers = 8,
        units = i_unit,
        optimizer = i_optimizer,
        activation = lv_activation[i_activation],
        epoch = 25,
        
        loss = tst[1],
        accuracy = tst[2]
      )
      results <- rbind(results,result)
      print(result)
      i <- i + 1
    }
  }
}
write.csv(results,"results8.csv", row.names = FALSE) #todo: results2: layers = 2



# K-Fold cross validation with K = 10
# the following code was run and documented for the following scenarios:
#
# hidden layers | units per layer   | optimizer     | activation function
# 6             | 128               | adam lr=1e-3  | elu
# 3             | 32                | adam lr=1e-2  | elu
# 4             | 256               | adam lr=1e-3  | elu
# 5             | 256               | adam lr=1e-3  | elu
# 5             | 256               | adam lr=1e-3  | relu

set.seed(1)

build_model <- function() {
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu", input_shape = c(26)) %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = optimizer_adam(lr = 1e-3),
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
}


k <- 10
indices <- sample(1:nrow(nn.sc01.loan.k))
folds <- cut(indices, breaks = k, labels = FALSE)

num_epochs <- 150
all_scores <- c()
all_accuracy_histories <- NULL
for(i in 1:k){
  cat("processing fold #", i, "\n")
  
  # Prepare the validation data: data from partition # k
  cat("    prepare data for fold #", i, "\n")
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_data <- nn.sc01.loan.k[val_indices,]
  val_targets <- nn.loan.k_y[val_indices]
  
  # Prepare the training data: data from all other partitions
  partial_train_data <- nn.sc01.loan.k[-val_indices,]
  partial_train_targets <- nn.loan.k_y[-val_indices]
  
  # Build the Keras model (already compiled)
  cat("    build model for fold #", i, "\n")
  model <- build_model()
  
  # Train the model
  cat("    train model for fold #", i, "\n")
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 512
  )
  accuracy_history <- history$metrics$val_accuracy
  all_accuracy_histories <- rbind(all_accuracy_histories, accuracy_history)
}


# compute the average of the per-epoch accuracy for all folds:
average_accuracy_history <- data.frame(
  epoch = seq(1:ncol(all_accuracy_histories)),
  validation_accuracy = apply(all_accuracy_histories, 2, mean)
)

average_accuracy_history[which.max(average_accuracy_history$validation_accuracy),]

ggplot(average_accuracy_history, aes(x = epoch, y = validation_accuracy)) + geom_smooth()


