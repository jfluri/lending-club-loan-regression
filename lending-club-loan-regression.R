#####################################################################################
# Data Science
# Providing a regression model for the interest rate - int_rate
# Jasmin Fluri, Roman Fischer, Sebastian Geiger, Thomas Probst
#####################################################################################


########## COACHING QUESTIONS ##########
# How can results or modelling attempts be saved to be able to compare them later on and 
# have an overview of the different attempts that have been made. #292


######################################################################################
# DATA INFORMATION 
######################################################################################

# Understand the data:
# https://www.kaggle.com/pragyanbo/a-hitchhiker-s-guide-to-lending-club-loan-data

######################################################################################
# Before starting with the data preparation, check for missing libraries
######################################################################################

libraries_used<-
  c("MASS","varImp","plyr", "dplyr", "glmnet", "caret", "tidyverse", "funModeling", "leaps", "corrplot", "car","randomForest", "mlbench","tidyr")

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
allData<- read.csv("regression_train_loan.csv",header= TRUE,sep = ",",quote="\"",dec=".",fill= TRUE,na.strings= "NA",blank.lines.skip= TRUE)

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
# TODO: Doesn't work!
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

# Print the summary of the new smaller dataset
summary(loan)

######################################################################################
# DATA CLEANING
######################################################################################

# In order to count NA, we first have to get rid of all blank or "n/a" (e.g. in emp_length) values.
loan[loan==""] <- NA
loan[loan=="n/a"] <- NA

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

# Remove rows with more than 70% NA Values --> There are none in our dataset

# 44 Rows leads to 31 NA Values to reach the 70% threshold
loan$na_count <- apply(loan, 1, function(x) sum(is.na(x)))

# TODO: Show overview

# Highest Values --> 14 NA Values, therefore no general Action required --> na_count column is removed
loan <- loan[,-c(45)]

# Drop rows that have only a few NA's in a specific column --> We are not doing this as long as we don't have problems
# loan <- loan %>% drop_na(delinq_2yrs)

#list occurrences of values in specific fields to spot anomalies, blank values, etc.
#as.data.frame(table(loan$grade)) 


######################################################################################
# MODIFICATION OF DATE COLUMNS
######################################################################################
# Dates have to be converted into categorical attributes for the regression

# convert dates to year without days and month -> YYYY
loan$issue_d <- substr(loan$issue_d,5,8)
loan$earliest_cr_line <- substr(loan$earliest_cr_line,5,8)
loan$last_pymnt_d <- substr(loan$last_pymnt_d,5,8)
loan$next_pymnt_d <- substr(loan$next_pymnt_d,5,8)
loan$last_credit_pull_d <- substr(loan$last_credit_pull_d,5,8)

# Print summary of last_pymnt_d
summary(loan$last_pymnt_d)

# summary(loan$last_pymnt_d) --> Based on this output we changed the values to ordinal

# Make year values ordinal (string to ordinal) of the date fields
# The order = TRUE puts the years in the correct order
loan$issue_d <- factor(loan$issue_d, order = TRUE)
loan$earliest_cr_line <- factor(loan$earliest_cr_line, order = TRUE)
loan$last_pymnt_d <- factor(loan$last_pymnt_d, order = TRUE)
loan$next_pymnt_d <- factor(loan$next_pymnt_d, order = TRUE)
loan$last_credit_pull_d <- factor(loan$last_credit_pull_d, order = TRUE)

# check the distribution of the grade variable
as.data.frame(table(loan$grade)) 

# Convert loan$grade to ordinal and put it into the correct order
loan$grade <- factor(loan$grade, order = TRUE) 

# check emp_length - the categories are strings and have to be converted to numeric
as.data.frame(table(loan$emp_length)) 

# Convert emp_length to numeric values - Only the numbers are extracted
loan$emp_length <- as.numeric(gsub("[^0-9.]", "",  loan$emp_length))

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


######################################################################################
# MISSING VALUE TREATMENT
######################################################################################

# For the emp_length value: we assume that missing values are values smaller than 1 year
# Therefore we fill missing values with 0 and set NA to 0 
loan$emp_length[is.na(loan$emp_length)] <- 0 

# Replace NA Values with mean (for numerical values) or with the mode (for categorical values) 
# in columns that have less than 70% NA

# for numerical values the mean is set instead of the missing value for the following columns
# tot_cur_bal - 43
loan[is.na(loan[,43]), 43] <- mean(loan[,43], na.rm = TRUE) 

# total_rev_hi_lim - 44
loan[is.na(loan[,44]), 44] <- mean(loan[,44], na.rm = TRUE) 

# tot_coll_amt - 42
loan[is.na(loan[,42]), 42] <- mean(loan[,42], na.rm = TRUE) 

# mths_since_last_deling - 20
loan[is.na(loan[,20]), 20] <- mean(loan[,20], na.rm = TRUE) 

# revol_bal - 23
loan[is.na(loan[,23]), 23] <- mean(loan[,23], na.rm = TRUE) 

# revol_util - 24
loan[is.na(loan[,24]), 24] <- mean(loan[,24], na.rm = TRUE) 

# total_acc - 25
loan[is.na(loan[,25]), 25] <- mean(loan[,25], na.rm = TRUE) 

# annual_inc - 10
loan[is.na(loan[,10]), 10] <- mean(loan[,10], na.rm = TRUE) 

# delinq_2yrs - 17
loan[is.na(loan[,17]), 17] <- mean(loan[,17], na.rm = TRUE) 

# inq_last_6mths - 19
loan[is.na(loan[,19]), 19] <- mean(loan[,19], na.rm = TRUE) 

# open_acc - 21
loan[is.na(loan[,21]), 21] <- mean(loan[,21], na.rm = TRUE) 

# pub_rec - 22
loan[is.na(loan[,22]), 22] <- mean(loan[,22], na.rm = TRUE) 


# for categorical values the mode is set instead of the missing value for the following columns


# set the mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# earliest_cr_line - 18
loan[is.na(loan[,18]), 18] <- Mode(loan[,18]) 

# next_pymnt_d - 37
loan[is.na(loan[,37]), 37] <- Mode(loan[,37]) 

# last_pymnt_d - 35
loan[is.na(loan[,35]), 35] <- Mode(loan[,35]) 

# last_credit_pull_d - 38
loan[is.na(loan[,38]), 38] <- Mode(loan[,38]) 

# collections_12_mths_ex_med - 39
loan[is.na(loan[,39]), 39] <- Mode(loan[,39]) 

# acc_now_delinq - 41
loan[is.na(loan[,41]), 41] <- Mode(loan[,41]) 


######################################################################################
# ATTRIBUTE IMPORTANCE
######################################################################################
# Overview of possibilities: https://www.linkedin.com/pulse/how-find-most-important-variables-r-amit-jain/
# Getting the attribute importance of variables in our dataset


# fit the random forest with default parameter
regressor <- randomForest(int_rate ~ . , data = loan, importance=TRUE, prOximity=TRUE) #,na.action=na.roughfix 

# get variable importance, based on mean decrease in accuracy
caret::varImp(regressor) 

# conditional=True, adjusts for correlations between predictors
caret::varImp(regressor, conditional=TRUE) 

# scale = FALSE doesn't scale the results to 100
caret::varImp(regressor, scale = FALSE)



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


######################################################################################
# Feature selection
######################################################################################
#removes values that have a correlation
loan <- subset(loan, select=-c(loan_amnt, funded_amnt, funded_amnt_inv, total_pymnt, total_pymnt_inv, out_prncp,
                               total_rec_prncp, revol_bal, total_acc, recoveries)) 

#removes values that have a high percentage of zeros
loan <- subset(loan, select=-c(tot_coll_amt, acc_now_delinq, collections_12_mths_ex_med, collection_recovery_fee, 
                               total_rec_late_fee, pub_rec, delinq_2yrs		))



########## SPLITTING TEST/TRAINING DATA ##########

train=sample(nrow(loan),nrow(loan)*0.8) # indices of a training data (80%)
loan.Train <- loan[train,] # training data 
loan.Test <- loan[-train,] # test data



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
mymodel <- lm(int_rate~.,data=loan.Train)
ourmodels <- list(mymodel)
summary(ourmodels[[1]])
vif(ourmodels[[1]])
plot(ourmodels[[1]])
# Notice the points fall along a line in the middle of the graph, but curve off in the extremities.
# Normal Q-Q plots that exhibit this behavior usually mean your data have more extreme values than
# would be expected if they truly came from a Normal distribution.

mymodel <- lm(int_rate~grade,data=loan.Train)
append(ourmodels,mymodel, after = length(ourmodels))
ourmodels[[2]] <- mymodel
summary(ourmodels[[2]])
plot(ourmodels[[2]])


#This stepAIC approach runs quite long and does not provide a good model
#fit <- lm(int_rate~., data=loan[complete.cases(loan),])
#step <- stepAIC(fit, direction = "both")
#step$anova


# Ridge & Lasso Regression

predictors <- model.matrix(int_rate ~ ., loan)[,-1] #TODO: we only get 10 rows, I guess because of all other rows containing NAs(?)
head(predictors)
outputs <- loan$int_rate

m_ridge <- glmnet(predictors, outputs, alpha = 0) # calling ridge regression by using alpha=0.
#TODO: this causes an error because the numbers don't match. Problem because of NAs?

