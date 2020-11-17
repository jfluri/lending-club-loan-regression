###############################################################
# Data Science
# Providing a regression model for the interest rate - int_rate
# Jasmin Fluri, Roman Fischer, Sebastian Geiger, Thomas Probst
###############################################################


########## PRELIMINARIES ##########

# Understand the data:
# https://www.kaggle.com/pragyanbo/a-hitchhiker-s-guide-to-lending-club-loan-data

# Set the working directory to the folder with the data
setwd("C:/Users/jasmi/Dropbox/MSc-FHNW/Modules/_DataScience/Assignment") #Jasmin
setwd("C:/Users/Thoems/OneDrive/Studium/Master/Data Science/R scripts/Assignment for regression") #Thomas PC
setwd("E:/OneDrive/Studium/Master/Data Science/R scripts/Assignment for regression") #Thomas Laptop
setwd("C:/Users/roman/Desktop/FHNW/Data Science/Assignment") #Roman

set.seed (1)

# Load csv file into data object
allData<- read.csv("regression_train_loan.csv",header= TRUE,sep = ",",quote="\"",dec=".",fill= TRUE,na.strings= "NA",blank.lines.skip= TRUE)

head(allData)


########## FEATURE ENGINEERING ##########

#Remove the following:
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

summary(loan)


#TODO Remove attribute with >70% NA

# in order to count NA, we first have to get rid of all blank or "n/a" (e.g. in emp_length) values.
loan[loan==""] <- NA
loan[loan=="n/a"] <- NA

loan.column <- as.data.frame(colnames(loan))
loan.column$NAs <- as.data.frame(sapply(loan, function(x) sum(is.na(x))))[,1] #store the count of NA of all columns.
loan.column$NA_percent <- loan.column$NAs / nrow(loan) #NA in %

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




# convert dates to year -> YYYY
loan$issue_d <- substr(loan$issue_d,5,8)
loan$earliest_cr_line <- substr(loan$earliest_cr_line,5,8)
loan$last_pymnt_d <- substr(loan$last_pymnt_d,5,8)
loan$next_pymnt_d <- substr(loan$next_pymnt_d,5,8)
loan$last_credit_pull_d <- substr(loan$last_credit_pull_d,5,8)


loan$grade <- factor(loan$grade, order = TRUE) #make loan$grade ordinal

#list occurrences of values in specific fields to spot anomalies, blank values, etc.
as.data.frame(table(loan$next_pymnt_d)) 

## define how to clean the following datapoints:


# convert string fields to number fields

#TODO: emp_length
#TODO: issue_d
#TODO: addr_state ? --> Find a solution -> could doom the model - might have to be removed
#TODO: earliest_cr_line
#TODO: mths_since_last_deling - a lot of NAs
#TODO: mths_since_last_record - a lot of NAs
#TODO: last_pymnt_d
#TODO: next_pymnt_d
#TODO: last_credit_pull_d
#TODO: mths_since_last_major_derog - a lot of NAs
#TODO:annual_inc_joint - a lot of NAs
#TODO: dti_joint - a lot of NAs
#TODO: open_acc_6m - a lot of NAs
#TODO: open_il_6m - a lot of NAs
#TODO: open_il_12m - a lot of NAs

#general question: are a lot of NAs in a column bad?


########## MISSING VALUE TREATMENT ##########

# TODO: List attributes that need missing value treatment and are important for the analysis



########## SPLITTING TEST/TRAINING DATA ##########

train=sample(nrow(loan),nrow(loan)*0.8) # indices of a training data (80%)
loan.Train <- loan[train,] # training data 
loan.Test <- loan[-train,] # test data


########## SUBSET SELECTION / FEATURE ANALYSIS & ENGINEERING ##########


# Best Subset Selection
library("leaps")

#sets <- regsubsets(int_rate ~ ., loan, nvmax = 12, really.big=T)
# => 1233 linear dependencies found. We could try with really.big=T but this will run for hours.


# Forward Stepwise Selection
#sets_FWS <- regsubsets(int_rate ~ ., loan, nvmax = 12, method = "forward")
# => ??


# Backward Stepwise Selection
#sets_BWS <- regsubsets(int_rate ~ ., loan, nvmax = 5, method = "backward")
# => ??

# Hybrid Stepwise Methods(?)


########## REGRESSION ##########

# Ridge & Lasso Regression
library("glmnet")

predictors <- model.matrix(int_rate ~ ., loan)[,-1] #TODO: we only get 10 rows, I guess because of all other rows containing NAs(?)
head(predictors)
outputs <- loan$int_rate

m_ridge <- glmnet(predictors, outputs, alpha = 0) # calling ridge regression by using alpha=0.
#TODO: this causes an error because the numbers don't match. Problem because of NAs?

########## ANALYSIS ##########
