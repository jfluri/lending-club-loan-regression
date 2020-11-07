###############################################################
# Data Science
# Providing a regression model for the interest rate - int_rate
# Jasmin Fluri, Roman Fischer, Sebastian Geiger, Thomas Probst
###############################################################


########## PRELIMINARIES ##########

# Set the working directory to the folder with the data
setwd("C:/Users/jasmi/Dropbox/MSc-FHNW/Modules/_DataScience/Assignment") #Jasmin
setwd("E:/OneDrive/Studium/Master/Data Science/R scripts/Assignment for regression") #Thomas

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

loan$grade <- factor(loan$grade, order = TRUE) #make loan$grade ordinal
#TODO: emp_length
#TODO: issue_d
#TODO: addr_state ?
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


########## SPLITTING TEST/TRAINING DATA ##########

train=sample(nrow(loan),nrow(loan)*0.8) # indices of a training data (80%)
loan.Train <- loan[train,] # training data 
loan.Test <- loan[-train,] # test data


########## SUBSET SELECTION ##########

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
