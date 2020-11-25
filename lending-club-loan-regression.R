###############################################################
# Data Science
# Providing a regression model for the interest rate - int_rate
# Jasmin Fluri, Roman Fischer, Sebastian Geiger, Thomas Probst
###############################################################


########## COACHING QUESTIONS ##########





########## PRELIMINARIES ##########

# Understand the data:
# https://www.kaggle.com/pragyanbo/a-hitchhiker-s-guide-to-lending-club-loan-data

#Check for missing libraries
libraries_used<-
  c("MASS","varImp","plyr", "dplyr", "glmnet", "caret", "tidyverse", "funModeling", "leaps", "corrplot", "car","randomForest", "mlbench")

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

# Set the working directory to the folder with the data
setwd("C:/Users/jasmi/Dropbox/MSc-FHNW/Modules/_DataScience/Assignment") #Jasmin
setwd("C:/Users/Thoems/OneDrive/Studium/Master/Data Science/R scripts/Assignment for regression") #Thomas PC
setwd("E:/OneDrive/Studium/Master/Data Science/R scripts/Assignment for regression") #Thomas Laptop
setwd("C:/Users/roman/Desktop/FHNW/Data Science/Assignment") #Roman
setwd("C:/Users/Sebastian/OneDrive/School_Master/DataScience/Assignment") #Sebi

set.seed (1)

# Load csv file into data object
allData<- read.csv("regression_train_loan.csv",header= TRUE,sep = ",",quote="\"",dec=".",fill= TRUE,na.strings= "NA",blank.lines.skip= TRUE)

head(allData)


########## FEATURE ENGINEERING ##########

#Get numbers of unique values
meta_loans <- funModeling::df_status(allData, print_results = FALSE)
knitr::kable(meta_loans)

meta_loans <-
  meta_loans %>%
  mutate(uniq_rat = unique / nrow(allData))
#Get unique values in percentage
meta_loans %>%
  select(variable, unique, uniq_rat) %>%
  mutate(unique = unique, uniq_rat = scales::percent(uniq_rat)) %>%
  knitr::kable()

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

# Remove columes with more than 70% NA Values
loan <- loan[,-c(21,41,43,44,48,49,50,51,52,53,54,55,56,57,58,60,61,62)]

# Remove rows with more than 70% NA Values
# 44 Rows leads to 31 NA Values to reach the 70% threshold
loan$na_count <- apply(loan, 1, function(x) sum(is.na(x)))

# Highest Values --> 14 NA Values, therefore no general Action required -->na_count row removed
loan <- loan[,-c(45)]



#list occurrences of values in specific fields to spot anomalies, blank values, etc.
#as.data.frame(table(loan$grade)) 


# convert dates to year -> YYYY
loan$issue_d <- substr(loan$issue_d,5,8)
loan$earliest_cr_line <- substr(loan$earliest_cr_line,5,8)
loan$last_pymnt_d <- substr(loan$last_pymnt_d,5,8)
loan$next_pymnt_d <- substr(loan$next_pymnt_d,5,8)
loan$last_credit_pull_d <- substr(loan$last_credit_pull_d,5,8)





####
# Documentation
# summary(loan$last_pymnt_d) --> Based on this output we changed the values to ordinal
####

# Make year values ordinal (string to ordinal)
loan$issue_d <- factor(loan$issue_d, order = TRUE)
loan$earliest_cr_line <- factor(loan$earliest_cr_line, order = TRUE)
loan$last_pymnt_d <- factor(loan$last_pymnt_d, order = TRUE)
loan$next_pymnt_d <- factor(loan$next_pymnt_d, order = TRUE)
loan$last_credit_pull_d <- factor(loan$last_credit_pull_d, order = TRUE)


as.data.frame(table(loan$grade)) #check grade
loan$grade <- factor(loan$grade, order = TRUE) #make loan$grade ordinal

as.data.frame(table(loan$emp_length)) #check emp_length: convert it to numeric
loan$emp_length <- as.numeric(gsub("[^0-9.]", "",  loan$emp_length))

as.data.frame(table(loan$home_ownership)) #check home_ownership
#TODO: should we get rid of "ANY" as it only occurs 2 times? What about NONE (45) and OTHER (155)? (downsampling)
#todo: plot against int_rate and decide whether significant or not. if not: delete the rows, ignore the OTHER category.

as.data.frame(table(loan$verification_status)) #check verification_status: this looks good: only 3 categories

as.data.frame(table(loan$loan_status)) #check loan_status
#TODO: downsampling, but how? check:
#https://triamus.github.io/project/lending-club-loan-data-in-r/#grade
# punkt 6. careful, we have correlations to other columns.

as.data.frame(table(loan$purpose)) #check purpose. leave it as it is

as.data.frame(table(loan$addr_state)) #check state: remove them from the data set because of too many levels (51). check dependency to int_rate
#TODO: question for coaching.
loan <- subset(loan, select = -c(addr_state))


## define how to clean the following datapoints:

# convert string fields to number fields

#TODO: earliest_cr_line
#TODO: mths_since_last_deling - a lot of NAs




#regressor <- randomForest(int_rate ~ .,data = loan, importance=TRUE)
#varImp(regressor) #get variable importance, based on mean decrease in accuracy
#varImp(regressor, conitional = TRUE) #conditional = TRUE, adjusts for correlations between predictors
#varImpAuc(regressor) #more robust towards class imbalance


########## MISSING VALUE TREATMENT ##########

# TODO: List attributes that need missing value treatment and are important for the analysis

#Replace NA Values with mean at columns that have less than 70% NA
# TODO: with: tot_cur_bal, total_rev_hi_lim, tot_cur_bal, revol_bal others??? 

for(i in 1:ncol(loan)){
  loan[is.na(loan[,i]), i] <- mean(loan[,i], na.rm = TRUE)
}

#TODO: question for coaching: what to do with mths_since_last_delinq?


# emp_length: assume that the borrower hasn't worked many years for his data to be recorded. Therefore fill missing values with 0
loan$emp_length[is.na(loan$emp_length)] <- 0 #NA to 0

#Convert variable application type as factor variable
loan$application_type <- factor(loan$application_type)


##########Feature selection##########

#get linear model for all variables
mymodel <- lm(int_rate~.,data=loan)
summary(mymodel)
vif(mymodel)

fit <- lm(int_rate~., data=loan[complete.cases(loan),])
step <- stepAIC(fit, direction = "both")
step$anova

#function to get all numerical variables
num_vars <- 
  loan %>%
  sapply(is.numeric) %>%
  which() %>%
  names()

meta_train <- funModeling::df_status(loan, print_results = FALSE)

meta_train %>%
  select(variable, p_zeros, p_na, unique) %>%
  filter_(~ variable %in% num_vars ) %>%
  knitr::kable() 

#Show numerical variables with correlation as pie chart
corrplot::corrplot(cor(loan[, num_vars], use = "complete.obs"),
                   method = "pie", type = "upper")
loan.cor <- cor(loan[, num_vars])


#cor(loan[, num_vars])

#find variables with high correlation
caret::findCorrelation(cor(loan[, num_vars], use = "complete.obs"),
                       names = TRUE, cutoff = .6)

vars_to_remove <-
  c("loan_amnt", "funded_amnt", "funded_amnt_inv", "installment", "total_pymnt", "total_pymnt_inv", "out_prncp",
    "total_rec_prncp", "revol_bal", "total_acc", "recoveries", "delinq_2yrs")

loan <- loan %>% select(-one_of(vars_to_remove))

########## SPLITTING TEST/TRAINING DATA ##########

train=sample(nrow(loan),nrow(loan)*0.8) # indices of a training data (80%)
loan.Train <- loan[train,] # training data 
loan.Test <- loan[-train,] # test data


########## SUBSET SELECTION / FEATURE ANALYSIS & ENGINEERING ##########

#TODO:check last semester approach: which attributes are most important for prediction?

# Best Subset Selection


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

predictors <- model.matrix(int_rate ~ ., loan)[,-1] #TODO: we only get 10 rows, I guess because of all other rows containing NAs(?)
head(predictors)
outputs <- loan$int_rate

m_ridge <- glmnet(predictors, outputs, alpha = 0) # calling ridge regression by using alpha=0.
#TODO: this causes an error because the numbers don't match. Problem because of NAs?

########## ANALYSIS ##########
