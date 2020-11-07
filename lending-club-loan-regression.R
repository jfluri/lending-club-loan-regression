# Providing a regression model for the interest rate - int_rate

# Set the working directory to the folder with the data
setwd("C:/Users/jasmi/Dropbox/MSc-FHNW/Modules/_DataScience/Assignment") #Jasmin
setwd("E:/OneDrive/Studium/Master/Data Science/R scripts/Assignment for regression") #Thomas

set.seed (1)

# Load csv file into data object
allData<- read.csv("regression_train_loan.csv",header= TRUE,sep = ",",quote="\"",dec=".",fill= TRUE,na.strings= "NA",blank.lines.skip= TRUE)

head(allData)

# Feature Engineering

#Remove the following:
# 1: X
# 2: id
# 3: member_id
# 8: int_rate (as we want to predict it)
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
loan <- allData[,-c(1,2,3,8,11,12,19,20,21,23,24,37,53,57)]

# Missing Value Treatment



# Splitting Test/Training Data


# Regression


# Analysis
