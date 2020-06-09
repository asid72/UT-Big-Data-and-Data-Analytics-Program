# Title: R_caret_pipeline_basic_regression_WholeYear

# Last update: 2019.09

# File/project name: 2019 R-pipeline-caret-basic-regression-WholeYear.R
# RStudio Project name: See resources for details on R projects

###############
# Project Notes
###############

# Summarize project: This is a simple R pipeline to help understand how to organize
# a project in R Studio using the the caret package. This is a basic pipeline that


###############
# Housekeeping
###############

# Clear objects if necessary
rm(list = ls())

# get working directory
getwd()
# set working directory
setwd("C:\\Users\\asiddiqui\\Documents\\UT Data Analytics Course\\Course Weeks\\Course 2 - Data Analytics Predict Customer Preferences 2017.3\\Task 2\\Files\\")
dir()

# set a value for seed (to be used in the set.seed function)
seed <- 123


################
# Load packages
################

install.packages("caret")
install.packages("lattice")
install.packages("ggplot2")
install.packages("generics")
install.packages("gower")
install.packages("pls")
install.packages("C50")
install.packages("inum")
install.packages("corrplot")
install.packages("readr")
library(caret)
library(C50)
library(readr)
library(corrplot)
#library(doMC)
#library(doParallel)
library(mlbench)
library(readr)


#####################
# Parallel processing
#####################

# NOTE: Be sure to use the correct package for your operating system.

#--- for WIN ---#
#install.packages("doParallel") # install in 'Load packages' section above
library(doParallel)  # load in the 'Load Packages' section above
detectCores()  # detect number of cores
cl <- makeCluster(2)  # select number of cores; 2 in this example
registerDoParallel(cl) # register cluster
getDoParWorkers()  # confirm number of cores being used by RStudio
# Stop Cluster. After performing your tasks, make sure to stop your cluster. 
stopCluster(cl)



###############
# Import data
##############

#### --- Load raw datasets --- ####

# --- Load Train/Existing data (Dataset 1) --- #
surveydataset <- read.csv("CompleteResponses.csv", stringsAsFactors = FALSE)
class(surveydataset)  # "data.frame"
str(surveydataset)



# --- Load Predict/New data (Dataset 2) --- #

# There is no additional dataset for this project



#### --- Load preprocessed datasets --- ####

incompleteSurveyData <- read.csv("SurveyIncomplete.csv", stringsAsFactors = FALSE) 


################
# Evaluate data
################

#--- Dataset 1 ---#
str(surveydataset)  # 35136 obs. of  8 variables 
names(surveydataset)
summary(surveydataset)
head(surveydataset)
tail(surveydataset)

# plot
hist(surveydataset$brand)
plot(surveydataset$brand, surveydataset$salary)
qqnorm(surveydataset$brand)
# check for missing values 
anyNA(surveydataset)
is.na(surveydataset)


#--- Dataset 2 ---#

# If there is a dataset with unseen data to make predictions on, then preprocess here
# to make sure that it is preprossed the same as the training dataset.


#############
# Preprocess
#############

#--- Dataset 1 ---#

# change data types
surveydataset$brand <- as.factor(surveydataset$brand)
summary(surveydataset)
str(surveydataset)
# rename a column
#names(DatasetName)<-c("ColumnName","ColumnName","ColumnName") 

# check for missin values (NAs)
#any(is.na(WholeYear)) 

# handle missing values 
na.omit(surveydataset$brand)
na.exclude(surveydataset$brand)        
#DatasetName$ColumnName[is.na(DatasetName$ColumnName)] <- mean(DatasetName$ColumnName,na.rm = TRUE)

# remove obvious features (e.g., ID, other)
#WholeYear7v <- WholeYear   # make a copy 
#WholeYear7v$X <- NULL   # remove ID
#str(WholeYear7v) # 35136 obs. of  7 variables

# save preprocessed dataset
#write.csv()


#--- Dataset 2 ---#

# Note: Be sure to alwasy procecess DS1 and DS2 (if available) in the same areas 
# of the pipeline
incompleteSurveyData$brand <- as.factor(incompleteSurveyData$brand)
summary(incompleteSurveyData)
str(incompleteSurveyData)


################
# Sampling
################

# ---- Sampling ---- #

# Note: The set.seed function has to be executed immediately preceeding any 
# function that needs a seed value

# Note: For this task, use the 1000 sample, and not the 10%

# 1k sample
#set.seed(seed)
#WholeYear7v1k <- WholeYear7v[sample(1:nrow(WholeYear7v), 1000, replace=FALSE),]
#head(WholeYear7v1k) # ensure randomness
#nrow(WholeYear7v1k) # ensure number of obs
# create 10% sample for 7v ds
#set.seed(seed) # set random seed
#WholeYear7v10p <- WholeYear7v[sample(1:nrow(WholeYear7v), round(nrow(WholeYear)*.1),replace=FALSE),]
#nrow(WholeYear7v10p)
#head(WholeYear10p7v) # ensure randomness


##################
# Train/test sets
##################

# create the training partition that is 75% of total obs
set.seed(seed) # set random seed
inTraining <- createDataPartition(surveydataset$brand, p=0.75, list=FALSE)
# create training/testing dataset
trainSet <- surveydataset[inTraining,]   
testSet <- surveydataset[-inTraining,]   
# verify number of obs 
nrow(trainSet)  
nrow(testSet)   


################
# Train control
################

# set 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


##############
# Train model
##############

?modelLookup()
modelLookup("rf")






## ------- RF ------- ##

# RF train/fit
set.seed(seed)
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))
system.time(rfFit1 <- train(brand~., data=trainSet, method="rf", importance=T, trControl=fitControl, tuneGrid=rfGrid)) #importance is needed for varImp
rfFit1

varImp(rfFit1)
# Importance
# salary    100.0000
# age        68.4653
# credit      1.0899
# zipcode     0.7549
# car         0.4767
# elevel      0.0000




## ------- C50 ------- ##
C50Fit <- train(brand~., data = trainSet, method = "C5.0", trControl=fitControl)
#rfFit
C50Fit
varImp(C50Fit)
# Overall
# age      100.00
# salary   100.00
# credit    84.05
# car       63.08
# zipcode    0.00
# elevel     0.00



#################
# Evaluate models
#################

##--- Compare models ---##

# use resamples to compare model performance
ModelFitResults1k <- resamples(list( rf=rfFit1, c5.0dt=C50Fit))

# output summary metrics for tuned models 
summary(ModelFitResults1k)
#Accuracy 
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#rf     0.8977120 0.9124605 0.9198666 0.9189120 0.9265753 0.9380888    0
#c5.0dt 0.8975741 0.9131898 0.9170595 0.9182358 0.9226110 0.9353970    0

#Kappa 
#            Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#rf     0.7837237 0.8142685 0.8307992 0.8281564 0.8442956 0.8689142    0
#c5.0dt 0.7780210 0.8156518 0.8249572 0.8260137 0.8336088 0.8637822    0



##--- Conclusion ---##






########################
# Validate top model
########################
#decided to Pick C50 due to higher Kappa and Accuracy Values
# make predictions
rfPred1 <- predict(C50Fit, testSet)
#rfPred1 <- predict(rfFit1, testSet)
# performace measurment
postResample(rfPred1, testSet$brand)

summary(rfPred1)
# plot predicted verses actual
plot(rfPred1,testSet$brand)
# print predictions
rfPred1


########################
# Predict with top model
########################

# make predictions
rfPred1 <- predict(C50Fit, incompleteSurveyData)
postResample(rfPred1, incompleteSurveyData$brand)
plot(rfPred1,incompleteSurveyData$brand)

print(rfPred1)
summary(rfPred1)
summary(incompleteSurveyData$brand)
summary(incompleteSurveyData$brand)


##########################
#Export to CSV 
#########################

PredictedSurvey <- incompleteSurveyData
PredictedSurvey$PredictedBrand <- rfPred1
write.csv(PredictedSurvey,file="PredictedSurvey.csv")
str(rfPred1)
rfPred1
max.print(rfPred1)



########################
# Save validated model
########################

##--- Save top performing model ---##

# save top performing model after it has been validated
C50Fit
# save model 
saveRDS(C50Fit,file="C50Fit.rds")  # Q: What type of object does saveRDS create?

# load and name model to make predictions with new data
RFfit1 <- readRDS(file="C50Fit.RDS") # Q: What type of object does readRDS create?
















