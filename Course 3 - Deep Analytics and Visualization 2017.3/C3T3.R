
###############
# Housekeeping
###############

# Clear objects if necessary
rm(list = ls())

# get working directory
getwd()
# set working directory
setwd("C:\\Users\\asiddiqui\\Documents\\UT Data Analytics Course\\Course Weeks\\Course 3 - Deep Analytics and Visualization 2017.3\\Task 3\\UJIndoorLoc")
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
install.packages("dplyr")
install.packages("tidyr")

install.packages("gdata")
library(gdata)
library(caret)
library(C50)
library(readr)
library(corrplot)
#library(doMC)
#library(doParallel)
library(mlbench)
library(readr)
library(dplyr)
library(tidyr)


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
Trainingdataset <- read.csv("trainingData.csv", stringsAsFactors = FALSE)
#Trainingdataset <- read.csv("trainingData.csv")
class(Trainingdataset)  # "data.frame"
str(Trainingdataset)



################
# Evaluate data
################

#--- Dataset 1 ---#
str(Trainingdataset)  # 19937  obs. of  529 variables 
names(Trainingdataset)

summary(Trainingdataset)
head(Trainingdataset)
tail(Trainingdataset)

# plot
hist(Trainingdataset$SPACEID)
hist(Trainingdataset$WAP204)
hist(Trainingdataset$WAP516)
plot(Trainingdataset$BUILDINGID,Trainingdataset$SPACEID)
plot(Trainingdataset$BUILDINGID,Trainingdataset$FLOOR)
qqnorm(Trainingdataset$SPACEID)
# check for missing values 
anyNA(Trainingdataset)
is.na(Trainingdataset)

################
# Sampling
################


set.seed(seed) # set random seed
nrow(Trainingdataset)


#different record sizes for each building
TrainingdatasetB1 <- subset(Trainingdataset,BUILDINGID == "1")
summary(TrainingdatasetB1)
nrow(TrainingdatasetB1) #5196

TrainingdatasetB2 <- subset(Trainingdataset,BUILDINGID == "2")
summary(TrainingdatasetB2)
nrow(TrainingdatasetB2) #9492

TrainingdatasetB0 <- subset(Trainingdataset,BUILDINGID == "0")
summary(TrainingdatasetB0)
nrow(TrainingdatasetB0) #5249

#Choose Building 1 as a Sample Set


TDSB <- TrainingdatasetB1
anyNA(TDSB)
is.na(TDSB)




#############
# Preprocess
#############

TDSB_Loc <- unite(TDSB,"LOCATIONID",c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION"), remove = TRUE)
TDSB_Loc$LOCATIONID <- as.factor(TDSB_Loc$LOCATIONID)
summary(TDSB_Loc)
str(TDSB_Loc)
str(TDSB_Loc, list.len=ncol(TDSB_Loc)) #Full List

# remove obvious features (e.g., ID, other)
TDSB_Loc$USERID <- NULL #Remove UserID
TDSB_Loc$PHONEID <- NULL #Remove PhoneID
TDSB_Loc$TIMESTAMP <- NULL #Remove Timestamp
TDSB_Loc$LONGITUDE <- NULL #Remove Longitude
TDSB_Loc$LATITUDE <- NULL #Remove Latitude

#Removing Factors that make up LocationID
TDSB_Loc$BUILDINGID <- NULL
TDSB_Loc$SPACEID <- NULL
TDSB_Loc$FLOOR <- NULL
TDSB_Loc$RELATIVEPOSITION <- NULL


str(TDSB_Loc) # 5196 obs. of  523 variables
str(TDSB_Loc, list.len=ncol(TDSB_Loc)) #Full List


##################
# Train/test sets
##################

# create the training partition that is 75% of total obs
set.seed(seed) # set random seed
inTraining <- createDataPartition(TDSB_Loc$LOCATIONID, p=0.75, list=FALSE)
# create training/testing dataset
trainSet <- TDSB_Loc[inTraining,]   
testSet <- TDSB_Loc[-inTraining,]   
# verify number of obs 
nrow(trainSet)  

nrow(testSet)   

is.na(trainSet)
anyNA(trainSet)

################
# Train control
################

# set 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


##############
# Train model
##############


## ------- DT ------- ##

# DT train/fit
set.seed(seed)
TBFit1 <- train(LOCATIONID~., data=trainSet, method="treebag", trControl=fitControl)
TBFit1
summary(TBFit1)


## ------- C50 ------- ##
C50Fit <- train(LOCATIONID~., data = trainSet, method = "C5.0", trControl=fitControl)
#rfFit
C50Fit
varImp(C50Fit)


## ------- RF ------- ##

# RF train/fit

set.seed(seed)
rfFit1 <- train(LOCATIONID~., data=trainSet, method="rf", trControl=fitControl) #importance is needed for varImp
rfFit1

varImp(rfFit1)




#################
# Evaluate models
#################

##--- Compare models ---##

# use resamples to compare model performance
ModelFitResults1k <- resamples(list( tb=TBFit1, c5.0dt=C50Fit, rf=rfFit1))

# output summary metrics for tuned models 
summary(ModelFitResults1k)
dotplot(ModelFitResults1k)
# 
# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# tb     0.7786260 0.7952088 0.8114385 0.8087232 0.8232924 0.8312020    0
# c5.0dt 0.7844612 0.7974265 0.8043045 0.8050077 0.8080368 0.8299492    0
# rf     0.8155844 0.8284600 0.8391486 0.8377007 0.8480518 0.8593350    0
# 
# Kappa 
#             Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# tb     0.7772704 0.7939729 0.8102807 0.8075484 0.8222389 0.8301533    0
# c5.0dt 0.7831494 0.7962314 0.8031374 0.8038257 0.8068606 0.8288922    0
# rf     0.8144528 0.8274224 0.8381637 0.8366997 0.8471194 0.8584639    0




##--- Conclusion ---##






#################################
# Make Predictions on All Models
#################################
# make predictions
c50Pred1 <- predict(C50Fit, testSet)
TBPred1 <- predict(TBFit1, testSet)
rfPred1 <- predict(rfFit1, testSet)

########################
# Performance Metrics
########################

confusiontreec50 <- confusionMatrix(reference = testSet$LOCATIONID, 
                                   data = c50Pred1, mode='everything')

c50performances <- as.matrix(confusiontreec50, what = "classes")
write.csv(c50performances,file="c50performances.csv")

confusiontreeTB <- confusionMatrix(reference = testSet$LOCATIONID, 
                                    data = TBPred1, mode='everything')

TBperformances <- as.matrix(confusiontreeTB, what = "classes")
write.csv(TBperformances,file="TBperformances.csv")


confusiontreerf <- confusionMatrix(reference = testSet$LOCATIONID, 
                                    data = rfPred1, mode='everything')

rfperformances <- as.matrix(confusiontreerf, what = "classes")
write.csv(rfperformances,file="rfperformances.csv")



#############################
## Metrics on Predictions
#############################
postResample(rfPred1, testSet$LOCATIONID)
# Accuracy     Kappa 
# 0.8360390 0.8350173 

postResample(TBPred1, testSet$LOCATIONID)
# Accuracy     Kappa 
# 0.8043831 0.8031773 

postResample(c50Pred1, testSet$LOCATIONID)
# Accuracy     Kappa 
# 0.7849026 0.7835853 


#Top Model Choice RF
summary(rfPred1)
rfPred1




#####################################
#Export All Model Predictions to CSV 
#####################################

# Add Predicted column to DataFrames
PredicatedDatasetc50 <- testSet
PredicatedDatasetc50$PredLocationID <- c50Pred1

PredicatedDatasetTB <- testSet
PredicatedDatasetTB$PredLocationID <- TBPred1

PredicatedDatasetrf <- testSet
PredicatedDatasetrf$PredLocationID <- rfPred1

# Export Predictions to CSV
write.csv(PredicatedDatasetc50,file="PredictedDatasetc50.csv")
write.csv(PredicatedDatasetTB,file="PredictedDatasetTB.csv")
write.csv(PredicatedDatasetrf,file="PredictedDatasetrf.csv")



















