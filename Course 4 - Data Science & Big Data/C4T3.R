
###############
# Housekeeping
###############

# Clear objects if necessary
rm(list = ls())

# get working directory
getwd()
# set working directory
setwd("C:\\Users\\asiddiqui\\Documents\\UT Data Analytics Course\\Course Weeks\\Course 4 - Data Science & Big Data\\Task 3\\")
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
install.packages("plotly")

# Reference Libraries
library(plotly)
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

#Load Iphone Small Matrix
iphoneSmallMatix8d <- read.csv("iphone_smallmatrix_labeled_8d.csv", stringsAsFactors = FALSE)
class(iphoneSmallMatix8d)  # "data.frame"
str(iphoneSmallMatix8d)


#Load Galaxy Small Matrix
galaxySmallMatix9d <- read.csv("galaxy_smallmatrix_labeled_9d.csv", stringsAsFactors = FALSE)
class(galaxySmallMatix9d)  # "data.frame"
str(galaxySmallMatix9d)

#Load Large Matrix
LargeMatrix <- read.csv("LargeMatrix.csv", stringsAsFactors = FALSE)
class(LargeMatrix)  # "data.frame"
str(LargeMatrix)

################
# Evaluate data
################

#--- Dataset Iphone Small Matrix ---#
str(iphoneSmallMatix8d)  # 12973 obs. of  59 variables 
names(iphoneSmallMatix8d)
iphoneSmallMatix8d$iphonesentiment <- as.factor(iphoneSmallMatix8d$iphonesentiment)
summary(iphoneSmallMatix8d)
head(iphoneSmallMatix8d)
tail(iphoneSmallMatix8d)
plot_ly(iphoneSmallMatix8d, x= ~iphoneSmallMatix8d$iphonesentiment, type='histogram')

#Check for NULLS
anyNA(iphoneSmallMatix8d)
is.na(iphoneSmallMatix8d)
is.null(iphoneSmallMatix8d)

#--- Dataset Galaxy Small Matrix ---#
str(galaxySmallMatix9d)  # 12973 obs. of  59 variables 
names(galaxySmallMatix9d)
galaxySmallMatix9d$galaxysentiment <- as.factor(galaxySmallMatix9d$galaxysentiment)
summary(galaxySmallMatix9d)
head(galaxySmallMatix9d)
tail(galaxySmallMatix9d)
plot_ly(galaxySmallMatix9d, x= ~galaxySmallMatix9d$galaxysentiment, type='histogram')

#Check for NULLS
anyNA(galaxySmallMatix9d)
is.na(galaxySmallMatix9d)
is.null(galaxySmallMatix9d)

#--- Dataset Large Matrix ---#
str(LargeMatrix) #28001 obs. of  59 variables
names(LargeMatrix)
head(LargeMatrix)
tail(LargeMatrix)

#Check for NULLS
anyNA(LargeMatrix)
is.na(LargeMatrix)
is.null(LargeMatrix)


#####################
# Feature Selection #
#####################

##----Out of the Box Datasets----##

###Iphone OOB:
iphoneS_OOB <- iphoneSmallMatix8d

###Galaxy OOB:
galaxyS_OOB <- galaxySmallMatix9d

###Large Matrix:
LargeM_OOB <- LargeMatrix


##----Correlation Datasets----##

#Iphone Correlation:
iphoneSC <- iphoneS_OOB
iphoneSC_cor <- cor(iphoneSC)
corrplot(iphoneSC_cor)
corrplot(iphoneSC_cor,method="number",type = "upper")
corrplot(iphoneSC_cor,method="number",type = "upper",number.digits = 2,number.cex=0.7, tl.cex=0.7)

# Remove any Feature highly correlated to the Dependent Variable
# 0.95 
#N/A

# Remove any idependent features that are highly correlated
#0.90
#iphoneSC$iphone # 0.92
iphoneSC$ios <- NULL# 0.92 Removed

#iphoneSC$nokiacamneg # 0.9
iphoneSC$nokiacampos <- NULL# 0.9 Removed

iphoneSC$nokiacamunc <- NULL# 0.94 Removed
#iphoneSC$nokiacampos #0.94

#iphoneSC$htcdispos #.98
iphoneSC$htcphone <- NULL#.98 Removed

iphoneSC$sonydisneg <- NULL# .9 Removed
#iphoneSC$sonydispos # .9

iphoneSC$samsungdisunc <- NULL#.91 Removed
#iphoneSC$samsungdispos #.91

#iphoneSC$nokiacamunc #.9
iphoneSC$nokiadisunc <- NULL#.9 Removed

#iphoneSC$nokiaperpos #.9
#iphoneSC$nokiacamunc #.9

#iphoneSC$samsungperneg #.9
iphoneSC$samsungdisneg <- NULL# .9 Removed

#iphoneSC$nokiaperneg #.9
iphoneSC$nokiaperpos <- NULL#.9 Removed

iphoneSC$samsungperunc <- NULL# .94 Removed
#iphoneSC$samsungdispos # .94

#iphoneSC$samsungperunc # .9
#iphoneSC$samsungdisunc # .9

#iphoneSC$nokiaperunc #.9
#iphoneSC$nokiadisunc #.9

iphoneSC$iosperneg <- NULL# .9 Removed
#iphoneSC$iosperpos

iphoneSC$iosperunc <- NULL# .9 Removed
#iphoneSC$iosperpos #.9

#iphoneSC$iosperunc #.9
#iphoneSC$iosperneg #.9


str(iphoneSC)
iphoneS_COR <- iphoneSC


###Galaxy Correlation:
galaxySC <- galaxyS_OOB
galaxySC_cor <- cor(galaxySC)
corrplot(galaxySC_cor)
corrplot(galaxySC_cor,method="number")
corrplot(galaxySC_cor,method="number",type = "upper",number.digits = 2,number.cex=0.70, tl.cex=0.70)

# Remove any Feature highly correlated to the Dependent Variable
# 0.95 
#N/A

# Remove any idependent features that are highly correlated
#0.90
#iphoneSC$iphone # 0.92
galaxySC$ios <- NULL# 0.92 Removed

#iphoneSC$nokiacamneg # 0.9
galaxySC$nokiacampos <- NULL# 0.9 Removed

galaxySC$nokiacamunc <- NULL# 0.94 Removed
#iphoneSC$nokiacampos #0.94

#iphoneSC$htcdispos #.98
galaxySC$htcphone <- NULL#.98 Removed

galaxySC$sonydisneg <- NULL# .9 Removed
#iphoneSC$sonydispos # .9

galaxySC$samsungdisunc <- NULL#.91 Removed
#iphoneSC$samsungdispos #.91

#iphoneSC$nokiacamunc #.9
galaxySC$nokiadisunc <- NULL#.9 Removed

#iphoneSC$nokiaperpos #.9
#iphoneSC$nokiacamunc #.9

#iphoneSC$samsungperneg #.9
galaxySC$samsungdisneg <- NULL# .9 Removed

#iphoneSC$nokiaperneg #.9
galaxySC$nokiaperpos <- NULL#.9 Removed

galaxySC$samsungperunc <- NULL# .94 Removed
#iphoneSC$samsungdispos # .94

#iphoneSC$samsungperunc # .9
#iphoneSC$samsungdisunc # .9

#iphoneSC$nokiaperunc #.9
#iphoneSC$nokiadisunc #.9

galaxySC$iosperneg <- NULL# .9 Removed
#iphoneSC$iosperpos

galaxySC$iosperunc <- NULL# .9 Removed
#iphoneSC$iosperpos #.9

#iphoneSC$iosperunc #.9
#iphoneSC$iosperneg #.9


str(galaxySC)
galaxyS_COR <- galaxySC


##----Feature Variance Datasets----##

###Iphone Variance:
#nearZeroVar() with saveMetrics = TRUE returns an object containing a table including: frequency ratio, percentage unique, zero variance and near zero variance 

iphonenzvMetrics <- nearZeroVar(iphoneS_OOB, saveMetrics = TRUE)
iphonenzvMetrics

# nearZeroVar() with saveMetrics = FALSE returns an vector 
iphonenzv <- nearZeroVar(iphoneS_OOB, saveMetrics = FALSE) 
iphonenzv

iphoneS_NZV <- iphoneS_OOB[,-iphonenzv]
str(iphoneS_NZV)
iphoneS_NZV

###Galaxy Variance:
#nearZeroVar() with saveMetrics = TRUE returns an object containing a table including: frequency ratio, percentage unique, zero variance and near zero variance 

galaxynzvMetrics <- nearZeroVar(galaxyS_OOB, saveMetrics = TRUE)
galaxynzvMetrics

# nearZeroVar() with saveMetrics = FALSE returns an vector 
galaxynzv <- nearZeroVar(galaxyS_OOB, saveMetrics = FALSE) 
galaxynzv

galaxyS_NZV <- galaxyS_OOB[,-galaxynzv]
str(galaxyS_NZV)
galaxyS_NZV

##----Recursive Feature Elimination Datasets----##

###Iphone Recurisive Feature:
# Let's sample the data before using RFE
set.seed(seed)
iphoneSample <- iphoneS_OOB[sample(1:nrow(iphoneS_OOB), 1000, replace=FALSE),]

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrlRFED <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 59 iphonesentiment) 
iphonerfeResults <- rfe(iphoneSample[,1:58], 
                  iphoneSample$iphonesentiment, 
                  sizes=(1:58), 
                  rfeControl=ctrlRFED)

# Get results
iphonerfeResults

# Plot results
plot(iphonerfeResults, type=c("g", "o"))

iphoneS_RFE <- iphoneS_OOB[,predictors(iphonerfeResults)]

# add the dependent variable to iphoneRFE
iphoneS_RFE$iphonesentiment <- iphoneS_OOB$iphonesentiment

# review outcome
str(iphoneS_RFE) # 12973 obs. of  19 variables


###Large Matrix Iphone Recurisive Feature:
#Using Iphone results to filter Large Matrix
LargeM_iphoneRFE <- LargeM_OOB[,predictors(iphonerfeResults)]
str(LargeM_iphoneRFE)


###Galaxy Recurisive Feature:
# Let's sample the data before using RFE
set.seed(seed)
galaxySample <- galaxyS_OOB[sample(1:nrow(galaxyS_OOB), 1000, replace=FALSE),]


# Use rfe and omit the response variable (attribute 59 galaxysentiment) 
galaxyfeResults <- rfe(galaxySample[,1:58], 
                       galaxySample$galaxysentiment, 
                        sizes=(1:58), 
                        rfeControl=ctrlRFED)

# Get results
galaxyfeResults

# Plot results
plot(galaxyfeResults, type=c("g", "o"))

galaxyS_RFE <- galaxyS_OOB[,predictors(galaxyfeResults)]

# add the dependent variable to galaxyRFE
galaxyS_RFE$galaxysentiment <- galaxyS_OOB$galaxysentiment

# review outcome
str(galaxyS_RFE) #12911 obs. of  19 variables

###Large Matrix Galaxy Recurisive Feature:
#Using Iphone results to filter Large Matrix
LargeM_galaxyRFE <- LargeM_OOB[,predictors(galaxyfeResults)]
str(LargeM_galaxyRFE)
LargeM_galaxyRFE


#---Consolidated Feasture Selected Datasets----#

###Iphone:
iphoneS_OOB # Out of the Box
iphoneS_COR # Correlation
iphoneS_NZV # Near Zero Variance
iphoneS_RFE # Recursive Features

###Galaxy:
galaxyS_OOB # Out Of the Box
galaxyS_COR # Correlation
galaxyS_NZV # Near Zero Variance
galaxyS_RFE # Recursive Features

###Large Matrix:
LargeM_OOB # Out of the Box
LargeM_iphoneRFE # Recursive Features Iphone
str(LargeM_iphoneRFE) #28001 obs. of  18 variables
LargeM_galaxyRFE # Recursive Features Galaxy
str(LargeM_galaxyRFE) #28001 obs. of  18 variables


##################
# Train/test sets
##################

#------OOB Iphone Dataset------#
# create the training partition that is 70% of total obs
set.seed(seed) # set random seed
IPhoneinTraining <- createDataPartition(iphoneS_OOB$iphonesentiment, p=0.70, list=FALSE)
# create training/testing dataset
IphoneOOB_trainSet <- iphoneS_OOB[IPhoneinTraining,]   
IphoneOOB_testSet <- iphoneS_OOB[-IPhoneinTraining,]   
# verify number of obs 
nrow(IphoneOOB_trainSet)  # 9082
nrow(IphoneOOB_testSet)   # 3891


#------COR Iphone Dataset------#
# create the training partition that is 70% of total obs
set.seed(seed) # set random seed
IPhoneinTrainingCOR <- createDataPartition(iphoneS_COR$iphonesentiment, p=0.70, list=FALSE)
# create training/testing dataset
IphoneCOR_trainSet <- iphoneS_COR[IPhoneinTrainingCOR,]   
IphoneCOR_testSet <- iphoneS_COR[-IPhoneinTrainingCOR,]   
# verify number of obs 
nrow(IphoneCOR_trainSet)  # 9083
nrow(IphoneCOR_testSet)   # 3890

#------NZV Iphone Dataset------#
# create the training partition that is 70% of total obs
set.seed(seed) # set random seed
IPhoneinTrainingNZV <- createDataPartition(iphoneS_NZV$iphonesentiment, p=0.70, list=FALSE)
# create training/testing dataset
IphoneNZV_trainSet <- iphoneS_NZV[IPhoneinTrainingNZV,]   
IphoneNZV_testSet <- iphoneS_NZV[-IPhoneinTrainingNZV,]   
# verify number of obs 
nrow(IphoneNZV_trainSet)  # 9083
nrow(IphoneNZV_testSet)   # 3890

#------RFE Iphone Dataset------#
# create the training partition that is 70% of total obs
set.seed(seed) # set random seed
IPhoneinTrainingRFE <- createDataPartition(iphoneS_RFE$iphonesentiment, p=0.70, list=FALSE)
# create training/testing dataset
IphoneRFE_trainSet <- iphoneS_RFE[IPhoneinTrainingRFE,]   
IphoneRFE_testSet <- iphoneS_RFE[-IPhoneinTrainingRFE,]   
# verify number of obs 
nrow(IphoneRFE_trainSet)  # 9083
nrow(IphoneRFE_testSet)   # 3890

#-----------Galaxy------#
#------OOB Galaxy Dataset------#
# create the training partition that is 70% of total obs
set.seed(seed) # set random seed
galaxyinTraining <- createDataPartition(galaxyS_OOB$galaxysentiment, p=0.70, list=FALSE)
# create training/testing dataset
galaxyOOB_trainSet <- galaxyS_OOB[galaxyinTraining,]   
galaxyOOB_testSet <- galaxyS_OOB[-galaxyinTraining,]   
# verify number of obs 
nrow(galaxyOOB_trainSet)  # 9040
nrow(galaxyOOB_testSet)   # 3871


#------COR galaxy Dataset------#
# create the training partition that is 70% of total obs
set.seed(seed) # set random seed
galaxyinTrainingCOR <- createDataPartition(galaxyS_COR$galaxysentiment, p=0.70, list=FALSE)
# create training/testing dataset
galaxyCOR_trainSet <- galaxyS_COR[galaxyinTrainingCOR,]   
galaxyCOR_testSet <- galaxyS_COR[-galaxyinTrainingCOR,]   
# verify number of obs 
nrow(galaxyCOR_trainSet)  # 9040
nrow(galaxyCOR_testSet)   # 3933

#------NZV Galaxy Dataset------#
# create the training partition that is 70% of total obs
set.seed(seed) # set random seed
galaxyinTrainingNZV <- createDataPartition(galaxyS_NZV$galaxysentiment, p=0.70, list=FALSE)
# create training/testing dataset
galaxyNZV_trainSet <- galaxyS_NZV[galaxyinTrainingNZV,]   
galaxyNZV_testSet <- galaxyS_NZV[-galaxyinTrainingNZV,]   
# verify number of obs 
nrow(galaxyNZV_trainSet)  # 9040
nrow(galaxyNZV_testSet)   # 3871

#------RFE Galaxy Dataset------#
# create the training partition that is 70% of total obs
set.seed(seed) # set random seed
galaxyinTrainingRFE <- createDataPartition(galaxyS_RFE$galaxysentiment, p=0.70, list=FALSE)
# create training/testing dataset
galaxyRFE_trainSet <- galaxyS_RFE[galaxyinTrainingRFE,]   
galaxyRFE_testSet <- galaxyS_RFE[-galaxyinTrainingRFE,]   
# verify number of obs 
nrow(galaxyRFE_trainSet)  # 9040
nrow(galaxyRFE_testSet)   # 3871



################
# Train control
################

# set 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

##############
# Train model
##############

#---------OOB Iphone Dataset Models------------#

# OOB Iphone RF train/fit:
set.seed(seed)
system.time(iphoneOOB_rfFit <- train(iphonesentiment~., data=IphoneOOB_trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
iphoneOOB_rfFit

varImp(iphoneOOB_rfFit)

#OOB Iphone c50 train/fit:
iphoneOOB_C50Fit <- train(iphonesentiment~., data=IphoneOOB_trainSet, method = "C5.0", trControl=fitControl)
#rfFit
iphoneOOB_C50Fit
varImp(iphoneOOB_C50Fit)

#OOB Iphone SVM Train/Fit:
set.seed(seed)
iphoneOOB_svmFit <- train(iphonesentiment~., data=IphoneOOB_trainSet, method="svmLinear2", trControl=fitControl)
iphoneOOB_svmFit
varImp(iphoneOOB_svmFit)
summary(iphoneOOB_svmFit)

#OOB Iphone KNN Train/Fit:
set.seed(seed)
iphoneOOB_knnFit <- train(iphonesentiment~., data=IphoneOOB_trainSet, method="kknn", trControl=fitControl)
iphoneOOB_knnFit
varImp(iphoneOOB_knnFit)
summary(iphoneOOB_knnFit)

#---------COR,NZV,RFE Iphone Dataset Models------------#

##COR Iphone c50:
iphoneCOR_C50Fit <- train(iphonesentiment~., data=IphoneCOR_trainSet, method = "C5.0", trControl=fitControl)
#rfFit
iphoneCOR_C50Fit
varImp(iphoneCOR_C50Fit)

##NZV Iphone c50:
iphoneNZV_C50Fit <- train(iphonesentiment~., data=IphoneNZV_trainSet, method = "C5.0", trControl=fitControl)
#rfFit
iphoneNZV_C50Fit
varImp(iphoneNZV_C50Fit)

##RFE Iphone c50:
iphoneRFE_C50Fit <- train(iphonesentiment~., data=IphoneRFE_trainSet, method = "C5.0", trControl=fitControl)
#rfFit
iphoneRFE_C50Fit
varImp(iphoneRFE_C50Fit)


#-------Galaxy---------#
#---------OOB Galaxy Dataset Models------------#

# OOB galaxy RF train/fit:
set.seed(seed)
system.time(galaxyOOB_rfFit <- train(galaxysentiment~., data=galaxyOOB_trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
galaxyOOB_rfFit

varImp(galaxyOOB_rfFit)

#OOB Galaxy c50 train/fit:
galaxyOOB_C50Fit <- train(galaxysentiment~., data=galaxyOOB_trainSet, method = "C5.0", trControl=fitControl)
#rfFit
galaxyOOB_C50Fit
varImp(galaxyOOB_C50Fit)

#OOB Galaxy SVM Train/Fit:
set.seed(seed)
galaxyOOB_svmFit <- train(galaxysentiment~., data=galaxyOOB_trainSet, method="svmLinear2", trControl=fitControl)
galaxyOOB_svmFit
varImp(galaxyOOB_svmFit)
summary(galaxyOOB_svmFit)

#OOB galaxy KNN Train/Fit:
set.seed(seed)
galaxyOOB_knnFit <- train(galaxysentiment~., data=galaxyOOB_trainSet, method="kknn", trControl=fitControl)
galaxyOOB_knnFit
varImp(galaxyOOB_knnFit)
summary(galaxyOOB_knnFit)

#---------COR,NZV,RFE Galaxy Dataset Models------------#

##COR Galaxy rf:
galaxyCOR_trainSet
galaxyCOR_rfFit <- train(galaxysentiment~., data=galaxyCOR_trainSet, method="rf", importance=T, trControl=fitControl)
#rfFit
galaxyCOR_rfFit
varImp(galaxyCOR_rfFit)

##NZV Galaxy rf:
galaxyNZV_rfFit <- train(galaxysentiment~., data=galaxyNZV_trainSet, method="rf", importance=T, trControl=fitControl)
#rfFit
galaxyNZV_rfFit
varImp(galaxyNZV_rfFit)

##RFE Galaxy rf:
galaxyRFE_rfFit <- train(galaxysentiment~., data=galaxyRFE_trainSet, method="rf", importance=T, trControl=fitControl)
#rfFit
galaxyRFE_rfFit
varImp(galaxyRFE_rfFit)



##################
#Evaluate Models##
##################

#------Iphone OOB Model Eval ----#
##--- Compare models ---##

# use resamples to compare model performance
IphoneOOB_ModelFit <- resamples(list(c50fit=iphoneOOB_C50Fit, rf=iphoneOOB_rfFit, svm=iphoneOOB_svmFit,knn=iphoneOOB_knnFit))
IphoneOOB_ModelFit
summary(IphoneOOB_ModelFit)

# Accuracy
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# c50fit 0.7623762 0.7676841 0.7744764 0.7732018 0.7766466 0.7861080    0
# rf     0.7599119 0.7635560 0.7717323 0.7725431 0.7806567 0.7920792    0
# svm    0.6982379 0.6995026 0.7055594 0.7088003 0.7140120 0.7290749    0
# knn    0.3083700 0.3196828 0.3276442 0.3283062 0.3342511 0.3513216    0
# 
# Kappa
#             Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# c50fit 0.5343348 0.5479238 0.5607427 0.5596151 0.5693472 0.5886621    0
# rf     0.5332170 0.5425004 0.5613227 0.5625417 0.5804053 0.6065620    0
# svm    0.3849653 0.3934384 0.4062031 0.4107637 0.4172563 0.4525589    0
# knn    0.1417769 0.1501527 0.1562114 0.1609948 0.1712841 0.1871676    0

#------Iphone COR,NZV,RFE for C50 Model Eval ----#
##--- Compare models ---##

# use resamples to compare model performance
IphoneModelFit <- resamples(list(c50fitCOR=iphoneCOR_C50Fit, c50fitNZV=iphoneNZV_C50Fit, c50fitRFE=iphoneRFE_C50Fit))
IphoneModelFit
summary(IphoneModelFit)
# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# c50fitCOR 0.7599119 0.7678965 0.7763079 0.7737508 0.7794890 0.7863436    0
# c50fitNZV 0.7293729 0.7524780 0.7570105 0.7549299 0.7601332 0.7687225    0
# c50fitRFE 0.7571429 0.7701621 0.7728273 0.7730929 0.7763701 0.7909791    0
# 
# Kappa 
#                Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# c50fitCOR 0.5260268 0.5470690 0.5670547 0.5604696 0.5758180 0.5898587    0
# c50fitNZV 0.4642654 0.5129004 0.5209518 0.5184753 0.5303281 0.5498870    0
# c50fitRFE 0.5236595 0.5520805 0.5588735 0.5594154 0.5667479 0.5969005    0

iphoneRFE_C50Fit

#------galaxy OOB Model Eval ----#
##--- Compare models ---##

# use resamples to compare model performance
galaxyOOB_ModelFit <- resamples(list(c50fitg=galaxyOOB_C50Fit, rfg=galaxyOOB_rfFit, svmg=galaxyOOB_svmFit,knng=galaxyOOB_knnFit))
galaxyOOB_ModelFit
summary(galaxyOOB_ModelFit)
# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# c50fitg 0.7555310 0.7572523 0.7661706 0.7658221 0.7723367 0.7776549    0
# rfg     0.7535912 0.7594687 0.7645438 0.7639392 0.7693584 0.7743363    0
# svmg    0.6924779 0.7035912 0.7078029 0.7063032 0.7110066 0.7146018    0
# knng    0.6983425 0.7269973 0.7362905 0.7336326 0.7419801 0.7511062    0
# 
# Kappa 
#              Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# c50fitg 0.5037273 0.5134424 0.5321022 0.5300137 0.5424859 0.5558141    0
# rfg     0.5052111 0.5211696 0.5318253 0.5303086 0.5393723 0.5575496    0
# svmg    0.3333952 0.3758177 0.3877268 0.3796011 0.3904645 0.3981902    0
# knng    0.4369133 0.4766108 0.4900178 0.4887705 0.5094091 0.5172740    0

#------Galaxy COR,NZV,RFE for RF Model Eval ----#
##--- Compare models ---##

# use resamples to compare model performance
GalaxyModelFit <- resamples(list(rffitCOR=galaxyCOR_rfFit, rffitNZV=galaxyNZV_rfFit, rffitRFE=galaxyRFE_rfFit))
GalaxyModelFit
summary(GalaxyModelFit)

# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rffitCOR 0.7403315 0.7600211 0.7639583 0.7620617 0.7675912 0.7754425    0
# rffitNZV 0.7422566 0.7502769 0.7560832 0.7559684 0.7612600 0.7701657    0
# rffitRFE 0.7488938 0.7510368 0.7603790 0.7621637 0.7710819 0.7856354    0
# 
# Kappa 
#               Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rffitCOR 0.4895620 0.5216377 0.5278493 0.5264341 0.5370202 0.5553779    0
# rffitNZV 0.4768557 0.4904992 0.4999436 0.5028753 0.5164363 0.5396137    0
# rffitRFE 0.4994934 0.5045130 0.5251001 0.5291228 0.5448471 0.5814690    0


galaxyRFE_rfFit

########################
# Validate top model
########################

##OOB Iphone Pred:

iphoneOOB_C50Fit
iphoneOOBc50Fitpred <- predict(iphoneOOB_C50Fit, IphoneOOB_testSet)

# performace measurment
postResample(iphoneOOBc50Fitpred, IphoneOOB_testSet$iphonesentiment)
# Accuracy     Kappa 
# 0.7724936 0.5558736 

##OOB Galaxy Pred:
galaxyOOB_rfFit
galaxyOOBrfFitpred <- predict(galaxyOOB_rfFit, galaxyOOB_testSet)

# performace measurment
postResample(galaxyOOBrfFitpred, galaxyOOB_testSet$galaxysentiment)
# Accuracy     Kappa 
# 0.7672436 0.5359114 

##Iphone RFE c50 Pred:
iphoneRFE_C50Fit
iphoneRFEc50Fitpred <- predict(iphoneRFE_C50Fit, IphoneRFE_testSet)

# performace measurment
postResample(iphoneRFEc50Fitpred, IphoneRFE_testSet$iphonesentiment)
# Accuracy     Kappa 
# 0.7706941 0.5521643
confusionMatrix(iphoneRFEc50Fitpred,IphoneRFE_testSet$iphonesentiment)
save(iphoneRFE_C50Fit, file = "iphoneRFE_C50Fit.rda")

##Galaxy RFE RF Pred:
galaxyRFE_rfFit
galaxyRFErfFitpred <- predict(galaxyRFE_rfFit, galaxyRFE_testSet)

# performace measurment
postResample(galaxyRFErfFitpred, galaxyRFE_testSet$galaxysentiment)
# Accuracy     Kappa 
# 0.7669853 0.5361725 
confusionMatrix(galaxyRFErfFitpred,galaxyRFE_testSet$galaxysentiment)
save(galaxyRFE_rfFit, file = "galaxyRFE_rfFit.rda")

##--------Large Matrix Prediction -----##

##Iphone:
iphoneRFE_C50Fit # RFE c50 Model
LargeM_iphoneRFE # Recursive Features Iphone

LargeM_IphonePred <- predict(iphoneRFE_C50Fit,LargeM_iphoneRFE)
LargeM_IphonePred

##Galaxy:
galaxyRFE_rfFit # RFE Random Forest Model
LargeM_galaxyRFE # Recursive Features Galaxy

LargeM_GalaxyPred <- predict(galaxyRFE_rfFit,LargeM_galaxyRFE)

########################
#Add Perdiction to Frame
########################

LargeM_OOB$galaxysentiment <- LargeM_GalaxyPred # Galaxy Predicted Sentiment
LargeM_OOB$iphonesentiment <- LargeM_IphonePred # Iphone Predicted Sentiment
write.csv(LargeM_OOB, file="LargeMatrixPred.csv", row.names = TRUE)


