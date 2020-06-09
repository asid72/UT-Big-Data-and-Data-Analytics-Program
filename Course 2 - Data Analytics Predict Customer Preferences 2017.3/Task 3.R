

# get working directory
getwd()
# set working directory
setwd("C:\\Users\\asiddiqui\\Documents\\UT Data Analytics Course\\Course Weeks\\Course 2 - Data Analytics Predict Customer Preferences 2017.3\\Task 3\\")
dir()

# set a value for seed (to be used in the set.seed function)
seed <- 123


################
# Load packages
################

install.packages("caret")
install.packages("corrplot")
install.packages("readr")
library(caret)
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
install.packages("doParallel") # install in 'Load packages' section above
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
existingproduct <- read.csv("existingproductattributes2017.csv", stringsAsFactors = FALSE)
class(existingproduct)  # "data.frame"
str(existingproduct)



# --- Load Predict/New data (Dataset 2) --- #
newproductattributes <- read.csv("newproductattributes2017.csv", stringsAsFactors = FALSE)
class(newproductattributes)  # "data.frame"
str(newproductattributes)




################
# Evaluate data
################

#--- Dataset 1 ---#
str(existingproduct)  #80 obs. of  18 variables: 
names(existingproduct)
summary(existingproduct)
head(existingproduct)
tail(existingproduct)

# plot
hist(existingproduct$Volume)
#plot(WholeYear$TimeofDay, WholeYear$SolarRad)
#qqnorm(WholeYear$SolarRad)
# check for missing values 
anyNA(existingproduct)
is.na(existingproduct)
summary(existingproduct)


#--- Dataset 2 ---#
str(newproductattributes)  #24 obs. of  18 variables:
names(newproductattributes)
summary(newproductattributes)
head(newproductattributes)
tail(newproductattributes)

# plot
hist(newproductattributes$Volume)
#plot(WholeYear$TimeofDay, WholeYear$SolarRad)
#qqnorm(WholeYear$SolarRad)
# check for missing values 
anyNA(newproductattributes)
is.na(newproductattributes)
summary(newproductattributes)

#############
# Preprocess
#############

#--- Dataset 1 ---#

#dumfiy the data
#Dummy variables for ProductType
newDataFrame <- dummyVars(" ~ .", data = existingproduct)
existingproduct_readyData <- data.frame(predict(newDataFrame, newdata = existingproduct))
str(existingproduct_readyData)

# remove obvious features (e.g., ID, other)
existingproductDV <- existingproduct_readyData   # make a copy 
existingproductDV$ProductNum <- NULL   # remove ProductNum since ID
existingproductDV$BestSellersRank <- NULL # remove BestSellerRank since including NAs
str(existingproductDV) # 80 obs. of  27 variables:
summary(existingproductDV)
# save preprocessed dataset
write.csv(existingproductDV,file="existingproductDV.csv")


#--- Dataset 2 ---#

#dumfiy the data
#Dummy variables for ProductType
newDataFrame2 <- dummyVars(" ~ .", data = newproductattributes)
newproductattributes_readyData <- data.frame(predict(newDataFrame2, newdata = newproductattributes))
str(newproductattributes_readyData)

# remove obvious features (e.g., ID, other)
newproductattributesDV <- newproductattributes_readyData   # make a copy 
newproductattributesDV$ProductNum <- NULL   # remove ProductNum since ID
newproductattributesDV$BestSellersRank <- NULL # remove BestSellerRank since including NAs
str(newproductattributesDV) # 24 obs. of  27 variables:
summary(newproductattributesDV)
# save preprocessed dataset
write.csv(newproductattributesDV,file="newproductattributesDV.csv")


###################
#Correlation Matrix
###################

#--- Dataset 1 ---#

corExistingProduct <- cor(existingproductDV)
corExistingProduct
corrplot(corExistingProduct)
corrplot(corExistingProduct, type="upper")
corrplot(corExistingProduct,method="number")
corrplot(corExistingProduct,method="number",type = "upper",number.digits = 2,number.cex=0.70, tl.cex=0.70)

# Remove any Feature highly correlated to the Dependent Variable
# 0.95 

existingproductDV$x5StarReviews <- NULL

# Remove any idependent features that are highly correlated
#0.90
existingproductDV$x1StarReviews <- NULL
existingproductDV$x3StarReviews <- NULL
str(existingproductDV)

#--- Dataset 2 ---#

corExistingProduct2 <- cor(newproductattributesDV)
corExistingProduct2
corrplot(corExistingProduct2, type="upper")

corrplot(corExistingProduct2,method="number",type = "upper",number.digits = 2,number.cex=0.40, tl.cex=0.30)

# Remove any Feature highly correlated to the Dependent Variable
# 0.95 

newproductattributesDV$x5StarReviews <- NULL

# Remove any idependent features that are highly correlated
#0.90
newproductattributesDV$x1StarReviews <- NULL
newproductattributesDV$x3StarReviews <- NULL
str(newproductattributesDV)



##################
# Train/test sets
##################

# create the training partition that is 75% of total obs
set.seed(seed) # set random seed
inTraining <- createDataPartition(existingproductDV$Volume, p=0.75, list=FALSE)
# create training/testing dataset
trainSet <- existingproductDV[inTraining,]   
testSet <- existingproductDV[-inTraining,]   
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


## ------- LM ------- ##

# LM train/fit
set.seed(seed)
#lmFit1 <- train(SolarRad~., data=trainSet, method="leapSeq", trControl=fitControl)
lmFit1 <- train(Volume~., data=trainSet, method="lm", trControl=fitControl)
lmFit1  
# Evaluate performance metrics, but don't add as comments to script file. Performance
# metrics will be added as comments to the script file in the Evaluate models below

# evaluate var importance
varImp(lmFit1)





## ------- RF ------- ##

# RF train/fit
set.seed(seed)
system.time(rfFit1 <- train(Volume~., data=trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
rfFit1

varImp(rfFit1)


## ------- SVM ------- ##

# SVM train/fit
set.seed(seed)
svmFit1 <- train(Volume~., data=trainSet, method="svmLinear", trControl=fitControl)
svmFit1
varImp(svmFit1)
summary(svmFit1)


## ------- GBM --------##
# GBM train/fit
set.seed(seed)
gbmFit1 <- train(Volume~., data=trainSet, method="gbm", trControl=fitControl)
gbmFit1
varImp(gbmFit1)
summary(gbmFit1)

# C.5
#C50Fit <- train(brand~., data = trainSet, method = "C5.0", trControl=fitControl, tuneLength = 1)
#rfFit
#C50Fit


#################
# Evaluate models
#################

##--- Compare models ---##

# use resamples to compare model performance
ModelFitResults1k <- resamples(list(lm=lmFit1, rf=rfFit1, svm=svmFit1,gbm=gbmFit1))
# output summary metrics for tuned models 
summary(ModelFitResults1k)
# Results:
# RMSE 
#          Min.  1st Qu.   Median     Mean   3rd Qu.     Max. NA's
# lm  140.14184 427.7932 470.3557 730.4873  678.9288 2769.594    0
# rf   35.70961 154.6262 297.2972 785.2678  919.9273 3147.148    0
# svm 410.24460 484.6206 738.8094 999.2661 1029.3892 3019.513    0
# gbm 337.97734 365.4316 414.8010 941.9466  635.7744 3944.205    0
# 
# Rsquared 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# lm  0.1218093 0.7707961 0.9193350 0.7789685 0.9502848 0.9890099    0
# rf  0.7018239 0.8308051 0.9288055 0.9011142 0.9928239 0.9975329    0
# svm 0.1276351 0.7444579 0.8718971 0.7822891 0.9366675 0.9932980    0
# gbm 0.3446906 0.7801303 0.8424995 0.8108935 0.9380622 0.9847783    0


##--- Conclusion ---##
# Make a note of which model is the top model, and why





########################
# Validate top model
########################

# make predictions
Fit1 <- rfFit1
#Fit1 <- gbmFit1
#Fit1 <- svmFit1
#Fit1 <- lmFit1
rfPred1 <- predict(Fit1, testSet)

# performace measurment
postResample(rfPred1, testSet$Volume)
# RMSE      Rsquared  
# (make  note of performance metrics)

# plot predicted verses actual
plot(rfPred1,testSet$Volume)
# print predictions
rfPred1


########################
# Predict with top model
########################

# make predictions
finalPred <- predict(rfFit1, newproductattributesDV)
finalPred



########################
# Save validated model
########################

output <- newproductattributes
output$predictions <- finalPred
write.csv(output, file="C2.T3output.csv", row.names = TRUE)


