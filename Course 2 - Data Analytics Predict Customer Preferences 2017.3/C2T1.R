library(readr)
carsdataset<- read.csv("C:\\Users\\asiddiqui\\Documents\\UT Data Analytics Course\\Course Weeks\\Course 2 - Data Analytics Predict Customer Preferences 2017.3\\Task 1 - Get Started with R\\R Tutorial Data\\R Tutorial Data Sets\\cars.csv",header=TRUE)
attributes(carsdataset)
summary(carsdataset)
str(carsdataset)
names(carsdataset)
carsdataset$name.of.car
hist(carsdataset$speed.of.car)
plot(carsdataset$name.of.car, carsdataset$speed.of.car)
names(carsdataset)<- c("Brand","Speed","Distance")
carsdataset$Brand <- as.factor(carsdataset$Brand)
carsdataset$Speed <- as.integer(carsdataset$Speed)
carsdataset$Distance <- as.integer(carsdataset$Distance)
summary(carsdataset)
is.na(carsdataset)
TrainSize<- round(nrow(carsdataset)* 0.7)
TrainSize
TestSize<- nrow(carsdataset)- TrainSize
TestSize
set.seed(123)
Training_indices<- sample(seq_len(nrow(carsdataset)),size=TrainSize)
Trainset<- carsdataset[Training_indices,]
Testset<- carsdataset[-Training_indices,]
CarsModel <-lm(Distance~ Speed, Trainset)
summary(CarsModel)
CarsPrediction <- predict(CarsModel,Testset)
CarsPrediction

#1         2         6        16        18        20        22        23        34        35        38        39 
#-14.95415 -14.95415  10.41329  30.70724  30.70724  35.78073  35.78073  35.78073  56.07468  56.07468  61.14817  66.22166 
#44        46        47 
#76.36864  86.51561  86.51561 