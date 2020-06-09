

# get working directory
getwd()
# set working directory
setwd("C:\\Users\\asiddiqui\\Documents\\UT Data Analytics Course\\Course Weeks\\Course 2 - Data Analytics Predict Customer Preferences 2017.3\\Task 4\\")
dir()

# set a value for seed (to be used in the set.seed function)
seed <- 123


################
# Load packages
################

#arules - Is a package for analyzing transactional data. 
??arules

#arulesViz - Is a package that provides visual techniques for the arules package.
??arulesViz

#install.packages("caret")
#install.packages("corrplot")
install.packages("readr")
install.packages("arules")
install.packages("arulesViz")
install.packages('caTools')
#library(caret)
library(corrplot)
#library(doMC)
#library(doParallel)
library(arules)
library(arulesViz)
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

# --- Load Train/Existing data (Dataset) --- #
ElectronidexTransactions <- read.transactions("ElectronidexTransactions2017.csv")
class(ElectronidexTransactions)  # "sparse Matrix"
str(ElectronidexTransactions)
summary(ElectronidexTransactions)





################
# Evaluate data
################

#--- Dataset ---#
inspect (ElectronidexTransactions) # You can view the transactions. Is there a way to see a certain # of transactions?
length (ElectronidexTransactions) # Number of transactions.
size (ElectronidexTransactions) # Number of items per transaction
LIST(ElectronidexTransactions) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(ElectronidexTransactions)# To see the item labels

# plot
itemFrequencyPlot(ElectronidexTransactions, topN=5)
image(ElectronidexTransactions)
image(sample(ElectronidexTransactions,1000))




#############
# Preprocess
#############

# No Preprocess on this dataset

###################
#Correlation Matrix
###################

#No Correlation Matrix needed

##################
# Train/test sets
##################

# No training set needed, since apriori is unsupervised learner 


################
# Train control
################

#No Train Control Needed

##############
# Train model
##############



## ------- apriori ------- ##


set.seed(seed)
RulesElectT <- apriori (ElectronidexTransactions, parameter = list(supp = 0.1, conf = 0.8))



#################
# Evaluate Association Rules
#################
RulesElectT
inspect(RulesElectT)

# ----- Sort By Support ----- #
inspect(sort( RulesElectT, by = "support"))
#      lhs                            rhs        support  
# [1]  {Keyboard}                  => {Wireless} 0.1758007
# [2]  {and}                       => {Wireless} 0.1707168
# [3]  {Keyboard}                  => {and}      0.1591256
# [4]  {and,Keyboard}              => {Wireless} 0.1572954
# [5]  {Keyboard,Wireless}         => {and}      0.1572954
# [6]  {and,Wireless}              => {Keyboard} 0.1572954
# [7]  {and,Desktop}               => {Wireless} 0.1210981
# [8]  {Desktop,Keyboard}          => {Wireless} 0.1181495
# [9]  {Desktop,Keyboard}          => {and}      0.1155058
# [10] {and,Desktop}               => {Keyboard} 0.1155058
# [11] {and,Desktop,Keyboard}      => {Wireless} 0.1148958
# [12] {Desktop,Keyboard,Wireless} => {and}      0.1148958
# [13] {and,Desktop,Wireless}      => {Keyboard} 0.1148958
#      confidence lift     count
# [1]  0.9295699  2.850739 1729 
# [2]  0.8118956  2.489864 1679 
# [3]  0.8413978  4.001522 1565 
# [4]  0.9884984  3.031457 1547 
# [5]  0.8947368  4.255192 1547 
# [6]  0.9213818  4.871930 1547 
# [7]  0.9105505  2.792412 1191 
# [8]  0.9659185  2.962210 1162 
# [9]  0.9443059  4.490933 1136 
# [10] 0.8685015  4.592319 1136 
# [11] 0.9947183  3.050532 1130 
# [12] 0.9724613  4.624834 1130 
# [13] 0.9487825  5.016815 1130 


# ----- Sort By confidence ----- #
inspect(sort( RulesElectT, by = "confidence"))
#      lhs                            rhs        support  
# [1]  {and,Desktop,Keyboard}      => {Wireless} 0.1148958
# [2]  {and,Keyboard}              => {Wireless} 0.1572954
# [3]  {Desktop,Keyboard,Wireless} => {and}      0.1148958
# [4]  {Desktop,Keyboard}          => {Wireless} 0.1181495
# [5]  {and,Desktop,Wireless}      => {Keyboard} 0.1148958
# [6]  {Desktop,Keyboard}          => {and}      0.1155058
# [7]  {Keyboard}                  => {Wireless} 0.1758007
# [8]  {and,Wireless}              => {Keyboard} 0.1572954
# [9]  {and,Desktop}               => {Wireless} 0.1210981
# [10] {Keyboard,Wireless}         => {and}      0.1572954
# [11] {and,Desktop}               => {Keyboard} 0.1155058
# [12] {Keyboard}                  => {and}      0.1591256
# [13] {and}                       => {Wireless} 0.1707168
#     confidence lift     count
# [1]  0.9947183  3.050532 1130 
# [2]  0.9884984  3.031457 1547 
# [3]  0.9724613  4.624834 1130 
# [4]  0.9659185  2.962210 1162 
# [5]  0.9487825  5.016815 1130 
# [6]  0.9443059  4.490933 1136 
# [7]  0.9295699  2.850739 1729 
# [8]  0.9213818  4.871930 1547 
# [9]  0.9105505  2.792412 1191 
# [10] 0.8947368  4.255192 1547 
# [11] 0.8685015  4.592319 1136 
# [12] 0.8413978  4.001522 1565 
# [13] 0.8118956  2.489864 1679 


# ----- Sort By Lift ----- #
inspect(sort( RulesElectT, by = "lift"))
#      lhs                            rhs        support  
# [1]  {and,Desktop,Wireless}      => {Keyboard} 0.1148958
# [2]  {and,Wireless}              => {Keyboard} 0.1572954
# [3]  {Desktop,Keyboard,Wireless} => {and}      0.1148958
# [4]  {and,Desktop}               => {Keyboard} 0.1155058
# [5]  {Desktop,Keyboard}          => {and}      0.1155058
# [6]  {Keyboard,Wireless}         => {and}      0.1572954
# [7]  {Keyboard}                  => {and}      0.1591256
# [8]  {and,Desktop,Keyboard}      => {Wireless} 0.1148958
# [9]  {and,Keyboard}              => {Wireless} 0.1572954
# [10] {Desktop,Keyboard}          => {Wireless} 0.1181495
# [11] {Keyboard}                  => {Wireless} 0.1758007
# [12] {and,Desktop}               => {Wireless} 0.1210981
# [13] {and}                       => {Wireless} 0.1707168
#     confidence lift     count
# [1]  0.9487825  5.016815 1130 
# [2]  0.9213818  4.871930 1547 
# [3]  0.9724613  4.624834 1130 
# [4]  0.8685015  4.592319 1136 
# [5]  0.9443059  4.490933 1136 
# [6]  0.8947368  4.255192 1547 
# [7]  0.8413978  4.001522 1565 
# [8]  0.9947183  3.050532 1130 
# [9]  0.9884984  3.031457 1547 
# [10] 0.9659185  2.962210 1162 
# [11] 0.9295699  2.850739 1729 
# [12] 0.9105505  2.792412 1191 
# [13] 0.8118956  2.489864 1679 


Drule <- subset(RulesElectT, items %in% "Desktop")
inspect(Drule)

is.redundant(RulesElectT) # Check for Redundancies
# [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [10] FALSE FALSE FALSE FALSE



plot(RulesElectT[1:13], method="graph", control=list(type="items")) 

##--- Conclusion ---##
# Make a note of which model is the top model, and why





#############################
# Validate Associations Rules
#############################

#-----The Insightful Rules Category----#
# {and,Desktop,Wireless}      => {Keyboard}
# {and,Wireless}              => {Keyboard}
# {Desktop,Keyboard,Wireless} => {and}
# {and,Desktop,Keyboard}      => {Wireless}
# {and,Keyboard}              => {Wireless}
# {Desktop,Keyboard}          => {Wireless}



#---The Irrelevant Rules Category------#
# {and,Desktop}               => {Keyboard}
# {Keyboard}                  => {Wireless}
# {and,Desktop}               => {Wireless}

#---The Unclear Rules Category---------#
# {Desktop,Keyboard}          => {and}
# {Keyboard,Wireless}         => {and}
# {Keyboard}                  => {and}
# {and}                       => {Wireless}






########################
# Predict with top model
########################

# No Top Model



########################
# Save validated model
########################


