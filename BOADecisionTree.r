# Decision trees take a top-down, 'divide-and-conquer' approach to analyzing data. 
# They look for the indicator, and indicator value, that best splits the data into two 
# distinct groups. The algorithm then repeats this process on each subsequent group until 
# it correctly classifies every data point or a stopping criteria is reached. Each split,
# known as a 'node', tries to maximize the purity of the resulting 'branches'. The purity 
# is basically the probability that a data point falls in a given class, in our case 'up'
# or 'down', and is measured by the 'information gain' of each split.

# Decision trees, however, can easily overfit the data and perform poorly when looking at new data. 

# Three ways to fix this:

# 1) You can control the growth of the tree by specifying either a threshold on the information gain 
#or a minimum number of data points in each branch to justify a split. This creates a tree that will 
# attempt to capture larger, more broad patterns and not focus on minor splits 
# that may be unique to your particular data set.

# 2) You can also prune back the tree after it has already been built. Usually this is done by selecting the tree 
# size that minimizes the cross-validated error. Cross-validation is a process that partitions the data into multiple sections
# then uses all but one section to build the model and the remaining section to test the model. This process is then repeated, 
# using each section to test the model, and the tree size with the lowest average error is chosen. This is also a great way to 
# reduce overfitting if you have limited data.

# 3)  more sophisticated approach is to build many different trees and use them collectively to make predictions. 
# There are three popular techniques, bagging, boosting, and random forests

library("quantmod")
library("rpart.plot")

startDate = as.Date("2012-01-01")
endDate = as.Date("2014-01-01")

getSymbols("BAC", src = "yahoo", from = startDate, to = endDate)

#Calculate a 3-period relative strength index (RSI) off the open price
RSI3 = RSI(Op(BAC), n = 3)

EMA5 = EMA(Op(BAC),n=5) 
#Calculate a 5-period exponential moving average (EMA)
EMAcross = Op(BAC)-EMA5 
#Let's explore the difference between the open price and our 5-period EMA

MACD = MACD(Op(BAC),fast = 12, slow = 26, signal = 9) 
#Calculate a MACD with standard parameters
MACDsignal = MACD[,2] 
#Grab just the signal line to use as our indicator.

SMI = SMI(Op(BAC),n=13,slow=25,fast=2,signal=9) 
#Stochastic Oscillator with standard parameters
SMI = SMI[,1] 
#Grab just the oscillator to use as our indicator

PriceChange<- Cl(BAC) - Op(BAC) 
#Calculate the difference between the close price and open price
Class<-ifelse(PriceChange>0,"UP","DOWN") 
#Create a binary classification variable, the variable we are trying to predict.

DataSet<-data.frame(RSI3,EMAcross,MACDsignal,SMI,Class) 
#Create our data set
colnames(DataSet)<-c("RSI3","EMAcross","MACDsignal","Stochastic","Class") 
#Name the columns 
DataSet<-DataSet[-c(1:33),] 
#Get rid of the data where the indicators are being calculated

TrainingSet<-DataSet[1:312,] 
#Use 2/3 of the data to build the tree
TestSet<-DataSet[313:469,]
#And leave out 1/3 data to test our strategy

DecisionTree<-rpart(Class~RSI3+EMAcross+MACDsignal+Stochastic,data=TrainingSet, cp=.001)
#Specifying the indicators to we want to use to predict the class and controlling the growth of the tree by setting 
#the minimum amount of information gained (cp) needed to justify a split.

prp(DecisionTree,type=2,extra=8)
#Nice plotting tool with a couple parameters to make it look good. 


printcp(DecisionTree)
#shows the minimal cp for each trees of each size.
plotcp(DecisionTree,upper="splits")
#plots the average geometric mean for trees of each size.

PrunedDecisionTree<-prune(DecisionTree,cp=0.0272109)
#I am selecting the complexity parameter (cp) that has the lowest cross-validated error (xerror) 

prp(PrunedDecisionTree, type=2, extra=8)

table(predict(PrunedDecisionTree,TestSet,type="class"),TestSet[,5],dnn=list('predicted','actual'))
