#Allows us to import the data we need
#install.packages("quantmod")
library("quantmod")

#Makes it easier to work with dates
#install.packages("lubridate")
library("lubridate")

#Gives us access to Naive Bayes classifier
#install.packages("e1071")
library("e1071")

# The beginning  of the date range we want to look at 
startDate = as.Date("2012-06-01")
#The end  of the date range we want to look at 
endDate = as.Date("2014-01-01")

# Retrieve Facebook's daily OHLCV from Yahoo finance
getSymbols("FB", src = "yahoo", from = startDate, to = endDate)

#Find the day of the week
DayofWeek = wday(FB, label = TRUE)

#Find the difference between the close price and open price
PriceChange = Cl(FB) - Op(FB)

#Convert to a binary classification. 
#(In our data set, there are no bars with an exactly 0 price change so,
#for simplicity sake, we will not address bars that had the same open 
#and close price.)
Class = ifelse(PriceChange>0, "UP", "DOWN")

#Create our data set
DataSet = data.frame(DayofWeek, Class)

# The input, or independen variable (Dataset[,1]) and what we are trying to predict
# the dependent variable (DataSet[,2]).
MyModel = naiveBayes(DataSet[,1], DataSet[,2])

# Model shows the conditoinal probabilities, given that it is a certain day of the week
# what is the probability that the price will close up or down. 
MyModel


### Improving the Model ###
# Calculate a moving average cross to our model
# Exponential moving averages

# Calculate a 5 period EMA off the open price
EMA5 = EMA(Op(FB), n = 5)

#Calculate a 10-period EMA, also off the open price
EMA10 = EMA(Op(FB), n = 10)

#Positive values correspond to the 5-period EMA being above the 10-period EMA
EMACross = EMA5 - EMA10

#Make sure to round to 2 decimal places for Naive Bayes
EMACross = round(EMACross, 2)

DataSet2 = data.frame(DayofWeek, EMACross, Class)
DataSet2 = DataSet2[-c(1:10), ]

# Use 2/3 of the data to train the model
TrainingSet = DataSet2[1:(nrow(DataSet2)*2/3),]

#1/3 to test it on unseen data
TestSet = DataSet2[(nrow(DataSet2)*2/3):nrow(DataSet2),]

EMACrossModel = naiveBayes(TrainingSet[,1:2], TrainingSet[,3])

EMACrossModel
#The Conditional Probability of the EMA Cross, 
#a numeric variable, shows the mean value for 
#each case ([,1]), and the standard deviation ([,2]). 
#We can see that the mean difference between the 5-period EMA
#and 10-period EMA for long and short trades was $0.54 and -$0.24, respectively.

table(predict(EMACrossModel, TestSet), TestSet[,3], dnn = list('predicted', 'actual'))

