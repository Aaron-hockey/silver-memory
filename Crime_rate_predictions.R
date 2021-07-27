# Aaron Norstrom 
# Ali Elsheikh
# Brian Matamet
# Math-032-Group Project-First Draft

# Our goal is to find the variables that can be used as arguments
# in our linear model to find most accurately predict violent crimes
# per population
library(fitdistrplus)
library(MASS)
# clear memory
rm(list=ls(all=TRUE))

# Load in crime data
load("crime.Rdata")

# Removing the columns of variables that we have
# decided not to use after some careful consideration
# analysis this will help remove some unneeded data loading

x <- (x[,c(6:7,17:26,34:39,44:101,103,105,107,109,116:128)])

# create a while loop to find which columns are missing data
# if column is missing entries then fill entries wiht mean
# value of that column
# This will allow us to all of the predictive variables in the set.
# Helping us to avoid removing valuable data

for(i in 1:ncol(x))
{ 
  if (anyNA(x[,i])){
     narows = which(is.na(x[,i]))
     x[narows,i] = mean(x[,i], na.rm = TRUE)
     }
 }

# Create a for loop to cycle through the entire
# variable list and creating a lm of the form 
# lm(ViolentCrimesPerPop ~ x[,j], data = x), then
# we calculate the MSE of each one and store the 
# value in our vector call mseVal

mseVal = c(ncol(x))*0
for(j in 1:ncol(x))
{
  mylm = lm(x$ViolentCrimesPerPop ~ x[,j], data = x)
  mse = mean((x$ViolentCrimesPerPop - predict(mylm))^2)
  # multiply the mse when storing to see the results better
  # this is because all the values for mse are very low due
  # to the way the data is formatted for values between 0 & 1
  mseVal[j] = mse*100
}

# Sort our vector of MSE  values for every variable
# and show the indexes of the best 25. order() shows
# the order of the indexes and sort() will show the 
# actual values 
order.mseVal <- order(mseVal)
sort.mseVal <- sort(mseVal)

# First value is the one being used as the first argurment
# the lm model which results in useless data and MSE, which
# is we go from 2:26 instead of 1:25

cat('\nShow the indexes of top 25 variables
in terms of smallest MSE\n\n', order.mseVal[2:26],'\n')
cat('\nShow the values of top 25 variables
in terms of smallest MSE\n\n', sort.mseVal[2:26],'\n')

# for subsets
numfolds = 10 
shufflerows = sample(c(1:nrow(x)))
rowsperfold = floor(nrow(x)/ numfolds)
reshufflerows = sample(x =c(1:nrow(x)), size = nrow(x))

# Splitting data into partitions for creating test sets
# for doing cross validation
residuals = c() #vector to store our residual values from each loop
mseCross = c(1:numfolds)
 for ( i in c(1:numfolds))
 {
  si = rowsperfold*(i-1) + 1 #start index
  ei = rowsperfold*i #end index
  testrows = shufflerows[c(si:ei)]
  trainrows = shufflerows[-c(si:ei)]
  xtrain = x[trainrows,]
  xtest = x[testrows,]
  mymod = lm(ViolentCrimesPerPop ~ PctKids2Par+ pctWInvInc  + PctPersDenseHous + pctWPubAsst + PctNotHSGrad + medFamInc + FemalePctDiv + PctIlleg, data =  xtrain)
  ypred = predict(mymod, newdata = xtest)
  ytruth = (xtest$ViolentCrimesPerPop)
  residuals = c(residuals, (ytruth-ypred))
  mseCross[i] = mean((xtest$ViolentCrimesPerPop - ypred)^2)
 }

formCall = mymod$call
cat('\nFormula used:')
print(formCall)
cat('\nAverage of MSE from cross-validation', mean((mseCross)))
cat('\nSqrt of MSE avg or RMSE: ', sqrt(mean(mseCross)))
