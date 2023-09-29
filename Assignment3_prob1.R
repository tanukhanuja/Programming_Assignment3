##1. Plot the 30-day mortality rates for heart attack
#load csv file in outcome, all variables as character class
outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")

head(outcome)   #check data 
ncol(outcome)   #how many columns are there?

#read 11th column as numeric to plot histogram
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])



