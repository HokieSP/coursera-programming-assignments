# Coursera | R Programming | Week 4 | Programming Assignment 3 : Hospital Quality
# Objective 1 : Plot the 30-day mortality rates for heart attack

#Reading data
outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
ncol(outcomeFile)
names(outcomeFile)

#Because we originally read the data in as character (by specifying colClasses = "character" we need to
#coerce the column to be numeric. 
outcomeFile[, 11] <- as.numeric(outcomeFile[, 11])
hist(outcomeFile[, 11])

