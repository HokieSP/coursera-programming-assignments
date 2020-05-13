# Coursera | R Programming | Week 4 | Programming Assignment 3 | Hospital Quality
# Objective 2: Finding the best hospital in a state

# Input arguments: 
# The 2-character abbreviated name of a state and an outcome name
# The outcomes can be one of "heart attack", "heart failure", or "pneumonia"
best <- function(state, outcome) {

        # Reading data
        outcomeFile <- read.csv("outcome-of-care-measures.csv", header =T, stringsAsFactors=F)
        names(outcomeFile)
        
        # Required fields:
        # Field : [2] "Hospital.Name"
        # Field : [7] "State"
        # Field : [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
        # Field : [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
        # Field : [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
        new_df <- outcomeFile[, c(2,7,11,17,23)]
        
        # Renaming fields for simplicity and readability
        colnames(new_df) <- c("hospital", "State", "hospitals_HA", "hospitals_HF", "hospitals_P")
        
        # Checking the validity of state and outcome 
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) 
                stop("invalid outcome!")
        
        else if(!state %in% unique(new_df[["State"]]))
                stop("invalid state!")
        
        # Returning hospital name in the specified state with lowest 30-day death rate
        else {
                if (outcome == "heart attack") 
                        coln = 3
                else if (outcome == "heart failure") 
                        coln = 4
                else 
                        coln = 5
                
                # Subsetting based on the state specified
                subsetted_df <- subset(new_df, State == state)
                
                # Row/s with the minimum value
                min_rows_subset <- suppressWarnings(which(as.numeric(subsetted_df[,coln]) == 
                                         min(as.numeric(subsetted_df[,coln]), 
                                             na.rm = TRUE)))
                
                # Hospital name/s corresponding to the row/s with minimum value
                hospitalNames <- subsetted_df[min_rows_subset, 1]
                
                # Sorting hospitals 
                hospitalNames <- sort(hospitalNames)
                
                # Returning the first hospital in the order
                return(hospitalNames[1])
        }
}

# Testing the function:

# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state!
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome!