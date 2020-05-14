# Coursera | R Programming | Week 4 | Programming Assignment 3 | Hospital Quality
# Objective 3 : Ranking hospitals by outcome in a state

# Input arguments: 
# The 2-character abbreviated name of a state (state), 
# An outcome (outcome),  
# The ranking of a hospital in that state for that outcome (num).
# The num argument can take values "best", "worst", or an integer indicating the ranking (smaller numbers are better).

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
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
        
        ## Checking the validity of state, outcome and num
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) 
                stop("invalid outcome")
        
        else if(!state %in% unique(new_df[["State"]]))
                stop("invalid state")
        
        else if(num != 'best' & num !='worst' & num > length(unique(new_df[["State"]])))
                return(NA)
        
        
        ## Returning hospital name in the specified state with the given rank 30-day death rate
        else {
                if (outcome == 'heart attack') 
                        coln = 3
                else if (outcome == 'heart failure') 
                        coln = 4
                else 
                        coln = 5
                
                # Subsetting based on the state specified
                subsetted_df <- subset(new_df, State == state)
                
                # converting the column as numeric to perform order operations
                subsetted_df[ ,coln] <- suppressWarnings(as.numeric(subsetted_df[,coln]))
                
                # Removing NAs
                subsetted_df <- subsetted_df[(!is.na(subsetted_df[, coln])), ]
                
                # Ordered with outcome and hospital name
                ordered_df <- subsetted_df[order(subsetted_df[, coln], subsetted_df[, 1]), ]
                
                if(num == 'best')
                        num = 1
                else if(num == 'worst')
                        num = nrow(ordered_df)
                
                # Returning the name of the hospital of the rank specified in the parameter
                return(ordered_df[num, 1])
        }
}


## Testing the function:

# > source("rankhospital.R")

# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"

# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"

# > rankhospital("MN", "heart attack", 5000)
# [1] NA