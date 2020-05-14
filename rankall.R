# Coursera | R Programming | Week 4 | Programming Assignment 3 | Hospital Quality
# Objective 4 : Ranking hospitals in all states

# Input arguments:
# An outcome name (outcome) 
# A hospital ranking (num)

rankall <- function(outcome, num = "best") {
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
        
        ## Checking the validity of outcome and num
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) 
                stop("invalid outcome")
        
        else if(num != 'best' & num !='worst' & num > length(unique(new_df[["State"]])))
                return(NA)
        
        ## For each state, find the hospital of the given rank
        else {
                if (outcome == 'heart attack') 
                        coln = 3
                else if (outcome == 'heart failure') 
                        coln = 4
                else 
                        coln = 5
                
                # converting the column as numeric to perform order operations
                new_df[ ,coln] <- suppressWarnings(as.numeric(new_df[,coln]))
                
                # Removing NAs
                new_df <- new_df[(!is.na(new_df[, coln])), ]
                
                resultVector <- vector()
                states <- sort(unique(new_df[, 2]))
                
                for(i in 1 : length(states)) {
                        # Subsetting based on each ith state
                        subsetted_df <- subset(new_df, State == states[i])

                        # Ordered with outcome and hospital name
                        ordered_df <- subsetted_df[order(subsetted_df[, coln], subsetted_df[, 1]), ]
                        
                        hospital <- if(num == 'best') 
                                ordered_df[1, 1]
                        else if(num == 'worst') 
                                ordered_df[nrow(ordered_df), 1]
                        else
                                ordered_df[num, 1]
                        
                        resultVector <- append(resultVector, c(hospital, states[i]))
                }
                
                ## Returning the data frame with the hospital names and the (abbreviated) state name
                resultVector <- as.data.frame(matrix(resultVector, length(states), 2, byrow = TRUE))
                colnames(resultVector) <- c("hospital", "state")
                rownames(resultVector) <- states
                return(resultVector)
        }
}


## Testing the function 

# > source("rankall.R")

# > head(rankall("heart attack", 20), 10)
# hospital state
# AK                                <NA>    AK
# AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
# AR   ARKANSAS METHODIST MEDICAL CENTER    AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# CA               SHERMAN OAKS HOSPITAL    CA
# CO            SKY RIDGE MEDICAL CENTER    CO
# CT             MIDSTATE MEDICAL CENTER    CT
# DC                                <NA>    DC
# DE                                <NA>    DE
# FL      SOUTH FLORIDA BAPTIST HOSPITAL    FL

# > tail(rankall("pneumonia", "worst"), 3)
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
# WV                     PLATEAU MEDICAL CENTER    WV
# WY           NORTH BIG HORN HOSPITAL DISTRICT    WY

# > tail(rankall("heart failure"), 10)
# hospital state
# TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
# TX                                        FORT DUNCAN MEDICAL CENTER    TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
# VA                                          SENTARA POTOMAC HOSPITAL    VA
# VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
# VT                                              SPRINGFIELD HOSPITAL    VT
# WA                                         HARBORVIEW MEDICAL CENTER    WA
# WI                                    AURORA ST LUKES MEDICAL CENTER    WI
# WV                                         FAIRMONT GENERAL HOSPITAL    WV
# WY                                        CHEYENNE VA MEDICAL CENTER    WY