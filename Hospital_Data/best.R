## This function takes state and outcome data and evaluates the hospital with the
## best outcome = outcome  in a state = state producing one single hospital as 
## a result. Note that ties are broken by alphabetical ordering of hospital names

best <- function(state, outcome)  {
        
        ## Read outcome data and convert "Not Available" to NA
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        outcome_list <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check that the state and outcome are valid
        ##compare state to list of states found in the data, and stop if not found
        if ((state %in% outcome_data[, 7]) == FALSE) { 
                stop("invalid state") 
                
        } 
                ## Compare outcome to outcome list provided and stop if not found
                else if ((outcome %in% outcome_list) == FALSE) {
                        stop("invalid outcome") 
                }
        
        ## subset data by outcome measure and then by state, and order by outcome results
        if (outcome == "heart attack") {
                ##get heart attack data for hospital name where state = state
                ha_by_state <- subset(outcome_data[,c(2,7,11)], outcome_data[, 7]== state)
                
                ##get rid of NA in the smaller data set 
                ha_by_state <- ha_by_state[complete.cases(ha_by_state), ]
                
                ## Turn the outcome column numeric so it will order correctly
                ha_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(ha_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
                
                ## Order dataset by outcome measure and then by hospital name
                ha_by_state <- ha_by_state[order(ha_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, ha_by_state$Hospital.Name),]
                
                ## display the top 1 result, only the hospital name
                ha_by_state[1, 1]
            
                } 
                else if (outcome == "heart failure") {
                        ##get heart failure data for hospital name where state = state
                        hf_by_state <- subset(outcome_data[,c(2,7,17)], outcome_data[, 7]== state)
                        
                        ##get rid of NA in the smaller data set 
                        hf_by_state <- hf_by_state[complete.cases(hf_by_state), ]
                        
                        ## Turn the outcome column numeric so it will order correctly
                        hf_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(hf_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
                        
                        ## Order dataset by outcome measure and then by hospital name
                        hf_by_state <- hf_by_state[order(hf_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, hf_by_state$Hospital.Name),]
                        
                        ## display the top 1 result, only the hospital name
                        hf_by_state[1, 1]
                        
                
                } 
                else  if (outcome == "pneumonia") {
                        ##get pneumonia data for hospital name where state = state
                        pnu_by_state <- subset(outcome_data[,c(2,7,23)], outcome_data[, 7]== state)
                                
                        ##get rid of NA in the smaller data set 
                        pnu_by_state <- pnu_by_state[complete.cases(pnu_by_state), ]
                        
                        ## Turn the outcome column numeric so it will order correctly        
                        pnu_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(pnu_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
                                
                        ## Order dataset by outcome measure and then by hospital name
                        pnu_by_state <- pnu_by_state[order(pnu_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, pnu_by_state$Hospital.Name),]
                                
                        ## display the top 1 result, only the hospital name        
                        pnu_by_state[1, 1]
     
                }
}
 