## Read outcome data
## Return hospital name in that state with the given rank
## 30-day death rate for that outcome and state


rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data and convert "Not Available" to NA
        outcome_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors=FALSE)
        outcome_list <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check that the state and outcome are valid
        if ((state %in% outcome_data[, 7]) == FALSE) { 
                stop("invalid state") 
        } else if ((outcome %in% outcome_list) == FALSE) {
                stop("invalid outcome")
        }
        
        ## subset data by outcome measure and then by state, and order by outcome results
        if (outcome == "heart attack") {
                ##get heart attack data for hospital name where state = state
                ha_by_state <- subset(outcome_data[,c(2,7,11)], outcome_data[, 7]== state)
                
                ##get rid of NA in the smaller data set 
                ha_by_state <- ha_by_state[complete.cases(ha_by_state), ]
                
                ## Turn the outcome column numeric so it will order correctly
                ## ha_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(ha_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
                
                ## Order dataset by outcome measure and then by hospital name
                ha_by_state <- ha_by_state[order(ha_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, ha_by_state$Hospital.Name),]
                
                ## display result matching best, worst or integer for "num"
                if (num == "best")  {
                        ha_by_state[1, 1]
                } else if (num == "worst") {
                        n <- nrow(ha_by_state)
                        ha_by_state[n, 1]
                } else {
                        n <- as.numeric(c(num))
                        ha_by_state[n, 1]
                }
                
        } else if (outcome == "heart failure") {
                ##get heart failure data for hospital name where state = state
                hf_by_state <- subset(outcome_data[,c(2,7,17)], outcome_data[, 7]== state)
                
                ##get rid of NA in the smaller data set 
                hf_by_state <- hf_by_state[complete.cases(hf_by_state), ]
                
                ## Turn the outcome column numeric so it will order correctly
                ##hf_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(hf_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
                
                ## Order dataset by outcome measure and then by hospital name
                hf_by_state <- hf_by_state[order(hf_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, hf_by_state$Hospital.Name),]
                
                if (num == "best")  {
                        hf_by_state[1, 1]
                } else if (num == "worst") {
                        n <- nrow(hf_by_state)
                        hf_by_state[n, 1]
                } else {
                        n <- as.numeric(c(num))
                        hf_by_state[n, 1]
                }
                
        } else if (outcome == "pneumonia") {
                ##get pneumonia data for hospital name where state = state
                pnu_by_state <- subset(outcome_data[,c(2,7,23)], outcome_data[, 7]== state)
                
                ##get rid of NA in the smaller data set 
                pnu_by_state <- pnu_by_state[complete.cases(pnu_by_state), ]
                
                ## Turn the outcome column numeric so it will order correctly        
                ## pnu_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(pnu_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
                
                ## Order dataset by outcome measure and then by hospital name
                pnu_by_state <- pnu_by_state[order(pnu_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, pnu_by_state$Hospital.Name),]
                
                if (num == "best")  {
                        pnu_by_state[1, 1]
                } else if (num == "worst") {
                        n <- nrow(pnu_by_state)
                        pnu_by_state[n, 1]
                } else  {
                        n <- as.numeric(c(num))
                        pnu_by_state[n, 1]
                }
        }
      
}