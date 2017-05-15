## This function takes state and outcome data and evaluates the hospital with the
## best outcome = outcome  in a state = state producing one single hospital as 
## a result. Note that ties are broken by alphabetical ordering of hospital names

best <- function(state, outcome)  {
        
        ## Read outcome data and convert "Not Available" to NA
        outcome_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors=FALSE)
        
        ## setup data needed
        outcome_list <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check that the state and outcome are valid
        if ((state %in% outcome_data[, 7]) == FALSE) { 
                stop("invalid state") 
        } else if ((outcome %in% outcome_list) == FALSE) {
                stop("invalid outcome")
        }
        
        ## subset data by outcome and by a single state = "state"
        if (outcome == "heart attack") {
                oc_dat <- subset(outcome_data[,c(2,7,11)], outcome_data[, 7]== state)
                
        } else if(outcome == "heart failure") {
                oc_dat <- subset(outcome_data[,c(2,7,17)], outcome_data[, 7]== state)
                
        } else if(outcome == "pneumonia") {
                oc_dat <- subset(outcome_data[,c(2,7,23)], outcome_data[, 7]== state)
                
        }   
        
        ## set column names to be easier to work with
        names(oc_dat) <- c("hospital", "state", "outcome")        
        
        ##get rid of NA in the smaller data set 
        oc_dat <- oc_dat[complete.cases(oc_dat), ]
        
        ## Order dataset by state, outcome and  hospital 
        oc_dat <- oc_dat[order(oc_dat$state, oc_dat$outcome, oc_dat$hospital),]
        
        ## display the top 1 result, only the hospital name
        oc_dat[1, 1]
        
       
               
}
 