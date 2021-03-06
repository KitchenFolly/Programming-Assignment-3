## this function finds single hospital in each state that is ranked (num)
## and returns it in a grid of states. Used also for finding "best"'" and "worst"

rankall <- function(outcome, num = "best") {
        ## Read outcome data and convert "Not Available" to NA
        outcome_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors=FALSE)
        
        ## setup data needed
        outcome_list <- c("heart attack", "heart failure", "pneumonia")
        state_list <- c(state.abb, "PR")
        h_list <- data.frame()
        
        ## Check that the outcome is valid
        if ((outcome %in% outcome_list) == FALSE) {
                stop("invalid outcome")
        }
        
        ## subset data by outcome
        if (outcome == "heart attack") {
                oc_dat <- subset(outcome_data[,c(2,7,11)])
                
                } else if(outcome == "heart failure") {
                        oc_dat <- subset(outcome_data[,c(2,7,17)])
                        
                } else if(outcome == "pneumonia") {
                        oc_dat <- subset(outcome_data[,c(2,7,23)])
                                      
                }   
        
        ## set column names to be easier to work with
        names(oc_dat) <- c("hospital", "state", "outcome")        
        
        ##get rid of NA in the smaller data set 
        oc_dat <- oc_dat[complete.cases(oc_dat), ]
        
        ## Order dataset by state, outcome and  hospital 
        oc_dat <- oc_dat[order(oc_dat$state, oc_dat$outcome, oc_dat$hospital),]
        
        ##split the data by state
        oc_dat <- split(oc_dat, oc_dat$state)
                
        ##iterate over the split data to select single row matching num
        h_list <- lapply(oc_dat, function(x) {
                if (num == "best")  {
                        n <- 1
                } else if (num == "worst") {
                        n <- nrow(x)
                } else {
                        n <- as.numeric(num)
                }
                (x[n, c(1,2)])
                ##return(x[n, c(1,2)])
        })
        
        ## diag - str(h_list) ## we have a list of 54 data frames
        ## diag - return(h_list) ## to see data before transformation   
        
        ## format the data as a list of states with single hospital values per
        final <- do.call("rbind", h_list)
        columns = c("hospital", "state")
        rows = state_list
        return(final)
           
       
}       
        
       
                
                     
             
                
             
                
        
        
        

