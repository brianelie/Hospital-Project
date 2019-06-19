rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")[,c(2,7,11,17,23)]
    
    ## Check that state and outcome are valid
    if (!state %in% data[,2]){
        stop("invalid state")
    }
    
    if (outcome == "heart attack"){
        my_data <- data[c(1,2,3)]
    } else if (outcome == "heart failure"){
        my_data <- data[c(1,2,4)]
    } else if (outcome == "pneumonia"){
        my_data <- data[c(1,2,5)]
    } else{
        stop("invalid outcome")
    }
    names(my_data)<-c("Name","State","Rate")
    
    ## Return hospital name in that state with the given rank
    # Select the state data
    my_data <- my_data[my_data$State==state,]
    # Convert data into numbers for order
    my_data$Rate <- suppressWarnings(as.numeric(as.character(my_data$Rate)))
    # Remove NAs
    my_data <- my_data[!is.na(my_data$Rate),]
    # Rank data
    data_ordered <- my_data[order(my_data$Rate,my_data$Name),]
    # Find the index of the right ranking
    if (num == "best"){
        num <- 1
    } else if (num == "worst"){
        num <- length(my_data$Name)
    } else {
        num <- num
    }
    # top_index <- which(ranking == num)
    # Pull out the applicable hospital name
    return(data_ordered$Name[num])
    ## 30-day death rate
}

## Test Cases
rankhospital("TX", "heart failure", 4)
# "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst")
# "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000)
# NA