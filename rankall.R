rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
    ## Check that outcome are valid
    if (outcome == "heart attack"){
        my_data <- data[,c(1,2,3)]
    } else if (outcome == "heart failure"){
        my_data <- data[,c(1,2,4)]
    } else if (outcome == "pneumonia"){
        my_data <- data[,c(1,2,5)]
    } else{
        stop("invalid outcome")
    }
    names(my_data)<-c("Name","State","Rate")
    
    ## For each state, find the hospital of the given rank
    # Convert death rates to numbers
    my_data[,3] = suppressWarnings(as.numeric(my_data[,3]))
    # Split data into a list with 50 elements
    split_data <- split(my_data,my_data$State)
    
    soln <- lapply(split_data, function(x, num) {
        # Order by Rate and then Name
        x = x[order(x$Rate, x$Name),]

        # Return name with applicable rate
        if (num == "best"){
            return (x$Name[1])
        } else if (num == "worst"){
            return (x$Name[nrow(x)])
        } else {
            return (x$Name[num])
        }
    }, num)
    return (data.frame(Hospital=unlist(soln), State=names(soln)) )
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)