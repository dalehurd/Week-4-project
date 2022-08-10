rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  ## define the input data set equal to 'data'
  subdata   <- as.data.frame(cbind(data[, 2],   # hospital
                                   data[, 7],   # state
                                   data[, 11],  # heart attack
                                   data[, 17],  # heart failure
                                   data[, 23]), # pneumonia
                             stringsAsFactors = FALSE)
  colnames(subdata) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  if(!is.element (state,subdata[, "state"])) {  ## Check for valid state name
    stop('invalid state')
    
  } else if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))){  ## Check for valid outcome
    stop('invalid outcome')
  } else if {
    state_index <- which(subdata[, "state"] == state)  ## index values of global dataset for particular state
    state_data_table <- subdata[state_index, ]    # extracting  all data for the called state
  } else if (is.numeric(rank)) {
    
    state_index <- which(subdata[, "state"] == state)  ## index values of global dataset for particular state
    state_data_table <- subdata[state_index, ]    # extracting  all data for the called state
    outcome_values <- as.numeric(state_data_table[, eval(outcome)]) # extracting only the outcome data for analysis
    min_val <- min(outcome_values, na.rm = TRUE)  ## finding the minimum value for specified outcome
    result  <- state_data_table[, "hospital"][which(outcome_values == min_val)] ## finding all locations with minimum values
    
    output  <- result[order(result)]  ##Alphabetizing
    
   ## si <- which(fd[, "state"] == state)
   ## ts <- fd[si, ]                     # extracting dataframe for the called state
   ## ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
   ## ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"]), ]
   ## output <- ts[, "hospital"][rank]
  } else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      si <- which(fd[, "state"] == state)
      ts <- fd[si, ]    
      ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
      ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"], decreasing = TRUE), ]
      output <- ts[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  return(output)