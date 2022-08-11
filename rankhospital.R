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
  
  } else if (is.numeric(rank)) {
    
    state_index <<- which(subdata[, "state"] == state)  ## index values of global dataset for particular state
    state_data_table <<- subdata[state_index, ]    ## extracting  all data for the called state
    state_data_table[,eval(outcome)] <- as.numeric (state_data_table[,eval(outcome)]) ##Assure numeric values for outcome
    state_data_table <- state_data_table[order(state_data_table[, eval(outcome)], state_data_table[, "hospital"]),]
    output <- state_data_table[, "hospital"][rank]
   

  } else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      state_index <<- which(subdata[, "state"] == state)  ## index values of global dataset for particular state
      state_data_table <<- subdata[state_index, ]    ## extracting  all data for the called state
      state_data_table[,eval(outcome)] <- as.numeric (state_data_table[,eval(outcome)]) ##Assure numeric values for outcome
      state_data_table <- state_data_table[order(state_data_table[, eval(outcome)], state_data_table[, "hospital"],decreasing=TRUE),]
      output <- state_data_table[, "hospital"][1]

    } else {
      stop('invalid rank')
    }
  }
  #
  return(output)
}
  #