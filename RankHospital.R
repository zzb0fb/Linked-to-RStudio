rankhospital<-function(state, outcome, rank="best"){
  ## The function reads the outcome-of-care-measures.csv file and returns a 
  ## character vector with the name of the hospital that has the ranking specified by the num argument  
  rank_input_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  rankdataframe   <- as.data.frame(cbind(data[, 2],   # hospital name
                                         data[, 7],   # state
                                         data[, 11],  # heart attack
                                         data[, 17],  # heart failure
                                         data[, 23]), # pneumonia
                                   stringsAsFactors = FALSE)
  ## cambio el nombre a las columnas para hacerlas más manejables
  colnames(rankdataframe) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% rank_input_outcome$State){
    stop('invalid state')
  } else if (!outcome %in% colnames(rankdataframe)) {
    stop('invalid outcome')
  } else if (is.numeric(rank)) {
    rowstate<-which(rankdataframe[, "state"] == state) # fila en la que está el estado
    datastate<-rankdataframe[rowstate, ]   # extraigo los datos del estado
    datastate[, eval(outcome)] <- as.numeric(datastate[, eval(outcome)])
    datastate <- datastate[order(datastate[, eval(outcome)], datastate[, "hospital"]), ]    
    output <- datastate[, "hospital"][rank]    
  } else if (rank=="best"){
    output <- best(state, outcome)
  } else if (rank=="worst"){
    rowstate<-which(rankdataframe[, "state"] == state) # fila en la que está el estado
    datastate<-rankdataframe[rowstate, ]   # extraigo los datos del estado
    datastate[, eval(outcome)] <- as.numeric(datastate[, eval(outcome)])
    datastate <- datastate[order(datastate[, eval(outcome)], datastate[, "hospital"],decreasing = TRUE),  ]    
    output <- datastate[, "hospital"][1] 
  } else {
    stop ('invalid rank')
  }  
  
  return(output)
}