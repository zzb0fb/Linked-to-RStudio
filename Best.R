best <- function(state, outcome) {
  ## Read outcome data
  input_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Extraigo un data frame para trabajar sólo con las columnas que necesito
  dataframe   <- as.data.frame(cbind(data[, 2],   # hospital name
                                     data[, 7],   # state
                                     data[, 11],  # heart attack
                                     data[, 17],  # heart failure
                                     data[, 23]), # pneumonia
                               stringsAsFactors = FALSE)
  ## cambio el nombre a las columnas para hacerlas más manejables
  colnames(dataframe) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Validar que los datos que recibe la función son válidos
  if(!state %in% input_outcome$State){
    stop('invalid state')
  } else if (!outcome %in% colnames(dataframe)) {
    stop('invalid outcome')
  } else {
    rowstate<-which(dataframe[, "state"] == state) # filas en la que está el estado
    datastate<-dataframe[rowstate, ]               # extraigo los datos del estado
    valores <- as.numeric(datastate[, eval(outcome)])   # extraigo valores de la enfermedad
    lowest_val <- min(valores, na.rm = TRUE)            # obtengo el valor más bajo
    ## When na. rm is TRUE, the function skips over any NA values
    # output  <- datastate[, "hospital"][which(valores == lowest_val)]
  }
  return(datastate[, "hospital"][which(valores == lowest_val)])
}