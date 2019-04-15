best <- function(state, outcome) {
  ## Read outcome data
  ## The outcomes can
  ## be one of \heart attack", \heart failure", or \pneumonia".
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcome_data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  possible_state<-(unique(outcome_data$State)==toupper(state))
  possible_outcome <-(c("heart attack","heart failure","pneumonia")==tolower(outcome))
  if (sum(possible_state) != 1)
  {
    message("invalid state.")
    stop
    ##stop(print("invalid state."))
  }
  else if (sum(possible_outcome) != 1)
  {
    message("invalid outcome.")
    stop
    ##stop(print("invalid outcome."))
  }
  else 
  { outcomestate<-(outcome_data[,7]==toupper(state))
    if (tolower(outcome) == "heart attack")  index <- 11
    else if (tolower(outcome) == "heart failure")  index<-17
    else if (tolower(outcome) == "pneumonia")   index <-23
    else stop(print("Not valid input for outcome."))
    selected <- measures[outcomestate,c(2,index)]
    selected[,2]=as.numeric(selected[,2])
    selectedna <- selected[complete.cases(selected),]
    ordered<-selectedna[order(selectedna[,2],selectedna[,1]),]
  }
  ordered[1,1]
}