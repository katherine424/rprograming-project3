rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  ## The outcomes can
  ## be one of \heart attack", \heart failure", or \pneumonia".
  measures<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  possible_state<-(unique(measures$State)==toupper(state))
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
  { 
    outcomestate<-(measures[,7]==toupper(state))
    if (tolower(outcome) == "heart attack")  index <- 11
    else if (tolower(outcome) == "heart failure")  index<-17
    else if (tolower(outcome) == "pneumonia")   index <-23
    else stop(print("Not valid input for outcome."))
  
    selected <- measures[outcomestate,c(2,index)]
    selected[,2]=as.numeric(selected[,2])
    selectedna <- selected[complete.cases(selected),]
    ordered<-selectedna[order(selectedna[,2],selectedna[,1]),]
  }
  n<-nrow(ordered)
  if (tolower(num) =="best")
    hospital <- ordered[1,1]
  else if (tolower(num) == "worst")
    hospital <- ordered[n,1]
  else hospital <- ordered[as.numeric(num),1]
  hospital    
}
  
