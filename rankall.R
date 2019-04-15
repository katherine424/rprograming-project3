rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  measures<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  if (tolower(outcome) == "heart attack")  index <- 11
  else if (tolower(outcome) == "heart failure")  index<-17
  else if (tolower(outcome) == "pneumonia")   index <-23
  else
  {
    message("invalid outcome")
    stop
  }
  selected <- measures[,c(2,7,index)]
  selected[,3]=as.numeric(selected[,3])
  selectedna <- selected[complete.cases(selected),]
  spilted<-split(selectedna,selectedna[,2])
  splitordered<-lapply(spilted,function(x) {x[order(x[,3],x[,1]),]})
  n<-lapply(splitordered,nrow)
  hospital<-rep(0,length(n))
  state<-rep(0,length(n))
  for (i in 1:length(n))
  {   
      state[i]<-names(n[i])
      if (tolower(num) =="best")  nn<-1 
      else if (tolower(num) == "worst") nn<-as.numeric(n[i])
      else nn<-as.numeric(num)
      hospitallist <- as.data.frame(splitordered[i])
      if (nn>nrow(hospitallist)) hospital[i]<-NA
      else hospital[i]<-hospitallist[nn,1]
      
  }
  data.frame(hospital,state)
}
  