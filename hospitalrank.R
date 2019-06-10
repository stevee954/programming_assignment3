
rankhospital <- function(state, outcome,num="best") {
  ## Read outcome data
  d<-read.csv("c:/GitRepos/prog_assign3/outcome-of-care-measures.csv")
  d2<-d[,c(2,7,11,17,23)]
  names(d2)[3] <-"heart attack"
  names(d2)[4] <-"heart failure"
  names(d2)[5] <-"pneumonia"
  
  my_num<-num;
  
  my_state<-state
  
  
  ## Check that state and outcome are valid
  states<-unique(d2[,2])
  
  valid_state<- my_state %in% states
  
  if (valid_state != TRUE) {stop("invalid state")} 
  
  condition_names<-c("heart attack","heart failure","pneumonia")
  
  my_outcome<-outcome  ##"pneumonia"   
  
  
  z<-match(my_outcome,names(d2))  ##get column number
  
  valid_condition <- my_outcome %in% condition_names
  
  if (valid_condition != TRUE) {stop("invalid condition")} 
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  
  d3<-data.frame(d2$Hospital.Name,d2$State, d2[,z])
  
  names(d3)[3]<-'Measure'
  
  
  
  d4<-subset(d3,d3$d2.State==my_state & d3$Measure != 'Not Available')
  
  
  
  
  d5<-d4[
    order( d4[,3], d4[,1] ),
    ]
  
  my_best<-1:my_num
  
  d5[my_best,c(1,2,3)]
  
  
}

rankhospital("CA", "heart failure",num=20)



