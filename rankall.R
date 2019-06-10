##install.packages("dplyr")
##library(dplyr)


rankall <- function(outcome,num="best") {
  ## Read outcome data
  d<-read.csv("c:/GitRepos/prog_assign3/outcome-of-care-measures.csv")
  d2<-d[,c(2,7,11,17,23)]
  names(d2)[3] <-"heart attack"
  names(d2)[4] <-"heart failure"
  names(d2)[5] <-"pneumonia"
  
  my_num<-num;
  
  condition_names<-c("heart attack","heart failure","pneumonia")
  
  my_outcome<-outcome  ##"pneumonia"   
  
  
  z<-match(my_outcome,names(d2))  ##get column number
  
  valid_condition <- my_outcome %in% condition_names
  
  if (valid_condition != TRUE) {stop("invalid condition")} 
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  d3<-data.frame(d2$Hospital.Name,d2$State, d2[,z])
  
  names(d3)[3]<-'Measure'
  
  d4<-subset(d3,d3$Measure != 'Not Available')
  
    d5 <- d4 %>%
    group_by(d2.State) %>%
    arrange(Measure) %>%
    mutate(rank = order(Measure))
    
  
  d6<-data.frame()
  
  d6<-subset(d5,d5$rank==my_num)
  
  ##d6-filter(d5, d5$rank==20)  --incorrect length (3947), expecting: 10 
  
  d7<-d6[
    order( d6[,2]),
    ]
  
  d7
  }

rankall("heart failure",num=1)






