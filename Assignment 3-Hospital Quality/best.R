best<-function(state,outcome){
  dat<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available", stringsAsFactors=FALSE)
  
  if (is.na(match(state,dat[,7]))){
    stop('invalid state')
  }
  
  nam<-c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  if (!(outcome %in% names(nam))){
    stop('invalid outcome')
  }
  
  
  my_dat<-data.frame(dat[,2],dat[,7],dat[,nam[outcome]])
  my_dat<-na.omit(my_dat)
  my_dat<-my_dat[my_dat[,2]==state,]
  my_dat<-my_dat[my_dat[,3] == min(my_dat[,3]),]
  sort(my_dat[,1])[1]
}
