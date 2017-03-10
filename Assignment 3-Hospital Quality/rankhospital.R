rankhospital<-function (state, outcome, num="best"){
  dat<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available", stringsAsFactors=FALSE)
  
  if (is.na(match(state,dat[,7]))){
    stop('invalid state')
  }
  
  nam<-c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  if (!(outcome %in% names(nam))){
    stop('invalid outcome')
  }
  
  
  my_dat<-data.frame(dat[,2],dat[,7],dat[,nam[outcome]])
  names(my_dat)<-c('Hospital','State','P(D)')
  my_dat<-na.omit(my_dat)
  my_dat<-my_dat[my_dat[,2]==state,]
  my_dat$Hospital<-as.character(my_dat$Hospital)
  ord<-order(my_dat[,3])
  my_dat<-my_dat[ord,]
  
  if (is.numeric(num) & num > nrow(my_dat)) {
    return(NA)
  }
  
  
  for (i in 1:(nrow(my_dat)-1)) {
    
    if (my_dat[i,3] == my_dat[i+1,3]){
      tie<-c(my_dat[i,1],my_dat[i+1,1])
      ord1<-order(tie)
      
      if (ord1[1] == 2){
        tmpRow<-my_dat[i,]
        my_dat[i,] <-my_dat[i+1,]
        my_dat[i+1,]<-tmpRow
      }
    }
  }
  
  
  if (num =="best"){
    my_dat[1,1]
  }
  if (num == "worst") {
    my_dat[nrow(my_dat),1]
  }
  else {
    my_dat[num,1]
    
  }
  
}