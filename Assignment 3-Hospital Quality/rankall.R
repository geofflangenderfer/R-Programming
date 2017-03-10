rankall<-function (outcome, num = 'best'){
  dat<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available", stringsAsFactors=FALSE)
  
  nam<-c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  if (!(outcome %in% names(nam))){
    stop('invalid outcome')
  }
  
  
  my_dat<-data.frame(dat[,2],dat[,7],dat[,nam[outcome]])
  names(my_dat)<-c('Hospital','State','P(D)')
  my_dat<-na.omit(my_dat)
  my_dat$Hospital<-as.character(my_dat$Hospital)
  init<-my_dat
  state<-levels(my_dat$State)
  
  hospital<-c()
  for (i in 1:length(state)){
    
    my_dat<-init[init[,2]==state[i],]
    ord<-order(my_dat[,3])
    my_dat<-my_dat[ord,]
    
  
    
    for (j in 1:(nrow(my_dat)-1)) {
      
      if (nrow(my_dat) > 1){
        if (my_dat[j,3] == my_dat[j+1,3]){
          tie<-c(my_dat[j,1],my_dat[j+1,1])
          ord1<-order(tie)
          
          if (ord1[1] == 2){
            tmpRow<-my_dat[j,]
            my_dat[j,] <-my_dat[j+1,]
            my_dat[j+1,]<-tmpRow
          }
        }
      }  
    }
  
    
    

    
    if (num =="best"){
      hospital<-c(hospital,my_dat[1,1])
    }else if (num == "worst") {
      hospital<-c(hospital,my_dat[nrow(my_dat),1])
    }else if (num > nrow(my_dat)) {
      hospital<-c(hospital,NA)
    }else 
      hospital<-c(hospital,my_dat[num,1])
    
  }
  data.frame(hospital,state)
}