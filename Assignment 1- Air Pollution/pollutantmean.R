pollutantmean<-function(directory, pollutant, id=1:332) {
  if (getwd()!='/home/geoff/RDir/specdata'){
    setwd(directory)
  }
  
  num<-c(0)
  denom<-c(0)
  for (i in id) {
    data<-read.csv(list.files()[i])
    numer<-sum(data[pollutant][!is.na(data[pollutant])])
    denomin<-sum(!is.na(data[pollutant]))
    num<-num + numer
    denom<-denom + denomin
    
  }
  num/denom 
}
