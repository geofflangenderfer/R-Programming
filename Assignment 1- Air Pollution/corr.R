corr<-function(directory, threshold=0){
  if (getwd()!='/home/geoff/RDir/specdata'){
    setwd(directory)
  }
  vec<-c()
  for (i in 1:332) {
    data<-read.csv(list.files()[i])
    if (complete('specdata',i)[2] > threshold){
      vec<-append(vec,cor(data$sulfate, data$nitrate, use='complete.obs'))
    }
    
  }
  vec
}