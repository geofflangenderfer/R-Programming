complete<-function(directory, id=1:332) {
  if (getwd()!='/home/geoff/RDir/specdata'){
    setwd(directory)
  }
  mat<-matrix(nrow=length(id),ncol=2)
  colnames(mat)=c('id','nobs')
  count = 0
 
  for (i in id) {
    data<-read.csv(list.files()[i])
    count<-0
    
    for (x in 1:nrow(data)) {
      if (!is.na(data[x,2]) & !is.na(data[x,3])) {
        count<-count + 1
      }
    }
    mat[match(i,id),]=c(data[i,4],count)
  }
  mat
}