complement <- function(Mat_Object){
  for (iter in 1: nrow(Mat_Object)){
    pos0 <- which(Mat_Object[iter,]==0)
    pos1 <- which(Mat_Object[iter,]==1)
    Mat_Object[iter,pos0]<-1
    Mat_Object[iter,pos1]<-0
  }
  return(Mat_Object)
}