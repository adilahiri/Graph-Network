find_neighbors <- function (Skeleton_Graph)
{
  L= list()
  for (iter in 1: dim(Skeleton_Graph)[1]){
    pos<- which(Skeleton_Graph[iter,]==1)
    pos<-pos[-c(which(iter==pos))]
    if(length(pos)==0){pos<-NA}
    L[[iter]]<- pos
  }
 return(L)
}