convert_numbered_edge <- function(pos_ud){
Edges_ud<-matrix(nrow=dim(pos_ud)[1],ncol=dim(pos_ud)[2],dimnames = list(NULL, c("from", "to")))
  for (iter in 1: dim(pos_ud)[1]){
    Edges_ud[iter,1]<-LETTERS[pos_ud[iter,1]]
    Edges_ud[iter,2]<-LETTERS[pos_ud[iter,2]]
  }
 return(Edges_ud)
}