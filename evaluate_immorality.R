evaluate_immorality<-function(Immorality_Mat,POS_IND,Witness_Indep){
  M<-Immorality_Mat[,c(1,3)]
  Witness <-rep(0,length(Immorality_Mat)/3)
  for (iter in 1:((length(Immorality_Mat)/3))){
  store_pos<-which(apply(POS_IND, 1, function(x) all(x == M[iter,]))==TRUE)
  Witness[iter] <-  Witness_Indep[store_pos]
  }
  
  return(Witness)##remve unlist before witness
}