Check_Immo_Cycle<- function (skeleton,Set_IMMO,Immorality_Mat){
  Set_Adj_Nodes <- matrix(ncol=3, nrow=dim(Immorality_Mat)[1]-dim(Set_IMMO)[1])
  for (iter in 1: dim(Set_IMMO)[1]){
      store_pos<-which(apply(Immorality_Mat, 1, function(x) all(x == Set_IMMO[iter,]))==TRUE)
      Immorality_Mat<-Immorality_Mat[-store_pos,]
  }
  directed_Nodes<-Immorality_Mat
  
  
  
  for  (iter1 in 1: dim(directed_Nodes)[1]){
    for(iter2 in 1:dim(directed_Nodes[1])){
      
      if(directed_Nodes[iter1,3]==directed_Nodes[iter2,1]){
        directed_Nodes<-directed_Nodes[-c(iter2),]
      }
    }
    
    
  }
}

