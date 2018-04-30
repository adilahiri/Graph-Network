Rule1 <- function (Undirected, From_To){
  for (iter in 1: nrow(From_To)){
    
    Child_Node <- From_To[iter,2]
    Descendant <- which(Undirected[Child_Node,]==1) 
    Descendant<-Descendant[-which(Descendant==From_To[iter,1])]
    
    if(length(Descendant)>0){
      
      for(iter_des in Descendant){
        
        pos_des <- which( (From_To[,1]==Child_Node & From_To[,2]==iter_des) | 
                            (From_To[,2]==Child_Node & From_To[,1]==iter_des) )
        if(length(pos_des)==0)
          From_To<-rbind(From_To,c(Child_Node,iter_des))
        
      }
      
    }
    
  }
  return (From_To)
}