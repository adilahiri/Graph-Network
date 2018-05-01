# P-DAG rule 2 pg88-89 Koller book to direct an edge and avoid cycles
Rule2 <- function (Undirected, From_To){
  
  for (iter in 1: nrow(From_To)){
    Parent_Node <- From_To[iter,1]
    Child_Node <- From_To[iter,2]
    pos_descendants <- which(From_To[,1]==Child_Node)
    Directed_Descendants <- From_To[pos_descendants,2]
    
    if(length(Directed_Descendants) > 0) {
      
      
      for (iter_des in Directed_Descendants){
        
        Adj_Nodes_des <- which(Undirected[iter_des,]==1)
       
        if( any(Adj_Nodes_des==Parent_Node)){
          
          pos_des <- which( (From_To[,1]==Parent_Node & From_To[,2]==iter_des) | 
                              (From_To[,2]==Parent_Node & From_To[,1]==iter_des) )
          
          if(length(pos_des)==0)
            From_To<-rbind(From_To,c(Parent_Node,iter_des))
        }
        
      }
      
      
      
      
      
    }  
   
    
    
  }
  return(From_To)
  
}
