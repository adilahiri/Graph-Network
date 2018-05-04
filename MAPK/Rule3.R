# P-DAG rule 3 pg88-89 Koller book to direct an edge
Rule3<- function (Undirected,From_To){
  
  for (iter in 1: nrow(From_To)){
    
    Old_Parent <- From_To[iter,1]
    Child_Node <- From_To[iter,2]
    
    Pos_New_Parents <- which(From_To[,2]==Child_Node)
    New_Parents <- From_To[Pos_New_Parents,1]
    New_Parents <- New_Parents[-which(New_Parents==Old_Parent)]
    
    if (length(New_Parents)>0){
      for (iter_parent in New_Parents){
        
        Old_Parent_Neighbor <- which(Undirected[Old_Parent,]==1)
        Old_Parent_Neighbor <- Old_Parent_Neighbor[-which(Old_Parent_Neighbor==New_Parents)]
        Old_Parent_Neighbor <- Old_Parent_Neighbor[-which(Old_Parent_Neighbor==Child_Node)]
        
        New_Parent_Neighbor <- which(Undirected[New_Parents,]==1)
        New_Parent_Neighbor <- New_Parent_Neighbor[-which(New_Parent_Neighbor==Old_Parent)]
        New_Parent_Neighbor <- New_Parent_Neighbor[-which(New_Parent_Neighbor==Child_Node)]
        
        Child_Neighbor <- which(Undirected[Child_Node,]==1)
        Child_Neighbor <- Child_Neighbor[-which(Child_Neighbor==Old_Parent)]
        Child_Neighbor <- Child_Neighbor[-which(Child_Neighbor==New_Parents)]
        
        Common_Node <- intersect(intersect(Old_Parent_Neighbor,New_Parent_Neighbor),Child_Neighbor)
        
        if(length(Common_Node)>0){
          
          for(iter_common_node in Common_Node){
            
            pos1 <- which((From_To[,1]==iter_common_node & From_To[,2]==Old_Parent)|
                            (From_To[,2]==iter_common_node & From_To[,1]==Old_Parent) )
            if(length(pos1)>0)
              FLAG1<-FALSE
            else 
              FLAG1<-TRUE
            
            pos2 <- which((From_To[,1]==iter_common_node & From_To[,2]==New_Parents)| 
                            (From_To[,2]==iter_common_node & From_To[,1]==New_Parents))
            if(length(pos1)>0)
              FLAG2<-FALSE
            else 
              FLAG2<-TRUE
            
            pos3 <- which((From_To[,1]==iter_common_node & From_To[,2]==Child_Node)| 
                            (From_To[,2]==iter_common_node & From_To[,1]==Child_Node))
            if(length(pos1)>0)
              FLAG3<-FALSE
            else 
              FLAG3<-TRUE
            
            if(FLAG1 & FLAG2 & FLAG3)
              From_To<-rbind(From_To,c(iter_common_node,Child_Node))
            
          }
        }
        
        
      }
      
    }
    return(From_To)
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}