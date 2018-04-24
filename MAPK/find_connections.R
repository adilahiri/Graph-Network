find_connections <- function (Node_Number,row_data){
   Parent_Nodes <- which(row_data==1)
   Position_NodeNum <- which(Parent_Nodes==Node_Number)
   
   if(length(Position_NodeNum>0)){
    Parent_Nodes<- Parent_Nodes[-which(Parent_Nodes==Node_Number)]
   }
   
  
   List_Connections <- combn(Parent_Nodes,2,simplify = FALSE)
   Matrix_Connection <-matrix(nrow=length(List_Connections),ncol=2)    
    for (iter in 1 : length(List_Connections)){
     Matrix_Connection[iter,]<- List_Connections[[iter]]
   }
   
   Center_Col<-cbind(rep(Node_Number,times=nrow(Matrix_Connection))) 
   Matrix_Connection <- cbind(Matrix_Connection[,1],Center_Col,Matrix_Connection[,2])
   return(Matrix_Connection)  
}
