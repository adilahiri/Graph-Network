find_undirected<-function(From_To,Original_Undirected){
  pos1<-vector()
  pos2<-vector()
  for (iter in 1: nrow(From_To)){
    
    pos1[iter]<-which(Original_Undirected[,1]==From_To[iter,1] & Original_Undirected[,2]==From_To[iter,2])
                      
    pos2[iter] <-which(Original_Undirected[,1]==From_To[iter,2] & Original_Undirected[,2]==From_To[iter,1])
   
                  
  }
  Original_Undirected<-Original_Undirected[-c(pos1,pos2),]
  
  print(iter)
  return(Original_Undirected)
  
}