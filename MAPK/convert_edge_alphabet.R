convert_edge_alphabet<-function(From_To_Table){
  
  From_To_Table_Alpha<- matrix(nrow = nrow(From_To_Table),ncol = ncol(From_To_Table))
  From_To_Table_Alpha<-as.data.frame(From_To_Table_Alpha)
  for(iter in 1:nrow(From_To_Table)){
    From_To_Table_Alpha[iter,1]<-LETTERS[From_To_Table[iter,1]]
    From_To_Table_Alpha[iter,2]<-LETTERS[From_To_Table[iter,2]]
  }
  return(From_To_Table_Alpha)
}