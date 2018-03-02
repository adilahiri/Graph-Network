get_skeleton<- function(Data_Table,POS_IND,Graph_Construct,list_indep,Mutual_Threshold)
{
  for (iter in 1:length(list_indep)){
    if(0 %in% list_indep[[iter]]){
       list_indep[[iter]]<- single_witness(Data_Table,iter,POS_IND,Mutual_Threshold)    # compute the witness of independence 
       if(0 %in% list_indep[[iter]]){
         list_indep[[iter]]<- double_witness(Data_Table,iter,POS_IND,Mutual_Threshold)    # compute the witness of independence  
       }     
    }
    
  }
  
  for (iter in 1: length(list_indep)){
      if(!is.na(list_indep[[iter]]) & !(0%in% list_indep[[iter]])){
        row1=POS_IND[iter,1]
        col1=POS_IND[iter,2]
        Graph_Construct[row1,col1] <- 0
        Graph_Construct[col1,row1]<-0
       }
  }
  
  
  
  
  
  return(list(Graph_Construct,list_indep))
  #return(list_indep)
}