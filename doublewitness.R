double_witness<- function(Data_Table,row_number,POS_IND,Mutual_Threshold){
  Nodes<- POS_IND[row_number,]
  possible_witness<-c(1:7)
  possible_witness<-possible_witness[-c(Nodes)]
  possible_witness_pairs<-t(combn(possible_witness, 2))
  
  Flag<- FALSE # WITNESS OR NOT
  iter<-1
  while(!Flag)
  {
    Evidence_Pair<-as.data.frame(Data_Table[,possible_witness_pairs[iter,]])
    Condi_En <- natstobits(condinformation_2(Data_Table[,Nodes[1]],Data_Table[,Nodes[2]],
                                           Evidence_Pair,method = "emp"))
    if(Condi_En < Mutual_Threshold)
    {
      Flag <- TRUE
      
    }
    if(Flag==TRUE){
      return (possible_witness_pairs[iter,])
    }
    iter<-iter+1
    if(iter> dim(possible_witness_pairs)[1]){Flag <-TRUE}
  }
  return(0)
  
  
  
}