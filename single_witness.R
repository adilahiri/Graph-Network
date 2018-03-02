single_witness<- function(Data_Table,row_number,POS_IND,Mutual_Threshold) {
  
  Nodes<- POS_IND[row_number,]
  possible_witness<-c(1:7)
  possible_witness<-possible_witness[-c(Nodes)]
  Flag<- FALSE # WITNESS OR NOT
  iter<-1
  while(!Flag)
  {
    Condi_En <- natstobits(condinformation(Data_Table[,Nodes[1]],Data_Table[,Nodes[2]],
                                Data_Table[,possible_witness[iter]],method = "emp"))
    if(Condi_En < Mutual_Threshold)
    {
      Flag <- TRUE
      
    }
    if(Flag==TRUE){
     return (possible_witness[iter])
    }
    iter<-iter+1
    if(iter> length(possible_witness)){Flag <-TRUE}
  }
  return(0)
}