ROC_AUC<- function(thresh, data_vector,true_label)
{ 
  x <- rep(0,length(data_vector)) 
  y <- rep(0,length(data_vector))
  result_vector<-data_vector
  TPR <-  rep(0, length(thresh))
  FPR <-  rep(0, length(thresh))
  for (iter in 1: length(thresh)){
    pos_1 =which(abs(data_vector)>thresh[iter])
    pos_0= which(abs(data_vector)<thresh[iter])
    result_vector[pos_1]=1
    result_vector[pos_0]=0
    arr=spec_sens(result_vector,true_label)
    TPR[iter]= arr[1]/(arr[1]+arr[4])
    FPR[iter]=1-(arr[2]/(arr[2]+arr[3]))
    
  }

 L=cbind(TPR,FPR)
 return(L)
}