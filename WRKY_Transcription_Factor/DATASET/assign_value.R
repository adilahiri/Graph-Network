assign_value<-function(value){
  if (value ==1){
    pro<- sample(c(0.6,0.7,0.8,0.9,1),1)
    value<-sample(0:1, 1, replace=T,prob=c(1-pro,pro))
  }
  else if (value ==0) {
    pro<- sample(c(0.6,0.7,0.8,0.9,1),1)
    value<-sample(0:1, 1, replace=T,prob=c(pro,1-pro))
  } 
  return(value)
    
}