MLE_Estimates <- function (observation, Evidence=NULL,set=NULL){
  
  if (is.null(Evidence)){
    Count1 <- length(which(observation ==1)) 
    Count0 <- length(which(observation ==0))
    L1 <- Count1/(Count1+Count0)
  }

  else if (is.null(dim(Evidence)[2])){
    
    Count1 <- length (which(observation==1 & Evidence==set ))
    Count0 <- length (which(observation==0 & Evidence==set ))
    
    L1 <- Count1/(Count1+Count0)
  }
  
  else if(dim(Evidence)[2]==2){
    
    Count1 <- length (which(observation==1 & Evidence[,1]==set[1] & Evidence[,2]==set[2] ))
    Count0 <- length (which(observation==0 & Evidence[,1]==set[1]  & Evidence[,2]==set[2]))
    L1 <- Count1/(Count1+Count0)
  }
    
    
  
  else if(dim(Evidence)[2]==3){
    
    Count1 <- length (which(observation==1 & Evidence[,1]==set[1] & Evidence[,2]==set[2] & Evidence[,3]==set[3] ))
    Count0 <- length (which(observation==0 & Evidence[,1]==set[1]  & Evidence[,2]==set[2]& Evidence[,3]== set[3]))
    L1 <- Count1/(Count1+Count0)
  }
  else
    L1<- NA
  
  return(L1)
  
}