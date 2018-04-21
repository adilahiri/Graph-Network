setwd("C:/Users/adi44/Desktop/ATM/Spring2018/ECEN689/FinalProject/MAPK")
compute_chisquare<- function(X,Y, Evidence=NULL){
  
   if(is.null(Evidence)){
     X0Y0 <- length(which(X==0 & Y==0))
     X0Y1 <-length(which(X==0 & Y==1))
     X1Y0 <-length(which(X==1 & Y==0))
     X1Y1 <-length(which(X==1 & Y==1))
     
     t<-as.table(rbind(c(X0Y0,X0Y1),c(X1Y0,X1Y1)))
     result<-chisq.test(t,correct=FALSE)
     print(result)
     return(result$p.value)
   }
  
  else if(dim(Evidence)[2]==1){
    
    
    
  }
  

  else if(dim(Evidence)[2]==0){
    
    
    
  }
  
}