calc_shape_param<-function(alpha_init,beta_init,Node_Data,Parents){
  
    if (is.null(Parents)){
      alpha1 <- alpha_init+ length(which(Node_Data==1))
      beta1 <- beta_init + length(which(Node_Data==0))
      return(cbind(alpha1,beta1))
    }
    else if(dim(Parents)[2] == 1 ){
      alpha1 <-alpha_init + length(which(Parents==1 & Node_Data ==1) )
      beta1 <-beta_init + length(which(Parents==1 & Node_Data ==0) )
      
      alpha2<-alpha_init + length(which(Parents==0 & Node_Data ==1) )
      beta2 <-beta_init + length(which(Parents==0 & Node_Data ==0) )
      
      Param <- matrix(c(alpha1,alpha2,beta1,beta2),nrow=2,ncol=2)
      return (Param)
      
    }
   
  else if(dim(Parents)[2] == 2 ){
    alpha1 <-alpha_init + length(which(Parents[,1]==1 & Parents[,2]==1 & Node_Data ==1) )
    beta1 <-beta_init + length(which(Parents[,1]==1 & Parents[,2]==1 & Node_Data ==0) )
    
    alpha2<-alpha_init + length(which(Parents[,1]==0 & Parents[,2]==1 & Node_Data ==1) )
    beta2 <-beta_init + length(which(Parents[,1]==0 & Parents[,2]==1 & Node_Data ==0) )
    
    alpha3<-alpha_init + length(which(Parents[,1]==1 & Parents[,2]==0 & Node_Data ==1) )
    beta3 <-beta_init + length(which(Parents[,1]==1 & Parents[,2]==0 & Node_Data ==0) )
    
    alpha4<-alpha_init + length(which(Parents[,1]==0 & Parents[,2]==0 & Node_Data ==1) )
    beta4 <-beta_init + length(which(Parents[,1]==0 & Parents[,2]==0 & Node_Data ==0) )
    
    Param <- matrix(c(alpha1,alpha2,alpha3,alpha4,beta1,beta2,beta3,beta4),nrow=4,ncol=2)
    return (Param)
    
  }
  
  
  
  else if(dim(Parents)[2] == 3 ){
    alpha1 <-alpha_init + length(which(Parents[,1]==1 & Parents[,2]==1 & Parents[,3]==1 
                                       & Node_Data ==1) )
    beta1 <-beta_init + length(which(Parents[,1]==1 & Parents[,2]==1 & Parents[,3]==1
                                       & Node_Data ==0) )
    
    
    alpha2 <-alpha_init + length(which(Parents[,1]==1 & Parents[,2]==1 & Parents[,3]==0 
                                       & Node_Data ==1) )
    beta2 <-beta_init + length(which(Parents[,1]==1 & Parents[,2]==1 & Parents[,3]==0 
                                     & Node_Data ==0) )
    
    
    alpha3 <-alpha_init + length(which(Parents[,1]==1 & Parents[,2]==0 & Parents[,3]==0 
                                       & Node_Data ==1) )
    beta3 <-beta_init + length(which(Parents[,1]==1 & Parents[,2]==0 & Parents[,3]==0 
                                     & Node_Data ==0) )
    
    
    alpha4 <-alpha_init + length(which(Parents[,1]==0 & Parents[,2]==1 & Parents[,3]==0 
                                       & Node_Data ==1) )
    beta4 <-beta_init + length(which(Parents[,1]==0 & Parents[,2]==1 & Parents[,3]==0 
                                     & Node_Data ==0) )
    
    
    alpha5 <-alpha_init + length(which(Parents[,1]==0 & Parents[,2]==0 & Parents[,3]==1 
                                       & Node_Data ==1) )
    beta5 <-beta_init + length(which(Parents[,1]==0 & Parents[,2]==0 & Parents[,3]==1 
                                     & Node_Data ==0) )
    
    
    alpha6 <-alpha_init + length(which(Parents[,1]==0 & Parents[,2]==1 & Parents[,3]==1 
                                       & Node_Data ==1) )
    beta6 <-beta_init + length(which(Parents[,1]==0 & Parents[,2]==1 & Parents[,3]==1 
                                     & Node_Data ==0) )
    
    
    alpha7 <-alpha_init + length(which(Parents[,1]==1 & Parents[,2]==0 & Parents[,3]==1
                                     & Node_Data ==1) )
    beta7 <-beta_init + length(which(Parents[,1]==1 & Parents[,2]==0 & Parents[,3]==1 
                                     & Node_Data ==0) )
    
    
    alpha8 <-alpha_init + length(which(Parents[,1]==0 & Parents[,2]==0 & Parents[,3]==0 
                                     & Node_Data ==1) )
    beta8 <-beta_init + length(which(Parents[,1]==0 & Parents[,2]==0 & Parents[,3]==0
                                     & Node_Data ==0) )
    
    
    
    Param <- matrix(c(alpha1,alpha2,alpha3,alpha4,alpha5, alpha6,alpha7,alpha8,
                      beta1,beta2,beta3,beta4,beta5,beta6,beta7,beta8),nrow=8,ncol=2)
    return (Param)
    
  }

  
  
}
  