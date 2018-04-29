double_Witness <- function(data_set,Parent1,Parent2,Threshold){
  
  Candidates<- c(1:9)
  Candidates<- Candidates[-which(Candidates==Parent1)]
  Candidates<- Candidates[-which(Candidates==Parent2)]
  
  Evidence_Combinations<-combn(Candidates,2,simplify = FALSE)
  
  
  FLAG_INDEP <- FALSE
  iter <-1
  
  Witness<-vector() # No witness
  Indep<-1 #dependent
  
  while(!FLAG_INDEP & (iter <= length(Evidence_Combinations)) ){
    Evidence_Var <- cbind(cbind(data_set[,Evidence_Combinations[[iter]][1]],cbind(data_set[,Evidence_Combinations[[iter]][2]])))
                                
    X_Square <- compute_chisquare(cbind(data_set[,Parent1]),cbind(data_set[,Parent2]),Evidence_Var)
    if(X_Square < Threshold){ #Changed
      FLAG_INDEP<-TRUE
      Witness<-Evidence_Combinations[[iter]] 
      Indep<-0
    }
    iter <- iter + 1
  }
  
  L <- list(Indep,Witness)
  return(L)
  
}


