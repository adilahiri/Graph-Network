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
     return(result$statistic)
   }
  
  else if(dim(Evidence)[2]==1){
    M<-length(X)
    Sum_ChiSquare <-0
    Values <-c(0,1)
    Z<-Evidence
    for (iter1 in 1:2){ #Z
      for(iter2 in 1:2){#X
        for(iter3 in 1:2){#Y
          M_XYZ <- length(which(X==Values[iter2] & Y==Values[iter3] & Z== Values[iter1]))
          P_Z <- length(which(Z==Values[iter1]))/M
          P_X_Z <- (length(which(Z==Values[iter1] & X==Values[iter2]))/M)/P_Z
          P_Y_Z <- (length(which(Z==Values[iter1] & Y==Values[iter3]))/M)/P_Z
          Product <- M * P_Z * P_X_Z * P_Y_Z
        
          
          if(Product != 0 & !is.nan(Product)){
            Chi_Squared<- ((M_XYZ-Product)^2)/Product
            Sum_ChiSquare<-Sum_ChiSquare+Chi_Squared
          }
        }
      }
      
    }
    
    return(Sum_ChiSquare)
    
  }
  

  else if(dim(Evidence)[2]==2){
    
    M<-length(X)
    Sum_ChiSquare <-0
    Values <-c(0,1)
    Z0<-Evidence[,1]
    Z1<-Evidence[,2]
    for(iter0 in 1:2){#Z0
      for (iter1 in 1:2){ #Z1
        for(iter2 in 1:2){#X
          for(iter3 in 1:2){#Y
            M_XYZ <- length(which(X==Values[iter2] & Y==Values[iter3] & Z1== Values[iter1] & Z0== Values[iter0]))
            P_Z <- length(which( Z1==Values[iter1] & Z0==Values[iter0] ))/M
            P_X_Z <- (length(which(Z1==Values[iter1] & Z0==Values[iter0] & X==Values[iter2]))/M)/P_Z
            P_Y_Z <- (length(which(Z1==Values[iter1] & Z0==Values[iter0] & Y==Values[iter3]))/M)/P_Z
            Product <- M * P_Z * P_X_Z * P_Y_Z
            print(Product)
  
          if(Product != 0 & !is.nan(Product) ){
            Chi_Squared<- ((M_XYZ-Product)^2)/Product
            Sum_ChiSquare<-Sum_ChiSquare+Chi_Squared
          }
        }
      }
      
    }
    }
    return(Sum_ChiSquare)
    
  }
  
}