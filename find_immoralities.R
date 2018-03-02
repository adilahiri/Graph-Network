find_immoralities <- function (List_Adj){
  List_Imm=list()
  count <-1
  for (iter in 1 : length(List_Adj)){
    if(length(List_Adj[[iter]]) > 1 )
      { #change to >0 and & !is.na(List_Adj[[iter]])
      ##MAKE THE LISt
       M1<-t(combn(List_Adj[[iter]], 2))
       M1<-cbind(M1,cbind(rep(iter,dim(M1)[1])))
       M1<-M1[,c(1,3,2)]
       List_Imm[[count]]<-M1
       count<-count+1
    }
    
  }
  return(List_Imm)
}