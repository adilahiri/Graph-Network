Indicate_V_Struct <- function(Parent1,Parent2,Witness,LRT){
  VStruct <-0 
  pos <- which((LRT[,1]==Parent1 & LRT[,2]==Parent2)| (LRT[,1]==Parent2 & LRT[,2]==Parent1)  )
  pos <- pos[1]
  Vstruct <-LRT[pos,4]
}